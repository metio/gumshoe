;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.firebook-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.evpn :as evpn-detectives]
            [gumshoe.detectives.pods :as pods]
            [gumshoe.effect :as effect]
            [gumshoe.evpn :as evpn]
            [gumshoe.firebook :as firebook]
            [gumshoe.verify :as verify]))

(deftest manifest-test
  (testing "every drill resource lives in the drill namespace"
    (is (= firebook/drill-namespace
           (-> (firebook/deployment-manifest {:name "crash-loop" :image "img"})
               :metadata :namespace)))
    (is (= firebook/drill-namespace
           (-> (firebook/pvc-manifest {:name "pvc" :storage-class "none"})
               :metadata :namespace))))
  (testing "a command is only set when given"
    (is (= ["sh" "-c" "exit 1"]
           (-> (firebook/deployment-manifest {:name "x" :image "img" :command ["sh" "-c" "exit 1"]})
               :spec :template :spec :containers first :command)))
    (is (nil? (-> (firebook/deployment-manifest {:name "x" :image "img"})
                  :spec :template :spec :containers first :command))))
  (testing "the pvc requests the broken storage class"
    (is (= "none"
           (-> (firebook/pvc-manifest {:name "pvc" :storage-class "none"})
               :spec :storageClassName)))))

(deftest fabric-drill-values-cannot-collide-with-a-real-fabric
  (testing "the MAC derives from RFC 5737 documentation space, so no guest can hold it"
    (is (= "02:00:cb:00:71:01" firebook/drill-mac))
    (is (str/starts-with? firebook/drill-mac "02:00:") "still an OpenNebula-shaped guest MAC"))
  (testing "the destination is not a VTEP anywhere, so nothing is attracted to it"
    (is (= "192.0.2.1" firebook/drill-vtep))))

(deftest fabric-drill-command-assembly
  (testing "replace, so relighting a burning drill is not an error"
    (is (= ["sudo" "bridge" "fdb" "replace" "02:00:cb:00:71:01" "dev" "lo.4208"
            "dst" "192.0.2.1" "self" "static"]
           (firebook/fabric-pin-args 4208))))
  (testing "putting it out names the same device and MAC"
    (is (= ["sudo" "bridge" "fdb" "del" "02:00:cb:00:71:01" "dev" "lo.4208"]
           (firebook/fabric-unpin-args 4208))))
  (testing "the drill entry is what the fabric detective reports"
    (let [row (str firebook/drill-mac " dev lo.4208 dst " firebook/drill-vtep " self static\n")
          rows (vec (evpn/parse-fdb 4208 "host-a" row))
          findings (evpn-detectives/detect-static-fdb {"evpn-fdb" rows})]
      (is (= 1 (count findings)))
      (is (= :warning (:severity (first findings))))
      (is (str/includes? (:summary (first findings)) firebook/drill-vtep)))))

;; --- the detectives prove the fire --------------------------------------------

(defn- pod
  [namespace reason]
  {:metadata {:namespace namespace :name "crash-loop-7d9f8-abcde"
              :ownerReferences [{:kind "ReplicaSet" :name "crash-loop-7d9f8"}]}
   :status {:phase "Running"
            :containerStatuses [(if reason
                                  {:name "crash-loop" :restartCount 4
                                   :state {:waiting {:reason reason}}}
                                  {:name "crash-loop" :restartCount 0
                                   :state {:running {:startedAt "2026-01-01T00:00:00Z"}}})]}})

(def ^:private crash-loop-expect
  (:expect (firebook/drill-spec
            {:description "d" :drill "crash-loop" :fire "f"
             :manifests [(firebook/deployment-manifest {:name "crash-loop" :image "img"})]
             :expect {:detectives pods/detectives
                      :fire? (fn [{:keys [summary]} _] (str/includes? summary "CrashLoopBackOff"))}})))

(defn- fire-in
  [& pods]
  (vec (firebook/fire-findings crash-loop-expect {"pods" {:items (vec pods)} :now (java.time.Instant/now)} {})))

(deftest a-kubernetes-fire-is-what-its-detective-reports-in-the-drill-namespace
  (testing "a crash-looping drill pod is the fire"
    (is (= ["fire-drill/crash-loop-7d9f8-abcde"] (map :component (fire-in (pod "fire-drill" "CrashLoopBackOff"))))))
  (testing "a healthy drill pod is not"
    (is (empty? (fire-in (pod "fire-drill" nil)))))
  (testing "a real crash loop elsewhere on the cluster never stands in for the drill"
    (is (empty? (fire-in (pod "shop" "CrashLoopBackOff"))))))

(deftest a-detective-that-throws-is-not-a-fire-that-is-out
  (let [expect {:detectives [{:name "broken" :requires ["pods"] :detect (fn [_] (throw (ex-info "boom" {})))}]
                :fire? (constantly true)}]
    (is (thrown? clojure.lang.ExceptionInfo (doall (firebook/fire-findings expect {"pods" {:items []}} {}))))
    (testing "so the out check does not hold"
      (is (false? (binding [*err* (java.io.StringWriter.)]
                    (verify/eventually (assoc (firebook/out-check (assoc expect :collect (constantly {"pods" {:items []}})) {})
                                              :timeout 0))))))))

(deftest a-check-over-evidence-its-detective-read-nothing-from-waits-for-an-answer
  (let [expect {:detectives pods/detectives
                :collect (constantly {"pods" nil})
                :fire? (constantly true)}
        out (java.io.StringWriter.)
        held (binding [*out* out *err* out]
               (verify/eventually (assoc (firebook/out-check expect {}) :timeout 0)))]
    (is (false? held))
    (is (str/includes? (str out) "the pods, stuck-pods detectives find nothing to read"))))

(deftest fabric-evidence-from-a-host-that-did-not-answer-is-unread
  (with-redefs [evpn/collect-host (fn [_ vni] {:host "hv1" :vni vni :error "host is not reachable over ssh"})]
    (let [e (try (firebook/fabric-evidence {:host "hv1"} 4208) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e))
      (is (:gumshoe.kubectl/unread (ex-data e)))
      (is (= "hv1: host is not reachable over ssh" (ex-message e)))))
  (with-redefs [evpn/collect-host (fn [_ vni] {:host "hv1" :vni vni :fdb [{:mac "m"}] :macs []})]
    (is (= [{:mac "m"}] (get (firebook/fabric-evidence {:host "hv1"} 4208) "evpn-fdb")))))

;; --- the spec ------------------------------------------------------------------

(def ^:private kubernetes-spec
  {:description "d" :drill "crash-loop" :fire "a crash-looping deployment"
   :manifests [(firebook/deployment-manifest {:name "crash-loop" :image "img"})]
   :prerequisites {:kubectl-can-create ["deployments"]}
   :expect {:detectives pods/detectives :fire? (constantly true)}})

(deftest a-kubernetes-drill-plans-both-changes
  (let [{:keys [ignite extinguish]} (firebook/drill-spec kubernetes-spec)
        ctx {:context "ctx"}
        [[op context stdin & args]] (effect/collect (effect/as-plan ((:effect ignite) ctx)))]
    (testing "the ignite applies the namespace and the manifests as one list"
      (is (= [:kubectl-stdin "ctx" ["apply" "--filename" "-"]] [op context (vec args)]))
      (is (= ["Namespace" "Deployment"] (map :kind (:items (json/parse-string stdin true))))))
    (testing "the extinguish deletes the drill namespace"
      (is (= [[:kubectl "ctx" "delete" "namespace" "fire-drill" "--ignore-not-found"]]
             (effect/collect (effect/as-plan ((:effect extinguish) ctx))))))))

(deftest a-kubernetes-drill-derives-what-it-needs
  (let [{:keys [options prerequisites]} (firebook/drill-spec kubernetes-spec)]
    (is (= {:extinguish {:desc "Put out the fire: delete the fire-drill namespace" :alias :e :coerce :boolean}} options))
    (is (= ["namespaces" "deployments"] (:kubectl-can-create prerequisites)))
    (is (= ["namespaces" "pods"] (:kubectl-can-get prerequisites)) "read access to what its detectives read")))

(deftest a-firebook-leaves-extinguish-to-the-firebook
  (is (thrown? clojure.lang.ExceptionInfo
               (firebook/drill-spec (assoc kubernetes-spec :options {:extinguish {:desc "x"}}))))
  (is (thrown? clojure.lang.ExceptionInfo
               (firebook/drill-spec (assoc kubernetes-spec :options {:everything {:desc "x" :alias :e}})))))
