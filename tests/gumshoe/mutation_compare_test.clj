;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-compare-test
  "What a pin compares on an object without metadata.generation: :compare
   :spec compares uid and spec, so the kubelet's status writes to a Node
   never refuse a change, while every type that declares nothing keeps
   comparing resourceVersion."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.change :as change]
            [gumshoe.effect :as effect]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn- node
  "A Node as the API server returns it: no generation, a resourceVersion
   every write raises, and a status the kubelet keeps writing."
  [& {:keys [uid rv spec status labels] :or {uid "uid-n1" rv "100" spec {} status {}}}]
  {:metadata (cond-> {:name "n1" :uid uid :resourceVersion rv}
               labels (assoc :labels labels))
   :spec spec
   :status status})

(def ^:private heartbeat
  {:conditions [{:type "Ready" :status "True" :lastHeartbeatTime "2026-11-25T10:00:00Z"}]})

(def ^:private later-heartbeat
  {:conditions [{:type "Ready" :status "True" :lastHeartbeatTime "2026-11-25T10:00:40Z"}]
   :images [{:names ["docker.io/library/busybox:latest"]}]})

(def ^:private taint
  {:key "node.kubernetes.io/maintenance" :effect "NoSchedule"})

(def ^:private drift-cases
  "[what moved, compare, object at the confirmation, object right before the
   effect, the drift expected]"
  [["nothing" :spec (node) (node) nil]
   ["a status write" :spec (node :status heartbeat) (node :rv "101" :status later-heartbeat) nil]
   ["a label" :spec (node) (node :rv "101" :labels {:role "worker"}) nil]
   ["spec.unschedulable" :spec (node) (node :rv "101" :spec {:unschedulable true})
    "its spec changed after the confirmation: unschedulable"]
   ["spec.taints" :spec (node :spec {:unschedulable true})
    (node :rv "101" :spec {:unschedulable true :taints [taint]})
    "its spec changed after the confirmation: taints"]
   ["two spec fields and a status write" :spec (node :status heartbeat)
    (node :rv "101" :spec {:unschedulable true :taints [taint]} :status later-heartbeat)
    "its spec changed after the confirmation: taints, unschedulable"]
   ["a cleared field" :spec (node :spec {:unschedulable true}) (node :rv "101" :spec {})
    "its spec changed after the confirmation: unschedulable"]
   ["the uid, spec unchanged" :spec (node) (node :uid "uid-n1-again")
    "it was deleted and created again after the confirmation"]
   ["the object is gone" :spec (node) nil "it was deleted after the confirmation"]
   ["a status write, nothing declared" nil (node :status heartbeat) (node :rv "101" :status later-heartbeat)
    "it changed after the confirmation"]
   ["a status write, :resource-version declared" :resource-version (node) (node :rv "101" :status later-heartbeat)
    "it changed after the confirmation"]])

(deftest a-pin-compares-what-its-subject-declares
  (doseq [[what compare before after expected] drift-cases]
    (testing what
      (is (= expected (mutation/drift (mutation/fingerprint before nil compare)
                                      (mutation/fingerprint after nil compare)))))))

(deftest a-generation-less-type-that-declares-nothing-compares-resource-version
  (let [config-map (fn [rv data] {:metadata {:name "c" :uid "uid-c" :resourceVersion rv} :data data})]
    (is (= {:uid "uid-c" :resource-version "7"} (mutation/fingerprint (config-map "7" {:a "1"}))))
    (is (= "it changed after the confirmation"
           (mutation/drift (mutation/fingerprint (config-map "7" {:a "1"}))
                           (mutation/fingerprint (config-map "8" {:a "2"})))))
    (is (= "it changed after the confirmation"
           (mutation/drift (mutation/fingerprint (config-map "7" {:a "1"}) nil :resource-version)
                           (mutation/fingerprint (config-map "8" {:a "1"}) nil :resource-version))))))

(deftest a-generation-wins-over-compare
  (let [deployment (fn [generation rv spec] {:metadata {:uid "u" :generation generation :resourceVersion rv} :spec spec})]
    (is (= {:uid "u" :generation 3} (mutation/fingerprint (deployment 3 "10" {:replicas 1}) nil :spec)))
    (is (nil? (mutation/drift (mutation/fingerprint (deployment 3 "10" {:replicas 1}) nil :spec)
                              (mutation/fingerprint (deployment 3 "11" {:replicas 1}) nil :spec)))
        "a status write leaves the generation alone")
    (is (= "its spec changed after the confirmation (generation 3, now 4)"
           (mutation/drift (mutation/fingerprint (deployment 3 "10" {:replicas 1}) nil :spec)
                           (mutation/fingerprint (deployment 4 "11" {:replicas 2}) nil :spec))))))

(deftest a-spec-fingerprint-keeps-its-facts
  (let [facts #(get-in % [:metadata :labels :role])]
    (is (= {:uid "uid-n1" :spec {} :facts "worker"}
           (mutation/fingerprint (node :labels {:role "worker"}) facts :spec)))
    (is (= "what the change rests on moved after the confirmation (\"worker\", now \"infra\")"
           (mutation/drift (mutation/fingerprint (node :labels {:role "worker"}) facts :spec)
                           (mutation/fingerprint (node :rv "101" :labels {:role "infra"}) facts :spec))))))

(deftest a-node-without-a-spec-compares-an-empty-one
  (let [bare {:metadata {:uid "u" :resourceVersion "1"}}]
    (is (= {:uid "u" :spec {}} (mutation/fingerprint bare nil :spec)))
    (is (nil? (mutation/drift (mutation/fingerprint bare nil :spec)
                              (mutation/fingerprint (assoc-in bare [:metadata :resourceVersion] "2") nil :spec))))))

(deftest an-object-that-gained-a-generation-is-refused
  (doseq [spec [{} {:unschedulable true}]]
    (is (= "its spec changed after the confirmation"
           (mutation/drift (mutation/fingerprint (node :spec spec) nil :spec)
                           (mutation/fingerprint (assoc-in (node :spec spec) [:metadata :generation] 1) nil :spec)))
        "what is compared changed shape, so the change fails closed without naming keys that did not move")))

(deftest ref-fingerprint-reads-facts-and-compare-from-the-ref
  (is (= {:uid "uid-n1" :spec {:unschedulable true}}
         (mutation/ref-fingerprint {:type "nodes" :name "n1" :compare :spec} (node :spec {:unschedulable true}))))
  (is (= {:uid "uid-n1" :resource-version "100"}
         (mutation/ref-fingerprint {:type "nodes" :name "n1"} (node)))))

(deftest an-unknown-compare-is-refused
  (testing "a misspelt :compare never falls back to another comparison"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"a pin compares :status - write :compare :resource-version or :spec"
                          (mutation/fingerprint (node) nil :status)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"a pin compares :status"
                          (mutation/fingerprint nil nil :status))
        "even for an object that is gone"))
  (testing "normalize refuses it on a :subject and on a canonical :pick entry"
    (doseq [spec [{:subject {:type "nodes" :compare :status}}
                  {:subjects [{:from :pick :type "nodes" :compare "spec"}]}]]
      (let [e (try (change/normalize spec) nil (catch clojure.lang.ExceptionInfo e e))]
        (is (some? e) (pr-str spec))
        (is (change/invalid? e))
        (is (re-find #"a pin compares" (ex-message e))))))
  (testing "a known one passes normalize unchanged"
    (is (= [{:from :pick :type "nodes" :compare :spec}]
           (:subjects (change/normalize {:subject {:type "nodes" :compare :spec}}))))))

;; --- the engine ------------------------------------------------------------------------

(defn- run-node-change
  "run-on! of spec on node n1, whose object reads as at-confirmation until
   the operator confirmed and as before-effect after; what it printed on
   stdout and stderr, and the plans it ran."
  [spec at-confirmation before-effect]
  (let [confirmed? (atom false) executed (atom [])]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  interact/confirm! (fn [& _] (reset! confirmed? true))
                  effect/run! (fn [e] (swap! executed conj e) true)
                  mutation/fetch-pinned (fn [_ _] (if @confirmed? before-effect at-confirmation))]
      (let [out (with-out-str
                  (binding [*err* *out*]
                    (mutation/run-on! (merge {:select {:mode :one :label "Node" :flag :node}
                                              :confirm {:action "cordon a node" :destructive? false}
                                              :effect (fn [_] (effect/plan (effect/note "cordon")))}
                                             spec)
                                      "n1" {} nil)))]
        {:out out :executed @executed}))))

(deftest the-engine-pins-a-subject-by-what-it-declares
  (doseq [[what spec at before ran? re]
          [["a :subject with :compare :spec, status write" {:subject {:type "nodes" :compare :spec}}
            (node :status heartbeat) (node :rv "101" :status later-heartbeat) true nil]
           ["a :subject with :compare :spec, taint added" {:subject {:type "nodes" :compare :spec}}
            (node) (node :rv "101" :spec {:taints [taint]}) false
            #"refusing: its spec changed after the confirmation: taints"]
           ["a :subject declaring nothing, status write" {:subject {:type "nodes"}}
            (node :status heartbeat) (node :rv "101" :status later-heartbeat) false
            #"refusing: it changed after the confirmation"]
           ["a :pin ref with :compare :spec, status write"
            {:pin (constantly {:type "nodes" :name "n1" :compare :spec})}
            (node) (node :rv "101" :status later-heartbeat) true nil]
           ["a :pin ref with :compare :spec, uid moved"
            {:pin (constantly {:type "nodes" :name "n1" :compare :spec})}
            (node) (node :uid "uid-n1-again") false
            #"refusing: it was deleted and created again after the confirmation"]
           ["a :subjects ref with :compare :spec, status write"
            {:subjects (constantly [{:type "nodes" :name "n1" :compare :spec}])}
            (node) (node :rv "101" :status later-heartbeat) true nil]
           ["a :subjects ref with :compare :spec, cordoned meanwhile"
            {:subjects (constantly [{:type "nodes" :name "n1" :compare :spec}])}
            (node) (node :rv "101" :spec {:unschedulable true}) false
            #"refusing: nodes n1: its spec changed after the confirmation: unschedulable"]]]
    (testing what
      (let [{:keys [out executed]} (run-node-change spec at before)]
        (if ran?
          (is (= [[[:note "cordon"]]] executed) out)
          (do (is (empty? executed) out)
              (is (re-find re out) out)))))))
