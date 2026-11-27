;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.prometheus-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.prometheus :as prometheus]
            [gumshoe.shell :as shell]
            [gumshoe.storage :as storage]))

(def pods
  {:items [{:metadata {:name "prometheus-main-0" :namespace "monitoring"}
            :spec {:volumes [{:name "prometheus-data"
                              :persistentVolumeClaim {:claimName "data-prometheus-main-0"}}]}}]})

(def pvcs
  {:items [{:metadata {:name "data-prometheus-main-0" :namespace "monitoring"}
            :spec {:storageClassName "expandable"
                   :resources {:requests {:storage "50Gi"}}}}]})

(def storage-classes
  {:items [{:metadata {:name "expandable"} :allowVolumeExpansion true}
           {:metadata {:name "fixed"} :allowVolumeExpansion false}]})

(deftest resize-plan-test
  (let [plan (prometheus/resize-plan {:pods pods
                                      :volume-name "prometheus-data"
                                      :pvcs pvcs
                                      :storage-classes storage-classes
                                      :capacity "100Gi"})]
    (is (= [{:pod "prometheus-main-0"
             :pvc "data-prometheus-main-0"
             :exists? true
             :storage-class "expandable"
             :resizable? true
             :current "50Gi"
             :target "100Gi"}]
           plan))
    (testing "a growing plan on expandable storage has no problems"
      (is (empty? (storage/expansion-problems plan))))))

(deftest an-unnamed-claim-template-uses-the-operators-volume-name
  (is (= "prometheus-k8s-db"
         (prometheus/volume-name-of {:kind "Prometheus" :metadata {:name "k8s"}
                                     :spec {:storage {:volumeClaimTemplate {:spec {}}}}})))
  (is (= "prom-agent-edge-db"
         (prometheus/volume-name-of {:kind "PrometheusAgent" :metadata {:name "edge"}
                                     :spec {:storage {:volumeClaimTemplate {:spec {}}}}})))
  (is (= "data"
         (prometheus/volume-name-of {:kind "Prometheus" :metadata {:name "k8s"}
                                     :spec {:storage {:volumeClaimTemplate {:metadata {:name "data"}}}}}))))

(deftest the-defaults-find-the-operators-governing-service
  (let [operated {:metadata {:name "prometheus-operated" :namespace "monitoring"
                             :labels {:operated-prometheus "true"}}
                  :spec {:ports [{:name "web" :port 9090}]}}]
    (is (= "operated-prometheus=true" prometheus/governing-service-selector))
    (is (= 9090 (prometheus/api-port operated prometheus/default-port-name)))
    (is (nil? (prometheus/api-port operated "http"))
        "a port name the service lacks finds nothing, so the book refuses")))

;; --- the resize as a plan ----------------------------------------------------------

(def resize-plan
  [{:pvc "db-k8s-0" :current "10Gi" :target "20Gi"}
   {:pvc "db-k8s-1" :current "10Gi" :target "20Gi"}])

(def steps (prometheus/resize-steps "ctx" "monitoring" "k8s" "20Gi" resize-plan))

(defn- patch-of
  [effect]
  (json/parse-string (subs (last effect) (count "--patch=")) true))

(deftest the-prometheus-is-paused-resized-orphan-deleted-and-unpaused-in-that-order
  (is (= [:pause :claims :claims :delete :unpause] (mapv :stage steps)))
  (is (every? effect/effect? (storage/plan-of steps)) "every step carries exactly one effect")
  (let [[pause _ _ delete unpause] (effect/collect (storage/plan-of steps))]
    (is (= [:kubectl "ctx" "--namespace=monitoring" "patch" prometheus/prometheus-type "k8s" "--type=merge"]
           (vec (butlast pause))))
    (is (= {:spec {:paused true :storage {:volumeClaimTemplate {:spec {:resources {:requests {:storage "20Gi"}}}}}}}
           (patch-of pause))
        "the pause also asks for the new capacity in the claim template")
    (is (= [:kubectl "ctx" "--namespace=monitoring" "delete" "statefulset"
            "--selector=operator.prometheus.io/name=k8s" "--ignore-not-found" "--interactive=false" "--cascade=orphan"]
           delete))
    (is (= {:spec {:paused false}} (patch-of unpause))))
  (is (= (storage/resize-claim-effects "ctx" "monitoring" resize-plan "20Gi")
         (subvec (storage/plan-of steps) 1 3))
      "the claims are patched by the shared claim steps"))

(defn- recovery
  [applied]
  (prometheus/resize-recovery {:context "ctx" :target "monitoring/k8s"} steps applied))

(deftest no-recovery-when-the-pause-did-not-apply
  (is (nil? (recovery 0))))

(deftest a-stop-among-the-claims-says-the-prometheus-is-paused
  (testing "at the first claim, after the pause"
    (let [text (recovery 1)]
      (is (str/starts-with? text "Prometheus monitoring/k8s is PAUSED"))
      (is (str/includes? text "Not enlarged:\n  db-k8s-0\n  db-k8s-1"))
      (is (not (str/includes? text "cannot be undone")) "no claim was enlarged yet")))
  (testing "at the second claim"
    (let [text (recovery 2)]
      (is (str/includes? text "cannot be undone:\n  db-k8s-0\nNot enlarged:\n  db-k8s-1"))
      (is (str/includes? text "why db-k8s-1 could not be patched")))))

(deftest a-stop-at-the-delete-says-the-statefulset-keeps-the-old-template
  (let [text (recovery 3)]
    (is (str/includes? text "PAUSED"))
    (is (str/includes? text "its StatefulSet could not be deleted"))
    (is (str/includes? text "cannot be undone:\n  db-k8s-0\n  db-k8s-1"))))

(deftest a-stop-at-the-unpause-hands-over-the-command
  (let [text (recovery 4)]
    (is (str/starts-with? text "THE STATEFULSET OF PROMETHEUS monitoring/k8s IS DELETED"))
    (is (str/includes? text (str "  " (prometheus/unpause-args "ctx" "monitoring" "k8s"))))
    (is (= "kubectl --context=ctx --namespace=monitoring patch prometheuses.monitoring.coreos.com k8s --type=merge --patch='{\"spec\":{\"paused\":false}}'"
           (prometheus/unpause-args "ctx" "monitoring" "k8s"))
        "the command patches what the unpause step patches")))

(deftest a-run-that-applied-everything-says-nothing-was-rolled-back
  (is (str/starts-with? (recovery 5) "Every effect ran: Prometheus monitoring/k8s was unpaused")))

(deftest the-pods-check-needs-pods-and-every-one-running
  (let [[capacity _ pods-check] (prometheus/resize-checks "ctx" "cluster" "monitoring" "k8s" "20Gi" resize-plan)
        owner [{:kind "StatefulSet" :uid "uid-sts" :controller true}]
        answer (fn [& phases]
                 (fn [& argv]
                   {:exit 0 :err ""
                    :out (json/generate-string
                          (if (some #{"statefulsets"} argv)
                            {:items [{:metadata {:uid "uid-sts"}}]}
                            {:items (for [phase phases]
                                      {:metadata {:namespace "monitoring" :ownerReferences owner}
                                       :status {:phase phase}})}))}))]
    (is (= ["all 2 PVCs report capacity 20Gi" 300 15 true] ((juxt :description :timeout :interval :soft?) capacity)))
    (is (= ["every Prometheus pod is Running again under its StatefulSet" 300 15] ((juxt :description :timeout :interval) pods-check)))
    (with-redefs [shell/execute (answer "Running" "Running")] (is (true? ((:check pods-check)))))
    (with-redefs [shell/execute (answer "Running" "Pending")] (is (false? ((:check pods-check)))))
    (with-redefs [shell/execute (answer)] (is (false? ((:check pods-check))) "no pods is not running"))))

(deftest the-pods-are-read-by-the-operator-labels-in-the-namespace-only
  (let [seen (atom nil)]
    (with-redefs [shell/execute (fn [& argv]
                                  (reset! seen (vec argv))
                                  {:exit 0 :err ""
                                   :out (json/generate-string
                                         {:items [{:metadata {:namespace "monitoring" :name "a"}}
                                                  {:metadata {:namespace "other" :name "b"}}]})})]
      (is (= ["a"] (map #(get-in % [:metadata :name]) (:items (prometheus/pods-of "ctx" "monitoring" "k8s"))))))
    (is (some #{"--selector=app.kubernetes.io/name=prometheus,app.kubernetes.io/instance=k8s"} @seen))))
