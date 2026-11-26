;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.prometheus-delete-every-pod-test
  "prometheus/delete_metric deletes from every pod of the chosen Prometheus
   behind the service, each through a forward to that pod, reports per pod,
   and verifies only when every pod deleted the metric; set_capacity plans
   against the claims of the Prometheus' own namespace."
  (:require [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.http :as http]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.outcome :as outcome]
            [gumshoe.prometheus :as prometheus]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.verify :as verify]))

(def ^:private service
  {:metadata {:namespace "monitoring" :name "prometheus-operated"}
   :spec {:selector {:app.kubernetes.io/name "prometheus"}
          :ports [{:name "web" :port 9090 :targetPort "web"}]}})

(defn- pod
  [name & {:keys [instance ready phase port-name] :or {instance "k8s" ready "True" phase "Running" port-name "web"}}]
  {:metadata {:namespace "monitoring" :name name
              :labels {:app.kubernetes.io/name "prometheus" :app.kubernetes.io/instance instance}}
   :spec {:containers [{:name "prometheus" :ports [{:name port-name :containerPort 9090}]}]}
   :status {:phase phase :conditions [{:type "Ready" :status ready}]}})

(deftest a-service-selects-the-pods-carrying-its-selector
  (is (prometheus/selected-by? service (pod "p-0")))
  (is (not (prometheus/selected-by? service (assoc-in (pod "p-0") [:metadata :labels] {:app "x"}))))
  (is (not (prometheus/selected-by? (update service :spec dissoc :selector) (pod "p-0")))
      "a service without a selector selects nothing"))

(deftest the-pod-port-follows-the-service-target-port
  (is (= 9090 (prometheus/pod-api-port service "web" (pod "p-0"))) "a named target port")
  (is (= 9091 (prometheus/pod-api-port (assoc-in service [:spec :ports 0 :targetPort] 9091) "web" (pod "p-0"))))
  (is (= 9090 (prometheus/pod-api-port (update-in service [:spec :ports 0] dissoc :targetPort) "web" (pod "p-0"))))
  (is (nil? (prometheus/pod-api-port service "web" (pod "p-0" :port-name "other"))))
  (is (nil? (prometheus/pod-api-port service "http" (pod "p-0")))))

(deftest every-replica-and-shard-is-a-target
  (is (= {:targets [{:pod "prometheus-k8s-0" :port 9090} {:pod "prometheus-k8s-1" :port 9090}
                    {:pod "prometheus-k8s-shard-1-0" :port 9090}]}
         (prometheus/delete-targets {:service service :port-name "web" :prometheus "monitoring/k8s" :instance "k8s"
                                     :pods {:items [(pod "prometheus-k8s-1") (pod "prometheus-k8s-shard-1-0")
                                                    (pod "prometheus-k8s-0") (pod "prometheus-other-0" :instance "other")]}}))
      "a pod of another Prometheus in the namespace is not a target"))

(deftest a-deletion-that-could-not-reach-every-pod-is-refused
  (let [targets #(prometheus/delete-targets {:service service :port-name "web" :prometheus "monitoring/k8s" :instance "k8s" :pods %})]
    (is (str/includes? (:refusal (targets nil)) "could not read the pods"))
    (is (str/includes? (:refusal (targets {:items []})) "selects no pod"))
    (is (str/includes? (:refusal (targets {:items [(pod "p-0") (pod "p-1" :ready "False")]})) "p-1"))
    (is (str/includes? (:refusal (targets {:items [(pod "p-0" :phase "Pending")]})) "not ready"))
    (is (str/includes? (:refusal (targets {:items [(pod "p-0" :port-name "other")]})) "cannot be resolved"))))

(deftest every-pod-is-tried-and-reported
  (let [tried (atom [])
        {:keys [results ok?]} (prometheus/delete-on-every-pod!
                               [{:pod "p-0"} {:pod "p-1"} {:pod "p-2"}]
                               (fn [{:keys [pod]} record!]
                                 (swap! tried conj pod)
                                 (case pod
                                   "p-0" (do (record! :reached? true) (record! :deleted? true) (record! :confirmed? true) (record! :verified? true) true)
                                   "p-1" false
                                   "p-2" (do (record! :reached? true) (record! :deleted? true) (record! :verified? true) false))))]
    (is (= ["p-0" "p-1" "p-2"] @tried) "a failed pod does not stop the others")
    (is (false? ok?))
    (is (= [{:pod "p-0" :reached? true :deleted? true :verified? true :confirmed? true}
            {:pod "p-1" :reached? false :deleted? false :verified? false :confirmed? false}
            {:pod "p-2" :reached? true :deleted? true :verified? false :confirmed? false}]
           results)
        "a pod whose forward failed after the delete keeps its delete but is not verified"))
  (is (true? (:ok? (prometheus/delete-on-every-pod! [{:pod "p-0"}] (fn [_ record!] (record! :deleted? true) (record! :verified? true) true)))))
  (is (false? (:ok? (prometheus/delete-on-every-pod! [] (fn [_ _] true)))) "no pod is no deletion"))

(deftest a-forward-to-one-pod-names-that-pod
  (let [argv (atom nil)]
    (with-redefs [process/process (fn [_ & args] (reset! argv (vec args)) (throw (ex-info "stub" {})))]
      (binding [*err* (java.io.StringWriter.)]
        (is (false? (kubectl/with-port-forward {:context "c" :namespace "monitoring" :pod "prometheus-k8s-1"
                                                :local-port 39999 :remote-port 9090}
                      (constantly true))))))
    (is (some #{"pod/prometheus-k8s-1"} @argv) (pr-str @argv))
    (is (not-any? #(str/starts-with? (str %) "service/") @argv))))

;; --- the whole book ----------------------------------------------------------------

(defn- answer
  [argv]
  (let [type (second (drop-while #(not= "get" %) argv))
        items (case type
                "services" [service]
                "prometheuses.monitoring.coreos.com" [{:metadata {:namespace "monitoring" :name "k8s"}}]
                "pods" [(pod "prometheus-k8s-0") (pod "prometheus-k8s-1")
                        (pod "prometheus-other-0" :instance "other")]
                [])]
    {:exit 0 :out (json/generate-string {:items items}) :err ""}))

(defn- run-book!
  "Runs delete_metric against two pods of Prometheus monitoring/k8s, with
   pod `failing` refusing the delete. Returns what each forward targeted,
   the run's result and its outcome record."
  [failing & {:keys [unreachable]}]
  (let [{:keys [action]} (binding [runbook/*loading* true] (load-file "tools/prometheus/runbooks/delete_metric.clj"))
        forwards (atom [])
        current (atom nil)
        record (outcome/fresh)
        err (java.io.StringWriter.)
        eventually verify/eventually
        result (with-redefs [kubectl/current-context (constantly "ctx")
                             kubectl/current-cluster (constantly "cluster")
                             shell/execute (fn [& argv] (answer (vec argv)))
                             interact/confirm! (constantly true)
                             kubectl/with-port-forward (fn [{:keys [pod service remote-port]} f]
                                                         (swap! forwards conj [(or pod (str "service/" service)) remote-port])
                                                         (reset! current pod)
                                                         (if (= unreachable pod) false (f)))
                             kubectl/port-forward-alive? (constantly true)
                             http/request (fn [method _ _]
                                            (if (= :post method)
                                              {:reachable true :status (if (= failing @current) 500 204) :body ""}
                                              {:reachable true :status 200 :json {:status "success" :data []}}))
                             verify/eventually (fn [check] (eventually (assoc check :timeout 0 :watch nil)))]
                 (binding [*err* err
                           outcome/*record* record]
                   (action {:metric "up" :namespace "monitoring" :service "prometheus-operated"
                            :selector prometheus/governing-service-selector :port-name "web" :delete-timeout 5}
                           {:announcement-data nil})))]
    {:forwards @forwards :result result :record @record :err (str err)}))

(defn- delete-metric-report-lines
  [results]
  (binding [runbook/*loading* true] (load-file "tools/prometheus/runbooks/delete_metric.clj"))
  ((requiring-resolve 'runbooks.prometheus.delete-metric/report-lines) results))

(deftest the-book-deletes-on-every-pod-of-the-chosen-prometheus
  (let [{:keys [forwards result record]} (run-book! nil)]
    (is (= [["prometheus-k8s-0" 9090] ["prometheus-k8s-1" 9090]] forwards)
        "each pod of monitoring/k8s, none of another Prometheus, none through the service")
    (is (true? result))
    (is (true? (:verified record)))))

(deftest a-pod-that-kept-the-metric-fails-the-run-and-names-it
  (let [{:keys [forwards result record]} (run-book! "prometheus-k8s-0")]
    (is (= 2 (count forwards)) "the other pod is still tried")
    (is (false? result))
    (is (false? (:verified record)))
    (is (str/includes? (get-in record [:recovery :text]) "deleted on 1 of 2"))
    (is (str/includes? (get-in record [:recovery :text]) "prometheus-k8s-0 still hold it"))))

(deftest a-pod-no-forward-reached-is-never-said-to-hold-the-metric
  (let [{:keys [forwards result record err]} (run-book! nil :unreachable "prometheus-k8s-0")
        guidance (get-in record [:recovery :text])]
    (is (= 2 (count forwards)) "the other pod is still tried")
    (is (false? result))
    (is (str/includes? guidance "deleted on 1 of 2"))
    (is (str/includes? guidance "prometheus-k8s-0 could not be reached through a port-forward, so the delete never ran there"))
    (is (not (str/includes? guidance "still hold it")) guidance)
    (is (str/includes? err "prometheus-k8s-0: NOT deleted - no port-forward to it started") err)
    (is (not (str/includes? err "the metric is still there")) err)))

(deftest the-report-says-what-each-pod-did
  (is (= ["  a: NOT deleted - no port-forward to it started (see the error above), so it was not asked"
          "  b: NOT deleted - the metric is still there"
          "  c: deleted, no series left"]
         (vec (delete-metric-report-lines [{:pod "a" :reached? false :deleted? false}
                                           {:pod "b" :reached? true :deleted? false}
                                           {:pod "c" :reached? true :deleted? true :verified? true :confirmed? true}])))))

;; --- set_capacity's claims ---------------------------------------------------------

(deftest the-resize-plan-matches-claims-by-namespace-and-name
  (let [claim (fn [namespace size class]
                {:metadata {:namespace namespace :name "prometheus-k8s-db-prometheus-k8s-0"}
                 :spec {:storageClassName class :resources {:requests {:storage size}}}})
        plan (prometheus/resize-plan
              {:pods {:items [{:metadata {:namespace "mon" :name "prometheus-k8s-0"}
                               :spec {:volumes [{:name "prometheus-k8s-db"
                                                 :persistentVolumeClaim {:claimName "prometheus-k8s-db-prometheus-k8s-0"}}]}}]}
               :volume-name "prometheus-k8s-db"
               :pvcs {:items [(claim "tenant" "10Gi" "fast") (claim "mon" "100Gi" "fixed")]}
               :storage-classes {:items [{:metadata {:name "fast"} :allowVolumeExpansion true}
                                         {:metadata {:name "fixed"} :allowVolumeExpansion false}]}
               :capacity "50Gi"})]
    (is (= [{:pod "prometheus-k8s-0" :pvc "prometheus-k8s-db-prometheus-k8s-0" :exists? true
             :storage-class "fixed" :resizable? false :current "100Gi" :target "50Gi"}]
           plan))))

(deftest set-capacity-reads-claims-in-the-prometheus-namespace
  (binding [runbook/*loading* true] (load-file "tools/prometheus/runbooks/set_capacity.clj"))
  (let [planned (ns-resolve 'runbooks.prometheus.set-capacity 'planned)
        reads (atom [])]
    (with-redefs [shell/execute (fn [& argv]
                                  (swap! reads conj (vec argv))
                                  {:exit 0 :out (json/generate-string {:items []}) :err ""})]
      (binding [*err* (java.io.StringWriter.)]
        (planned "ctx" "cluster" "mon" "k8s" "50Gi")))
    (let [claim-reads (filter #(some #{"persistentvolumeclaims"} %) @reads)]
      (is (seq claim-reads))
      (is (every? #(some #{"--namespace=mon"} %) claim-reads) (pr-str claim-reads))
      (is (not-any? #(some #{"--all-namespaces"} %) claim-reads)))))
