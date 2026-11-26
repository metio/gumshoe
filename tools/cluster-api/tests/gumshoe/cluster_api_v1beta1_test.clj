;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cluster-api-v1beta1-test
  "Cluster API before 1.11 serves v1beta1 objects, whose v1beta2 conditions
   sit under status.v1beta2.conditions: they are judged there, and an object
   carrying no v1beta2 conditions at all is reported as unread rather than
   passed."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.cluster-api :as capi]))

(def ^:private now (java.time.Instant/parse "2026-11-19T12:00:00Z"))

(defn- condition [type status & {:keys [reason]}]
  (cond-> {:type type :status status :lastTransitionTime "2026-11-19T11:00:00Z"}
    reason (assoc :reason reason)))

(defn- v1beta1 [name & {:keys [conditions nested phase]}]
  {:apiVersion "cluster.x-k8s.io/v1beta1"
   :metadata {:namespace "fleet" :name name :creationTimestamp "2026-11-19T10:00:00Z"}
   :spec {}
   :status (cond-> {:conditions (vec conditions)}
             nested (assoc :v1beta2 {:conditions (vec nested)})
             phase (assoc :phase phase))})

(def ^:private v1beta1-conditions
  "What a CAPI 1.10 Cluster carries in status.conditions: the v1beta1 types."
  [(condition "Ready" "False" :reason "ControlPlaneNotReady")
   (condition "InfrastructureReady" "True")
   (condition "ControlPlaneReady" "False")])

(defn- run [detectives evidence]
  (vec (detective/run-detectives detectives evidence)))

(deftest a-v1beta1-cluster-is-judged-on-its-v1beta2-conditions
  (let [cluster (v1beta1 "prod" :conditions v1beta1-conditions
                         :nested [(condition "Available" "False" :reason "NotAvailable")
                                  (condition "RemoteConnectionProbe" "False" :reason "ProbeFailed")])]
    (is (= [[:critical "Cluster is not Available (RemoteConnectionProbe)"]]
           (map (juxt :severity :summary) (capi/detect-clusters {:now now capi/cluster-type {:items [cluster]}}))))))

(deftest a-v1beta1-cluster-without-v1beta2-conditions-is-unread
  (let [cluster (v1beta1 "prod" :conditions v1beta1-conditions)
        findings (run capi/detectives {:now now capi/cluster-type {:items [cluster]}})]
    (is (= ["the conditions of this Cluster were not read: it is served as v1beta1 without v1beta2 conditions"]
           (map :summary findings)))
    (is (= "fleet/prod" (:component (first findings))))))

(deftest an-unread-cluster-still-reports-its-phase
  (is (= ["the conditions of this Cluster were not read: it is served as v1beta1 without v1beta2 conditions"
          "Cluster is Failed"]
         (map :summary (capi/detect-clusters {:now now capi/cluster-type {:items [(v1beta1 "prod" :conditions v1beta1-conditions
                                                                                           :phase "Failed")]}})))))

(deftest v1beta1-machines-read-the-nested-conditions
  (testing "a failing health check under status.v1beta2"
    (let [machine (v1beta1 "m" :phase "Running"
                           :conditions [(condition "Ready" "True")]
                           :nested [(condition "Available" "True")
                                    (condition "HealthCheckSucceeded" "False" :reason "UnhealthyNode")])]
      (is (= ["Machine fails its MachineHealthCheck"]
             (map :summary (capi/detect-machines {:now now capi/machine-type {:items [machine]}}))))))
  (testing "a v1beta1 Machine without them"
    (is (= ["the conditions of this Machine were not read: it is served as v1beta1 without v1beta2 conditions"]
           (map :summary (capi/detect-machines {:now now capi/machine-type {:items [(v1beta1 "m" :phase "Running"
                                                                                             :conditions [(condition "Ready" "True")])]}}))))))

(deftest v1beta1-healthchecks-read-the-nested-conditions
  (let [mhc (v1beta1 "workers" :nested [(condition "RemediationAllowed" "False" :reason "TooManyUnhealthy")])]
    (is (= ["MachineHealthCheck may not remediate (TooManyUnhealthy)"]
           (map :summary (capi/detect-healthchecks {capi/healthcheck-type {:items [mhc]}}))))))

(deftest v1beta2-objects-are-read-as-they-are
  (let [cluster (assoc (v1beta1 "prod" :conditions [(condition "Available" "True")]) :apiVersion "cluster.x-k8s.io/v1beta2")]
    (is (= cluster (capi/v1beta2-view "Cluster" cluster)))
    (is (empty? (capi/detect-clusters {:now now capi/cluster-type {:items [cluster]}})))))
