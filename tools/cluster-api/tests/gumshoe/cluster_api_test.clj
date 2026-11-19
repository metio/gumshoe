;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cluster-api-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.cluster-api :as capi]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.cluster-api]))

(def ^:private now (java.time.Instant/parse "2026-11-19T12:00:00Z"))

(defn- condition [type status & {:keys [reason message at] :or {at "2026-11-19T11:00:00Z"}}]
  (cond-> {:type type :status status :lastTransitionTime at}
    reason (assoc :reason reason)
    message (assoc :message message)))

(defn- object [name & {:keys [conditions phase spec created deleting deletion]
                       :or {created "2026-11-19T10:00:00Z"}}]
  {:metadata (cond-> {:namespace "fleet" :name name :creationTimestamp created}
               deleting (assoc :deletionTimestamp deleting))
   :spec (or spec {})
   :status (cond-> {:conditions (vec conditions)} phase (assoc :phase phase) deletion (assoc :deletion deletion))})

;; --- clusters ------------------------------------------------------------------

(deftest an-unavailable-cluster-names-its-cause
  (let [[finding] (capi/cluster-findings
                   (object "prod" :conditions [(condition "Available" "False" :reason "NotAvailable")
                                               (condition "InfrastructureReady" "True")
                                               (condition "RemoteConnectionProbe" "False" :reason "ProbeFailed"
                                                          :message "Remote connection probe failed, probe last succeeded at 2026-11-19T10:59:00Z\ndetails")])
                   now)]
    (is (= :critical (:severity finding)))
    (is (= "fleet/prod" (:component finding)))
    (is (= "Cluster is not Available (RemoteConnectionProbe)" (:summary finding)))
    (is (str/starts-with? (:hint finding) "ProbeFailed - Remote connection probe failed, probe last succeeded at 2026-11-19T10:59:00Z - the management cluster cannot reach"))
    (is (= "2026-11-19T11:00:00Z" (:since finding))))
  (testing "each cause has its own hint"
    (doseq [[type phrase] [["InfrastructureReady" "infrastructure provider"] ["ControlPlaneAvailable" "control plane provider"]
                           ["WorkersAvailable" "worker Machines"]]]
      (is (str/includes? (:hint (first (capi/cluster-findings (object "c" :conditions [(condition "Available" "False") (condition type "False")]) now)))
                         phrase)
          type)))
  (testing "without a named cause"
    (is (= "Cluster is not Available (NotAvailable)"
           (:summary (first (capi/cluster-findings (object "c" :conditions [(condition "Available" "False" :reason "NotAvailable")]) now)))))))

(deftest cluster-states-that-are-not-failures
  (is (empty? (capi/cluster-findings (object "c" :conditions [(condition "Available" "False" :at "2026-11-19T11:55:00Z")]) now)) "settling")
  (is (empty? (capi/cluster-findings (object "c" :conditions [(condition "Available" "True")]) now)))
  (is (empty? (capi/cluster-findings (object "c" :deleting "2026-11-19T11:00:00Z" :conditions [(condition "Available" "False")]) now)))
  (is (= [[:info "Cluster is paused"]]
         (map (juxt :severity :summary) (capi/cluster-findings (object "c" :spec {:paused true} :conditions [(condition "Available" "False")]) now))))
  (is (= "Cluster is paused" (:summary (first (capi/cluster-findings (object "c" :conditions [(condition "Paused" "True")]) now))))))

(deftest a-failed-cluster
  (is (= [[:critical "Cluster is Failed" "InfrastructureReady - network broken"]]
         (map (juxt :severity :summary :hint)
              (capi/cluster-findings (object "c" :phase "Failed" :conditions [(condition "InfrastructureReady" "False" :reason "InfrastructureReady" :message "network broken")]) now)))))

;; --- machines ------------------------------------------------------------------

(deftest machines-stuck-provisioning
  (let [[finding] (capi/machine-findings
                   (object "prod-md-0-abc" :phase "Provisioning"
                           :conditions [(condition "BootstrapConfigReady" "True")
                                        (condition "InfrastructureReady" "False" :reason "NotReady" :message "OpenStackMachine status.initialization.provisioned is false")])
                   now)]
    (is (= :warning (:severity finding)))
    (is (= "Machine is still Provisioning after 120 minutes" (:summary finding)))
    (is (str/starts-with? (:hint finding) "NotReady - OpenStackMachine status.initialization.provisioned is false - the infrastructure provider has not created the server"))
    (is (= "2026-11-19T10:00:00Z" (:since finding))))
  (is (str/includes? (:hint (first (capi/machine-findings (object "m" :phase "Pending" :conditions [(condition "BootstrapConfigReady" "False")]) now)))
                     "bootstrap data"))
  (is (str/includes? (:hint (first (capi/machine-findings (object "m" :phase "Provisioned" :conditions [(condition "NodeHealthy" "False")]) now)))
                     "node is not healthy"))
  (is (empty? (capi/machine-findings (object "m" :phase "Provisioning" :created "2026-11-19T11:45:00Z") now)) "a young Machine")
  (is (empty? (capi/machine-findings (object "m" :phase "Running") now))))

(deftest failed-machines
  (is (= [[:critical "Machine is Failed"]]
         (map (juxt :severity :summary) (capi/machine-findings (object "m" :phase "Failed") now)))))

(deftest machines-that-never-finish-deleting
  (let [[finding] (capi/machine-findings (object "m" :phase "Deleting" :deleting "2026-11-19T11:00:00Z"
                                                 :deletion {:nodeDrainStartTime "2026-11-19T11:01:00Z"}
                                                 :conditions [(condition "Deleting" "True" :reason "DrainingNode" :message "Drain not completed yet")])
                                         now)]
    (is (= "Machine has been deleting for 60 minutes" (:summary finding)))
    (is (str/starts-with? (:hint finding) "DrainingNode - Drain not completed yet - draining its node does not finish"))
    (is (= "2026-11-19T11:00:00Z" (:since finding))))
  (is (str/includes? (:hint (first (capi/machine-findings (object "m" :deleting "2026-11-19T11:00:00Z"
                                                                  :deletion {:nodeDrainStartTime "2026-11-19T11:01:00Z"
                                                                             :waitForNodeVolumeDetachStartTime "2026-11-19T11:20:00Z"}) now)))
                     "volumes to detach"))
  (is (str/includes? (:hint (first (capi/machine-findings (object "m" :deleting "2026-11-19T11:00:00Z") now))) "finalizer"))
  (is (empty? (capi/machine-findings (object "m" :deleting "2026-11-19T11:45:00Z") now)))
  (testing "a deleting Machine is not also judged as provisioning or unhealthy"
    (is (= ["Machine has been deleting for 60 minutes"]
           (map :summary (capi/machine-findings (object "m" :phase "Provisioning" :deleting "2026-11-19T11:00:00Z"
                                                        :conditions [(condition "HealthCheckSucceeded" "False")]) now))))))

(deftest machines-failing-their-health-check
  (let [[finding] (capi/machine-findings (object "m" :phase "Running"
                                                 :conditions [(condition "HealthCheckSucceeded" "False" :reason "UnhealthyNode" :message "Condition Ready on Node is reporting status Unknown for more than 5m0s")])
                                         now)]
    (is (= :warning (:severity finding)))
    (is (= "Machine fails its MachineHealthCheck" (:summary finding)))
    (is (str/starts-with? (:hint finding) "UnhealthyNode - Condition Ready on Node"))))

;; --- health checks ------------------------------------------------------------------

(deftest blocked-remediation
  (let [[finding] (capi/detect-healthchecks
                   {capi/healthcheck-type {:items [(object "workers" :conditions [(condition "RemediationAllowed" "False" :reason "TooManyUnhealthy"
                                                                                             :message "Remediation is not allowed, the number of not started or unhealthy machines exceeds maxUnhealthy (total: 3, unhealthy: 2, maxUnhealthy: 40%)")])
                                                   (object "cp" :conditions [(condition "RemediationAllowed" "True")])]}})]
    (is (= :critical (:severity finding)))
    (is (= "MachineHealthCheck may not remediate (TooManyUnhealthy)" (:summary finding)))
    (is (str/includes? (:hint finding) "unhealthyLessThanOrEqualTo"))))

(deftest the-package-joins-the-platform-scope
  (is (every? (set (map :name (registry/for-scope :platform))) (map :name capi/detectives)))
  (is (empty? (detective/run-detectives capi/detectives {:now now})) "a cluster without Cluster API"))
