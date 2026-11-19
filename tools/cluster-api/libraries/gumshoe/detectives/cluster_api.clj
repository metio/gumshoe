;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.cluster-api
  "Detectives for Cluster API (cluster.x-k8s.io, v1beta2 conditions): workload
   Clusters that are not Available or that the management cluster cannot
   reach, Machines that failed, never finish provisioning or never finish
   deleting, and MachineHealthChecks whose remediation is blocked."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def cluster-type "clusters.cluster.x-k8s.io")
(def machine-type "machines.cluster.x-k8s.io")
(def healthcheck-type "machinehealthchecks.cluster.x-k8s.io")

(def ^:private settle-minutes 10)
(def ^:private provisioning-minutes 30)
(def ^:private deleting-minutes 30)

(def cluster-causes
  "The Cluster conditions that explain why it is not Available, in the order
   they are worth reading."
  ["RemoteConnectionProbe" "InfrastructureReady" "ControlPlaneAvailable" "WorkersAvailable"])

(def machine-causes
  ["BootstrapConfigReady" "InfrastructureReady" "NodeHealthy"])

(defn- conditions
  [object]
  (into {} (map (juxt :type identity)) (-> object :status :conditions)))

(defn- minutes-since
  [value now]
  (when-let [at (try (some-> value java.time.Instant/parse) (catch Exception _ nil))]
    (when now (.toMinutes (java.time.Duration/between at ^java.time.Instant now)))))

(defn- first-false
  [by-type types]
  (first (keep #(let [c (get by-type %)] (when (= "False" (:status c)) c)) types)))

(defn- first-line
  [text]
  (some-> text str str/split-lines first str/trim not-empty))

(defn- explain
  [condition]
  (str/join " - " (remove nil? [(:reason condition) (first-line (:message condition))])))

(defn cluster-findings
  [cluster now]
  (let [by-type (conditions cluster)
        available (get by-type "Available")
        cause (first-false by-type cluster-causes)
        component (kubectl/namespace-name-of cluster)]
    (cond
      (or (true? (-> cluster :spec :paused)) (= "True" (:status (get by-type "Paused"))))
      [{:severity :info
        :component component
        :summary "Cluster is paused"
        :hint "Cluster API reconciles none of its objects until spec.paused is cleared"}]

      (-> cluster :metadata :deletionTimestamp) nil

      (= "Failed" (-> cluster :status :phase))
      [{:severity :critical
        :component component
        :summary "Cluster is Failed"
        :hint (or (some-> cause explain not-empty) "its conditions say which part failed")}]

      (and (= "False" (:status available))
           (some-> (minutes-since (:lastTransitionTime available) now) (>= settle-minutes)))
      [(onset/since
        {:severity :critical
         :component component
         :summary (format "Cluster is not Available (%s)" (or (:type cause) (:reason available) "no reason"))
         :hint (str (some-> (not-empty (explain (or cause available))) (str " - "))
                    (case (:type cause)
                      "RemoteConnectionProbe" "the management cluster cannot reach the workload cluster's API server"
                      "InfrastructureReady" "the infrastructure provider has not finished the cluster's network and load balancer"
                      "ControlPlaneAvailable" "the control plane provider reports the control plane down"
                      "WorkersAvailable" "too few worker Machines are available"
                      "kubectl describe cluster names each condition"))}
        (:lastTransitionTime available))]

      :else nil)))

(defn machine-findings
  [machine now]
  (let [by-type (conditions machine)
        {:keys [phase deletion]} (:status machine)
        component (kubectl/namespace-name-of machine)
        age (minutes-since (-> machine :metadata :creationTimestamp) now)
        deleting-since (-> machine :metadata :deletionTimestamp)
        deleting (minutes-since deleting-since now)
        cause (first-false by-type machine-causes)
        health (get by-type "HealthCheckSucceeded")]
    (remove nil?
            [(when (= "Failed" phase)
               {:severity :critical
                :component component
                :summary "Machine is Failed"
                :hint (str (some-> (not-empty (some-> cause explain)) (str " - "))
                           "a MachineHealthCheck or its owner replaces it; a Machine without either needs a delete")})
             (when (and (contains? #{"Pending" "Provisioning" "Provisioned"} phase) (not deleting-since)
                        age (>= age provisioning-minutes))
               (onset/since
                {:severity :warning
                 :component component
                 :summary (format "Machine is still %s after %d minutes" phase age)
                 :hint (str (some-> (not-empty (some-> cause explain)) (str " - "))
                            (case (:type cause)
                              "BootstrapConfigReady" "its bootstrap data was never generated"
                              "InfrastructureReady" "the infrastructure provider has not created the server"
                              "NodeHealthy" "the server runs but its node is not healthy"
                              "no node has joined for it yet"))}
                (-> machine :metadata :creationTimestamp)))
             (when (and deleting (>= deleting deleting-minutes))
               (onset/since
                {:severity :warning
                 :component component
                 :summary (format "Machine has been deleting for %d minutes" deleting)
                 :hint (str (some-> (get by-type "Deleting") explain not-empty (str " - "))
                            (cond
                              (:waitForNodeVolumeDetachStartTime deletion) "it waits for volumes to detach from its node"
                              (:nodeDrainStartTime deletion) "draining its node does not finish - a PodDisruptionBudget or a pod that ignores eviction holds it"
                              :else "a finalizer holds it"))}
                deleting-since))
             (when (and (= "False" (:status health)) (not deleting-since))
               (onset/since
                {:severity :warning
                 :component component
                 :summary "Machine fails its MachineHealthCheck"
                 :hint (str (some-> (not-empty (explain health)) (str " - "))
                            "it is remediated unless remediation is blocked or it has no owner")}
                (:lastTransitionTime health)))])))

(defn healthcheck-finding
  [healthcheck]
  (let [allowed (get (conditions healthcheck) "RemediationAllowed")]
    (when (= "False" (:status allowed))
      (onset/since
       {:severity :critical
        :component (kubectl/namespace-name-of healthcheck)
        :summary (format "MachineHealthCheck may not remediate (%s)" (or (:reason allowed) "no reason"))
        :hint (str (some-> (first-line (:message allowed)) (str " - "))
                   "unhealthy Machines stay as they are until fewer are unhealthy than its unhealthyLessThanOrEqualTo limit")}
       (:lastTransitionTime allowed)))))

(defn detect-clusters
  [evidence]
  (mapcat #(cluster-findings % (:now evidence)) (kubectl/items-of (get evidence cluster-type))))

(defn detect-machines
  [evidence]
  (mapcat #(machine-findings % (:now evidence)) (kubectl/items-of (get evidence machine-type))))

(defn detect-healthchecks
  [evidence]
  (keep healthcheck-finding (kubectl/items-of (get evidence healthcheck-type))))

(def detectives
  [{:name "capi-clusters"
    :description "Cluster API Clusters that are not Available, Failed or paused"
    :requires [cluster-type]
    :detect detect-clusters}
   {:name "capi-machines"
    :description "Cluster API Machines that failed, never finish provisioning or deleting, or fail their health check"
    :requires [machine-type]
    :detect detect-machines}
   {:name "capi-healthchecks"
    :description "Cluster API MachineHealthChecks whose remediation is blocked"
    :requires [healthcheck-type]
    :detect detect-healthchecks}])
