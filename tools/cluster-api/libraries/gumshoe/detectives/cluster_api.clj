;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.cluster-api
  "Detectives for Cluster API (cluster.x-k8s.io, v1beta2 conditions): workload
   Clusters that are not Available or that the management cluster cannot
   reach, Machines that failed, never finish provisioning or never finish
   deleting, and MachineHealthChecks whose remediation is blocked.

   Cluster API 1.11 and later serve v1beta2, whose status.conditions hold the
   v1beta2 condition types. Before it the API serves only v1beta1, whose
   status.conditions hold the older types and whose status.v1beta2.conditions
   hold the v1beta2 ones (from Cluster API 1.9). A v1beta1 object is judged
   on the latter, and one that carries neither is reported as unread, since
   its conditions cannot be interpreted."
  (:require [clojure.string :as str]
            [gumshoe.conditions :as conditions]
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

(def ^:private v1beta2-markers
  "Per kind, the condition type only the v1beta2 condition set carries, so
   its presence in status.conditions says the list holds v1beta2 types."
  {"Cluster" "Available"
   "Machine" "Available"
   "MachineHealthCheck" "RemediationAllowed"})

(defn v1beta2-view
  "The object with its v1beta2 conditions in status.conditions, where the
   detectives read them, or nil when it carries none. An object whose
   apiVersion is not cluster.x-k8s.io/v1beta1 is v1beta2 and returned as it
   is. A v1beta1 object is read from status.v1beta2.conditions; failing that,
   its status.conditions count only when they carry the kind's v1beta2
   marker type."
  [kind object]
  (let [v1beta1? (= "cluster.x-k8s.io/v1beta1" (:apiVersion object))
        nested (-> object :status :v1beta2 :conditions)
        marker (v1beta2-markers kind)]
    (cond
      (not v1beta1?) object
      (seq nested) (assoc-in object [:status :conditions] nested)
      (some #(= marker (:type %)) (-> object :status :conditions)) object
      :else nil)))

(defn- unread-conditions-finding
  [kind object]
  {:severity :warning
   :reports-unread true
   :component (kubectl/namespace-name-of object)
   :summary (format "the conditions of this %s were not read: it is served as v1beta1 without v1beta2 conditions" kind)
   :hint (str "neither status.conditions nor status.v1beta2.conditions carries Cluster API's v1beta2 condition types"
              " (Cluster API 1.8 or older), so its availability and health are not judged - read them with kubectl describe")})

(defn- judged
  "The findings of judge over the object's v1beta2 view, or, for an object
   without v1beta2 conditions, the unread finding plus what judge concludes
   with no conditions at all - its phase, pause and deletion still read."
  [kind judge object now]
  (if-let [view (v1beta2-view kind object)]
    (judge view now)
    (cons (unread-conditions-finding kind object)
          (judge (assoc-in object [:status :conditions] []) now))))

(defn- explain
  [condition]
  (str/join " - " (remove nil? [(:reason condition) (conditions/first-line (:message condition))])))

(defn cluster-findings
  [cluster now]
  (let [by-type (conditions/by-type cluster)
        available (get by-type "Available")
        cause (conditions/first-false cluster cluster-causes)
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
           (some-> (conditions/minutes-since (:lastTransitionTime available) now) (>= settle-minutes)))
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
  (let [by-type (conditions/by-type machine)
        {:keys [phase deletion]} (:status machine)
        component (kubectl/namespace-name-of machine)
        age (conditions/minutes-since (-> machine :metadata :creationTimestamp) now)
        deleting-since (-> machine :metadata :deletionTimestamp)
        deleting (conditions/minutes-since deleting-since now)
        cause (conditions/first-false machine machine-causes)
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
  (let [allowed (get (conditions/by-type healthcheck) "RemediationAllowed")]
    (when (= "False" (:status allowed))
      (onset/since
       {:severity :critical
        :component (kubectl/namespace-name-of healthcheck)
        :summary (format "MachineHealthCheck may not remediate (%s)" (or (:reason allowed) "no reason"))
        :hint (str (some-> (conditions/first-line (:message allowed)) (str " - "))
                   "unhealthy Machines stay as they are until fewer are unhealthy than its unhealthyLessThanOrEqualTo limit")}
       (:lastTransitionTime allowed)))))

(defn detect-clusters
  [evidence]
  (mapcat #(judged "Cluster" cluster-findings % (:now evidence)) (kubectl/items-of (get evidence cluster-type))))

(defn detect-machines
  [evidence]
  (mapcat #(judged "Machine" machine-findings % (:now evidence)) (kubectl/items-of (get evidence machine-type))))

(defn detect-healthchecks
  [evidence]
  (mapcat #(judged "MachineHealthCheck" (fn [object _] (remove nil? [(healthcheck-finding object)])) % (:now evidence))
          (kubectl/items-of (get evidence healthcheck-type))))

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
