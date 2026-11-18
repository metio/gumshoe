;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.workloads
  "The decisions behind the remediation runbooks for workload controllers,
   pure over the objects so every refusal and every completion test is
   testable. A pick names one workload as \"<Kind> <namespace>/<name>\"."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(def types
  "The workload controllers, by resource type and kind."
  [["deployments" "Deployment"] ["statefulsets" "StatefulSet"] ["daemonsets" "DaemonSet"]])

(def ^:private type-of-kind (into {} (map (fn [[type kind]] [kind type])) types))

(defn type-of [kind] (type-of-kind kind))

(defn pick
  [kind workload]
  (str kind " " (kubectl/namespace-name-of workload)))

(defn parse-pick
  "\"<Kind> <namespace>/<name>\" as {:kind :type :namespace :name}, or nil for
   another shape or an unknown kind."
  [value]
  (when-let [[_ kind namespace name] (re-matches #"\s*(\w+)\s+([^/\s]+)/(\S+)\s*" (str value))]
    (when-let [type (type-of kind)]
      {:kind kind :type type :namespace namespace :name name})))

(defn picks
  "Every workload of the listed kinds that pred accepts, as picks."
  [lists-by-kind pred]
  (vec (for [[kind list] lists-by-kind
             workload (sort-by kubectl/namespace-name-of (kubectl/items-of list))
             :when (pred kind workload)]
         (pick kind workload))))

(defn desired-replicas
  "How many pods the workload wants: spec.replicas (1 when unset) for a
   Deployment or StatefulSet, the scheduled count for a DaemonSet."
  [kind workload]
  (if (= "DaemonSet" kind)
    (or (-> workload :status :desiredNumberScheduled) 0)
    (or (-> workload :spec :replicas) 1)))

(defn rolled-out?
  "Whether the controller has observed the newest spec and every pod runs it
   and is available."
  [kind workload]
  (let [{:keys [metadata spec status]} workload
        observed? (>= (or (:observedGeneration status) 0) (or (:generation metadata) 0))
        wanted (desired-replicas kind workload)]
    (boolean
     (and workload observed?
          (case kind
            "Deployment" (and (= wanted (or (:updatedReplicas status) 0))
                              (= wanted (or (:availableReplicas status) 0))
                              (= wanted (or (:replicas status) 0)))
            "StatefulSet" (and (= wanted (or (:updatedReplicas status) 0))
                               (= wanted (or (:readyReplicas status) 0))
                               (or (nil? (:updateRevision status))
                                   (= (:currentRevision status) (:updateRevision status))
                                   (= "OnDelete" (-> spec :updateStrategy :type))))
            "DaemonSet" (and (= wanted (or (:updatedNumberScheduled status) 0))
                             (= wanted (or (:numberAvailable status) 0)))
            false)))))

;; --- rollout restart -----------------------------------------------------------

(defn restart-refusal
  "Why restarting the workload would do nothing useful, or nil."
  [kind workload]
  (cond
    (nil? workload) "the workload no longer exists"
    (and (= "Deployment" kind) (true? (-> workload :spec :paused)))
    "the Deployment is paused, so a restart waits until it is resumed - resume it first"
    (and (= "StatefulSet" kind) (= "OnDelete" (-> workload :spec :updateStrategy :type)))
    "the StatefulSet updates OnDelete, so a restart replaces no pod until each is deleted by hand"
    (zero? (desired-replicas kind workload)) "it runs no pods, so there is nothing to restart"
    :else nil))

(defn restart-args
  [{:keys [type namespace name]}]
  [(str "--namespace=" namespace) "rollout" "restart" (str (str/replace type #"s$" "") "/" name)])

;; --- scale ---------------------------------------------------------------------

(def scalable-kinds #{"Deployment" "StatefulSet"})

(defn managing-hpa
  "The HorizontalPodAutoscaler that targets the workload, or nil."
  [hpas kind workload]
  (first (filter #(and (= (kubectl/namespace-of %) (kubectl/namespace-of workload))
                       (= kind (-> % :spec :scaleTargetRef :kind))
                       (= (kubectl/name-of workload) (-> % :spec :scaleTargetRef :name)))
                 (kubectl/items-of hpas))))

(defn scale-refusal
  "Why scaling the workload to replicas would be wrong or pointless, or nil."
  [kind workload hpas replicas]
  (let [hpa (when workload (managing-hpa hpas kind workload))]
    (cond
      (not (and (integer? replicas) (<= 0 replicas))) "--replicas must be a count of zero or more"
      (not (scalable-kinds kind)) (format "a %s is not scaled by a replica count" kind)
      (nil? workload) "the workload no longer exists"
      hpa (format "HorizontalPodAutoscaler %s manages its replicas (%d to %d) and would undo this - change the HPA instead"
                  (kubectl/name-of hpa) (or (-> hpa :spec :minReplicas) 1) (or (-> hpa :spec :maxReplicas) 0))
      (= replicas (desired-replicas kind workload)) (format "it runs %d replica(s) already" replicas)
      :else nil)))

(defn scale-items
  "The confirmation lines for a scale: the change, and what scaling a
   StatefulSet down leaves behind."
  [kind workload replicas]
  (let [from (desired-replicas kind workload)]
    (cond-> [(format "scale %s %s from %d to %d replica(s)" kind (kubectl/namespace-name-of workload) from replicas)]
      (zero? replicas) (conj "no pod serves it afterwards")
      (and (= "StatefulSet" kind) (< replicas from))
      (conj (format "the pods with the highest ordinals go first; their PersistentVolumeClaims stay")))))

(defn scaled?
  "Whether the workload runs exactly replicas pods and all of them are ready."
  [workload wanted]
  (let [status (:status workload)]
    (boolean (and workload
                  (>= (or (:observedGeneration status) 0) (or (-> workload :metadata :generation) 0))
                  (= wanted (or (:replicas status) 0))
                  (= wanted (or (:readyReplicas status) 0))))))

(defn scale-args
  [{:keys [type namespace name]} replicas]
  [(str "--namespace=" namespace) "scale" (str (str/replace type #"s$" "") "/" name) (str "--replicas=" replicas)])

;; --- rollback ------------------------------------------------------------------

(def ^:private revision-annotation :deployment.kubernetes.io/revision)

(defn- revision-of
  [object]
  (some-> object :metadata :annotations revision-annotation str parse-long))

(defn- images-of
  [template]
  (mapv :image (-> template :spec :containers)))

(defn revisions
  "The revisions of a Deployment, newest first, read from the ReplicaSets it
   owns: {:revision :replicaset :images}."
  [deployment replicasets]
  (->> (kubectl/items-of replicasets)
       (filter (fn [rs]
                 (and (= (kubectl/namespace-of rs) (kubectl/namespace-of deployment))
                      (some #(and (= "Deployment" (:kind %))
                                  (if-let [uid (-> deployment :metadata :uid)]
                                    (= uid (:uid %))
                                    (= (kubectl/name-of deployment) (:name %))))
                            (-> rs :metadata :ownerReferences)))))
       (keep (fn [rs] (when-let [revision (revision-of rs)]
                        {:revision revision :replicaset (kubectl/name-of rs) :images (images-of (-> rs :spec :template))})))
       (sort-by (comp - :revision))
       vec))

(defn current-revision
  [deployment]
  (revision-of deployment))

(defn rollback-target
  "The revision a rollback goes to: to-revision when given, else the newest
   one older than the current revision."
  [deployment replicasets to-revision]
  (let [current (current-revision deployment)
        all (revisions deployment replicasets)]
    (if to-revision
      (first (filter #(= to-revision (:revision %)) all))
      (first (filter #(and current (< (:revision %) current)) all)))))

(defn rollback-refusal
  [deployment replicasets to-revision]
  (let [current (when deployment (current-revision deployment))]
    (cond
      (nil? deployment) "the Deployment no longer exists"
      (true? (-> deployment :spec :paused)) "the Deployment is paused - resume it first"
      (and to-revision (= to-revision current)) (format "revision %d is the current one" to-revision)
      (nil? (rollback-target deployment replicasets to-revision))
      (if to-revision
        (format "revision %d is not among the Deployment's ReplicaSets" to-revision)
        "the Deployment has no earlier revision to go back to")
      :else nil)))

(defn rollback-items
  [deployment replicasets to-revision]
  (let [target (rollback-target deployment replicasets to-revision)]
    [(format "roll Deployment %s back from revision %s (%s) to revision %d (%s)"
             (kubectl/namespace-name-of deployment)
             (or (current-revision deployment) "?")
             (str/join ", " (images-of (-> deployment :spec :template)))
             (:revision target)
             (str/join ", " (:images target)))
     "the rollback becomes a new revision; the Deployment's source (Git, a chart) still holds the newer one and will reapply it"]))

(defn rolled-back?
  "Whether the Deployment runs the target revision's images and has rolled out."
  [deployment target]
  (boolean (and deployment target
                (= (:images target) (images-of (-> deployment :spec :template)))
                (rolled-out? "Deployment" deployment))))

(defn rollback-args
  [{:keys [namespace name]} revision]
  [(str "--namespace=" namespace) "rollout" "undo" (str "deployment/" name) (str "--to-revision=" revision)])
