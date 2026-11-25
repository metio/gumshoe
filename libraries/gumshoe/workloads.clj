;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.workloads
  "The decisions behind the remediation runbooks for workload controllers,
   pure over the objects so every refusal and every completion test is
   testable. A pick names one workload as \"<Kind> <namespace>/<name>\".
   The few reads that fetch those objects (scalable-picks) are marked as
   reads where they stand."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.pick :as pick]))

(def types
  "The workload controllers, by resource type and kind."
  [["deployments" "Deployment"] ["statefulsets" "StatefulSet"] ["daemonsets" "DaemonSet"]])

(def ^:private type-of-kind (into {} (map (fn [[type kind]] [kind type])) types))

(defn type-of [kind] (type-of-kind kind))

(defn pick
  [kind workload]
  (str kind " " (kubectl/namespace-name-of workload)))

(def parse-pick
  "\"<Kind> <namespace>/<name>\" as {:kind :type :namespace :name}, or nil for
   another shape or an unknown kind."
  (pick/kinded type-of))

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

(defn scalable-picks
  "A read: every Deployment and StatefulSet of context, as picks."
  [context]
  (picks (for [[type kind] types :when (scalable-kinds kind)]
           [kind (kubectl/get-all context type)])
         (constantly true)))

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

(defn scale-refusal-given
  "scale-refusal over a {:ok? :items} read of the namespace's
   HorizontalPodAutoscalers. The gate refuses when an HPA manages the
   workload, so an unanswered read refuses too rather than reading as none."
  [kind workload {:keys [ok? items]} replicas]
  (if ok?
    (scale-refusal kind workload {:items items} replicas)
    "could not read the HorizontalPodAutoscalers in its namespace, so whether one manages its replicas is unknown"))

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
;; A Deployment keeps its revisions in the ReplicaSets it owns, numbered by an
;; annotation that follows the Deployment's own. A StatefulSet or DaemonSet keeps
;; them in ControllerRevisions, whose data holds the pod template; a rollback
;; renumbers the revision it restores to the highest, so the current revision is
;; always the highest one.

(def ^:private revision-annotation :deployment.kubernetes.io/revision)

(defn- revision-of
  [object]
  (some-> object :metadata :annotations revision-annotation str parse-long))

(defn- images-of
  [template]
  (mapv :image (-> template :spec :containers)))

(def history-types
  "Where each kind keeps its revisions."
  {"Deployment" "replicasets" "StatefulSet" "controllerrevisions" "DaemonSet" "controllerrevisions"})

(defn- owned-by?
  [kind workload object]
  (and (= (kubectl/namespace-of object) (kubectl/namespace-of workload))
       (some #(and (= kind (:kind %))
                   (if-let [uid (-> workload :metadata :uid)]
                     (= uid (:uid %))
                     (= (kubectl/name-of workload) (:name %))))
             (-> object :metadata :ownerReferences))))

(defn revisions
  "The revisions of a workload, newest first, read from its history (the
   ReplicaSets of a Deployment, the ControllerRevisions of a StatefulSet or
   DaemonSet): {:revision :source :images}."
  [kind workload history]
  (->> (kubectl/items-of history)
       (filter #(owned-by? kind workload %))
       (keep (fn [object]
               (let [[revision template] (if (= "Deployment" kind)
                                           [(revision-of object) (-> object :spec :template)]
                                           [(:revision object) (-> object :data :spec :template)])]
                 (when revision
                   {:revision revision :source (kubectl/name-of object) :images (images-of template)}))))
       (sort-by (comp - :revision))
       vec))

(defn current-revision
  [kind workload history]
  (if (= "Deployment" kind)
    (revision-of workload)
    (:revision (first (revisions kind workload history)))))

(defn rollback-target
  "The revision a rollback goes to: to-revision when given, else the newest
   one older than the current revision."
  [kind workload history to-revision]
  (let [current (current-revision kind workload history)
        all (revisions kind workload history)]
    (if to-revision
      (first (filter #(= to-revision (:revision %)) all))
      (first (filter #(and current (< (:revision %) current)) all)))))

(defn rollback-refusal
  [kind workload history to-revision]
  (let [current (when workload (current-revision kind workload history))
        strategy (-> workload :spec :updateStrategy)
        partition (or (-> strategy :rollingUpdate :partition) 0)]
    (cond
      (nil? workload) (format "the %s no longer exists" kind)
      (true? (-> workload :spec :paused)) "the Deployment is paused - resume it first"
      (= "OnDelete" (:type strategy))
      (format "the %s updates OnDelete, so a rollback replaces no pod until each is deleted by hand" kind)
      (pos? partition)
      (format "the StatefulSet's rollingUpdate.partition is %d, so pods below it stay on their revision and the rollback never completes" partition)
      (and to-revision (= to-revision current)) (format "revision %d is the current one" to-revision)
      (nil? (rollback-target kind workload history to-revision))
      (if to-revision
        (format "revision %d is not among the %s's %s" to-revision kind
                (if (= "Deployment" kind) "ReplicaSets" "ControllerRevisions"))
        (format "the %s has no earlier revision to go back to" kind))
      :else nil)))

(defn rollback-items
  [kind workload history to-revision]
  (let [target (rollback-target kind workload history to-revision)]
    (cond-> [(format "roll %s %s back from revision %s (%s) to revision %d (%s)"
                     kind (kubectl/namespace-name-of workload)
                     (or (current-revision kind workload history) "?")
                     (str/join ", " (images-of (-> workload :spec :template)))
                     (:revision target)
                     (str/join ", " (:images target)))]
      (= "StatefulSet" kind)
      (conj "a StatefulSet replaces its pods one at a time from the highest ordinal, and restores only the pod template - volumes keep what the newer revision wrote")
      true
      (conj (format "the rollback becomes a new revision; the %s's source (Git, a chart) still holds the newer one and will reapply it" kind)))))

(defn rolled-back?
  "Whether the workload runs the target revision's images and has rolled out."
  [kind workload target]
  (boolean (and workload target
                (= (:images target) (images-of (-> workload :spec :template)))
                (rolled-out? kind workload))))

(defn rollback-args
  [{:keys [type namespace name]} revision]
  [(str "--namespace=" namespace) "rollout" "undo" (str (str/replace type #"s$" "") "/" name) (str "--to-revision=" revision)])

;; --- deployment pause / resume ---------------------------------------------------

(defn paused?
  [deployment]
  (true? (-> deployment :spec :paused)))

(defn pause-refusal
  "Why setting the Deployment's paused to wanted would change nothing, or nil."
  [deployment wanted]
  (cond
    (nil? deployment) "the Deployment no longer exists"
    (= wanted (paused? deployment)) (if wanted "it is paused already" "it is not paused")
    :else nil))

(defn- rollout-progress
  [deployment]
  (format "%d of %d pods" (or (-> deployment :status :updatedReplicas) 0) (desired-replicas "Deployment" deployment)))

(defn pause-items
  [deployment hpa wanted]
  (let [target (kubectl/namespace-name-of deployment)]
    (if wanted
      (cond-> [(format "pause Deployment %s - changes to its pod template are kept but not rolled out until it is resumed" target)]
        (not (rolled-out? "Deployment" deployment))
        (conj (format "its rollout in progress stops where it is (%s updated), old and new pods side by side" (rollout-progress deployment)))
        hpa (conj (format "HorizontalPodAutoscaler %s keeps scaling it - pausing stops rollouts, not scaling" hpa))
        true (conj "rollout restart and rollback refuse a paused Deployment"))
      [(format "resume Deployment %s - every pod template change made while it was paused rolls out now" target)])))

(defn pause-args
  [{:keys [namespace name]} wanted]
  [(str "--namespace=" namespace) "rollout" (if wanted "pause" "resume") (str "deployment/" name)])

(defn pause-applied?
  "Whether the Deployment reached the wanted pause state: paused once the
   controller observed it, resumed once it also rolled out."
  [deployment wanted]
  (boolean (and deployment
                (= wanted (paused? deployment))
                (if wanted
                  (>= (or (-> deployment :status :observedGeneration) 0) (or (-> deployment :metadata :generation) 0))
                  (rolled-out? "Deployment" deployment)))))

;; --- cronjob suspend / resume ----------------------------------------------------

(defn suspended?
  [cronjob]
  (true? (-> cronjob :spec :suspend)))

(defn suspend-refusal
  "Why setting the CronJob's suspend to wanted would change nothing, or nil."
  [cronjob wanted]
  (cond
    (nil? cronjob) "the CronJob no longer exists"
    (= wanted (suspended? cronjob)) (if wanted "it is suspended already" "it is not suspended")
    :else nil))

(defn suspend-items
  [cronjob wanted]
  (let [active (count (-> cronjob :status :active))
        target (kubectl/namespace-name-of cronjob)]
    (if wanted
      (cond-> [(format "suspend CronJob %s - no new Job starts until it is resumed" target)]
        (pos? active) (conj (format "%d Job(s) it started keep running" active)))
      (cond-> [(format "resume CronJob %s on its schedule %s" target (-> cronjob :spec :schedule))]
        (nil? (-> cronjob :spec :startingDeadlineSeconds))
        (conj "without startingDeadlineSeconds, a run missed while it was suspended starts at once")))))

(defn suspend-args
  [namespace name wanted]
  [(str "--namespace=" namespace) "patch" "cronjob" name "--type=merge"
   (str "--patch={\"spec\":{\"suspend\":" wanted "}}")])
