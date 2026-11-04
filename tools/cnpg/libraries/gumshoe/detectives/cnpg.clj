;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.cnpg
  "Detectives for CloudNativePG: unhealthy clusters, missing replicas, failing
   WAL archiving, and a cluster left with no writable primary.

   The last of those is not visible in the phase or the ready count. A cluster
   can report a plausible phase and two of three instances ready while the one
   instance it designates primary is the unhealthy one - replicas in recovery
   and nothing accepting writes.

   Nor is a crashlooping instance visible there. An instance whose postgres
   never starts leaves the pod restarting indefinitely while the cluster reports
   a storage phase and a ready count that look survivable."
  (:require [gumshoe.cnpg.instances :as inst]
            [gumshoe.kubectl :as kubectl]))

(def cluster-type "clusters.postgresql.cnpg.io")
(def backup-type "backups.postgresql.cnpg.io")
(def scheduled-backup-type "scheduledbackups.postgresql.cnpg.io")

(def ^:private healthy-phase "Cluster in healthy state")

(defn- condition
  [resource type]
  (first (filter #(= type (:type %)) (-> resource :status :conditions))))

(def ^:private stuck-after (java.time.Duration/ofHours 1))

(defn- instant
  [value]
  (when value (try (java.time.Instant/parse value) (catch Exception _ nil))))

(defn- describe-duration
  [^java.time.Duration d]
  (let [days (.toDays d) hours (.toHoursPart d)]
    (if (pos? days) (format "%dd %dh" days hours) (format "%dh" (.toHours d)))))

(defn- instance-buckets
  "status.instancesStatus groups instance names by state: healthy, replicating,
   failed. The primary is expected among the healthy ones."
  [cluster]
  (-> cluster :status :instancesStatus))

(defn- bucket-holding
  [buckets instance]
  (some (fn [[state names]] (when (some #{instance} names) (name state))) buckets))

(defn detect-primary-problems
  "A cluster whose designated primary is not among its healthy instances, and a
   cluster that has been not-Ready long enough that it is stuck rather than
   rolling."
  [evidence]
  (let [now (:now evidence)
        clusters (kubectl/items-of (get evidence cluster-type))]
    (concat
     (for [cluster clusters
           :let [buckets (instance-buckets cluster)
                 primary (-> cluster :status :currentPrimary)
                 target (-> cluster :status :targetPrimary)]
           ;; no buckets means the operator reported no instance states, which
           ;; is unknown rather than broken
           :when (and primary (seq buckets)
                      (not (some #{primary} (:healthy buckets))))]
       {:severity :critical
        :component (kubectl/namespace-name-of cluster)
        :summary (format "the primary %s is not healthy%s" primary
                         (if-let [state (bucket-holding buckets primary)]
                           (format " (listed as %s)" state) ""))
        :hint (if (= primary target)
                (str "currentPrimary equals targetPrimary, so no switchover is under way - "
                     "the cluster can have healthy replicas and still accept no writes, and "
                     "the -rw service follows the primary")
                (format "a switchover to %s is under way; if it does not complete, nothing accepts writes"
                        target))})
     (for [cluster clusters
           :let [ready (condition cluster "Ready")
                 since (instant (:lastTransitionTime ready))
                 age (when (and now since) (java.time.Duration/between since now))]
           :when (and (= "False" (:status ready)) age (pos? (.compareTo age stuck-after)))]
       {:severity :critical
        :component (kubectl/namespace-name-of cluster)
        :summary (format "not Ready for %s" (describe-duration age))
        :hint (str "reason " (or (:reason ready) "unknown")
                   " - a roll clears in minutes, so this is stuck rather than in progress")}))))

(def ^:private crashloop-reason "CrashLoopBackOff")

(defn- crashlooping-containers
  [pod]
  (filter #(= crashloop-reason (-> % :state :waiting :reason))
          (-> pod :status :containerStatuses)))

(defn- designated-primary?
  "Whether this pod is the one its cluster names in status.currentPrimary. An
   absent or unreadable cluster leaves the finding unenriched rather than
   suppressing it: a crashlooping instance is reportable either way."
  [clusters pod]
  (let [namespace (kubectl/namespace-of pod)
        cluster (inst/cluster-of pod)]
    (boolean
     (some #(and (= namespace (kubectl/namespace-of %))
                 (= cluster (kubectl/name-of %))
                 (= (kubectl/name-of pod) (-> % :status :currentPrimary)))
           clusters))))

(defn detect-crashlooping-instances
  "Instance pods whose container is in CrashLoopBackOff. The restart count is
   the part that distinguishes a pod that will recover from one that has been
   failing for days."
  [evidence]
  (let [clusters (kubectl/items-of (get evidence cluster-type))]
    (for [pod (filter inst/instance-pod? (kubectl/items-of (get evidence "pods")))
          container (crashlooping-containers pod)]
      {:severity :critical
       :component (kubectl/namespace-name-of pod)
       :summary (format "%s is in CrashLoopBackOff after %d restarts"
                        (:name container) (or (:restartCount container) 0))
       :hint (str (when (designated-primary? clusters pod)
                    "this is the cluster's designated primary, so nothing accepts writes - ")
                  "read the pod log for the reason; an instance with no room for WALs "
                  "refuses to start at all, so the count climbs without the cluster recovering")})))

(defn detect-cnpg-problems
  [evidence]
  (let [clusters (kubectl/items-of (get evidence cluster-type))]
    (concat
     (for [cluster clusters
           :let [phase (-> cluster :status :phase)]
           :when (and phase (not= healthy-phase phase))]
       {:severity :warning
        :component (kubectl/namespace-name-of cluster)
        :summary (format "cluster phase: %s" phase)
        :hint (-> cluster :status :phaseReason)})
     (for [cluster clusters
           :let [instances (-> cluster :spec :instances)
                 ready (-> cluster :status :readyInstances)]
           :when (and instances ready (< ready instances))]
       {:severity :critical
        :component (kubectl/namespace-name-of cluster)
        :summary (format "only %d of %d instances are ready" ready instances)})
     (for [cluster clusters
           :let [archiving (condition cluster "ContinuousArchiving")]
           :when (= "False" (:status archiving))]
       {:severity :warning
        :component (kubectl/namespace-name-of cluster)
        :summary "continuous WAL archiving is failing"
        :hint (:message archiving)}))))

(defn detect-backup-problems
  [evidence]
  (concat
   (for [backup (kubectl/items-of (get evidence backup-type))
         :when (= "failed" (-> backup :status :phase))]
     {:severity :warning
      :component (kubectl/namespace-name-of backup)
      :summary "backup failed"
      :hint (-> backup :status :error)})
   (for [scheduled (kubectl/items-of (get evidence scheduled-backup-type))
         :when (true? (-> scheduled :spec :suspend))]
     {:severity :info
      :component (kubectl/namespace-name-of scheduled)
      :summary "scheduled backup is suspended"
      :hint "resume it once the reason for the suspension is resolved"})))

(def detectives
  [{:name "cnpg"
    :description "CloudNativePG clusters: phase, ready instances, WAL archiving"
    :requires [cluster-type]
    :detect detect-cnpg-problems}
   {:name "cnpg-primary"
    :description "Clusters whose designated primary is unhealthy, or stuck not-Ready"
    :requires [cluster-type]
    :detect detect-primary-problems}
   {:name "cnpg-instance-crashloop"
    :description "Postgres instance pods stuck in CrashLoopBackOff"
    :requires [cluster-type "pods"]
    :detect detect-crashlooping-instances}
   {:name "cnpg-backups"
    :description "CloudNativePG backups that failed or are suspended"
    :requires [backup-type scheduled-backup-type]
    :detect detect-backup-problems}])
