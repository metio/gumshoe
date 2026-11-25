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
  (:require [clojure.string :as str]
            [gumshoe.cnpg.instances :as inst]
            [gumshoe.conditions :as conditions]
            [gumshoe.cron :as cron]
            [gumshoe.evidence :as evidence]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def cluster-type "clusters.postgresql.cnpg.io")
(def backup-type "backups.postgresql.cnpg.io")
(def scheduled-backup-type "scheduledbackups.postgresql.cnpg.io")

(def ^:private healthy-phase "Cluster in healthy state")
(def ^:private healthy-bucket "healthy")

(def ^:private stuck-after (java.time.Duration/ofHours 1))

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
                 target (-> cluster :status :targetPrimary)
                 state (bucket-holding buckets primary)]
           ;; The operator must positively place the primary in a state that is
           ;; not healthy. A primary in no bucket at all is unknown, not broken:
           ;; that is how a primary being restarted on purpose reads, and
           ;; reporting it would call a deliberate restart an outage.
           :when (and primary (seq buckets) state (not= healthy-bucket state))]
       {:severity :critical
        :component (kubectl/namespace-name-of cluster)
        :summary (format "the primary %s is not healthy (listed as %s)" primary state)
        :hint (if (= primary target)
                (str "currentPrimary equals targetPrimary, so no switchover is under way - "
                     "the cluster can have healthy replicas and still accept no writes, and "
                     "the -rw service follows the primary")
                (format "a switchover to %s is under way; if it does not complete, nothing accepts writes"
                        target))})
     (for [cluster clusters
           :let [ready (conditions/condition cluster "Ready")
                 since (onset/instant (:lastTransitionTime ready))
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

(def ^:private blocked-phases
  "Phases the operator does not leave on its own: each waits for someone to
   change the cluster, its definition or its environment."
  #{"Waiting for user action"
    "Cluster is unrecoverable and needs manual intervention"
    "Invalid cluster definition"
    "Unable to create required cluster objects"
    "Cluster cannot proceed to reconciliation due to an unknown plugin being required"
    "Cluster cannot proceed to reconciliation due to an error while interacting with plugins"
    "Cluster has incomplete or invalid image catalog"
    "Cluster cannot execute instance online upgrade due to missing architecture binary"})

(defn detect-cnpg-problems
  [evidence]
  (let [clusters (kubectl/items-of (get evidence cluster-type))]
    (concat
     (for [cluster clusters
           :let [phase (-> cluster :status :phase)
                 blocked? (contains? blocked-phases phase)]
           :when (and phase (not= healthy-phase phase))]
       {:severity (if blocked? :critical :warning)
        :component (kubectl/namespace-name-of cluster)
        :summary (format "cluster phase: %s" phase)
        :hint (not-empty (str/join " - " (remove str/blank? [(-> cluster :status :phaseReason)
                                                             (when blocked? "the operator waits for a change and will not recover on its own")])))})
     (for [cluster clusters
           :let [instances (-> cluster :spec :instances)
                 ready (-> cluster :status :readyInstances)]
           :when (and instances ready (< ready instances))]
       {:severity :critical
        :component (kubectl/namespace-name-of cluster)
        :summary (format "only %d of %d instances are ready" ready instances)})
     (for [cluster clusters
           :let [archiving (conditions/condition cluster "ContinuousArchiving")]
           :when (= "False" (:status archiving))]
       {:severity :critical
        :component (kubectl/namespace-name-of cluster)
        :summary "continuous WAL archiving is failing"
        :hint (str/join " - " (remove str/blank? [(:message archiving)
                                                  "no new recovery point is written, and WAL files pile up on the primary's volume until archiving works again"]))}))))

;; --- certificates ---------------------------------------------------------------

(def ^:private certificate-warning-days 7)

(def ^:private go-time
  (-> (java.time.format.DateTimeFormatterBuilder.)
      (.appendPattern "yyyy-MM-dd HH:mm:ss")
      (.appendFraction java.time.temporal.ChronoField/NANO_OF_SECOND 0 9 true)
      (.appendPattern " Z")
      (.toFormatter)))

(defn parse-expiration
  "An expiration as the operator records it. RFC 3339 is read, and so is Go's
   time.Time String form (\"2026-03-01 10:22:33 +0000 UTC\", possibly with
   fractional seconds and a monotonic clock suffix); anything else is nil."
  [value]
  (let [value (str/trim (str value))]
    (or (try (java.time.Instant/parse value) (catch Exception _ nil))
        (try (.toInstant (java.time.OffsetDateTime/parse value)) (catch Exception _ nil))
        (when-let [[_ stamp] (re-find #"^(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}(?:\.\d+)? [+-]\d{4})" value)]
          (try (.toInstant (java.time.OffsetDateTime/parse stamp go-time)) (catch Exception _ nil))))))

(defn detect-certificate-expiry
  "Certificates of a cluster that expired or expire within a week. The
   operator renews the certificates it issues itself before that, so one this
   close to expiry is either user-provided or not being renewed."
  [evidence]
  (let [now ^java.time.Instant (:now evidence)]
    (for [cluster (kubectl/items-of (get evidence cluster-type))
          [key expiration] (sort-by key (-> cluster :status :certificates :expirations))
          :let [secret (if (keyword? key) (subs (str key) 1) (str key))
                at (parse-expiration expiration)
                days (when (and at now) (.toDays (java.time.Duration/between now at)))]
          :when (and days (or (.isBefore ^java.time.Instant at now) (< days certificate-warning-days)))]
      {:severity (if (.isBefore ^java.time.Instant at now) :critical :warning)
       :component (kubectl/namespace-name-of cluster)
       :summary (if (.isBefore ^java.time.Instant at now)
                  (format "certificate in Secret %s expired %s" secret at)
                  (format "certificate in Secret %s expires in %d days" secret days))
       :hint "the operator renews the certificates it issues before this point - a user-provided certificate must be replaced by whoever issues it"
       :since (when (.isBefore ^java.time.Instant at now) (str at))})))

;; --- primary role labels --------------------------------------------------------

(def ^:private role-labels [:role :cnpg.io/instanceRole])

(defn- labelled-primary?
  [pod]
  (some #(= "primary" (get-in pod [:metadata :labels %])) role-labels))

(def ^:private moving-phases #{"Switchover in progress" "Failing over" "Setting up primary"})

(def primary-label-joins
  "The clusters name their designated primary, and the instance pods carry
   the role labels compared against it."
  [cluster-type "pods"])

(defn detect-primary-label-mismatch
  "Instance pods whose primary role label disagrees with the cluster's
   designated primary. Services that select the primary by its role label
   then reach no pod, or a replica that refuses writes. A switchover or
   failover relabels pods on its way, so a cluster in one is left alone.
   Nothing is judged when a joined read did not answer."
  [evidence]
  (let [pods (group-by (juxt kubectl/namespace-of inst/cluster-of)
                       (filter inst/instance-pod? (kubectl/items-of (get evidence "pods"))))]
    (for [cluster (when (evidence/joined? evidence primary-label-joins)
                    (kubectl/items-of (get evidence cluster-type)))
          :let [primary (-> cluster :status :currentPrimary)
                instances (get pods [(kubectl/namespace-of cluster) (kubectl/name-of cluster)])
                labelled (set (map kubectl/name-of (filter labelled-primary? instances)))]
          :when (and primary (seq instances)
                     (= primary (-> cluster :status :targetPrimary))
                     (not (contains? moving-phases (-> cluster :status :phase))))
          finding [(when (and (some #(= primary (kubectl/name-of %)) instances) (not (labelled primary)))
                     {:severity :critical
                      :component (kubectl/namespace-name-of cluster)
                      :summary (format "the designated primary %s does not carry the primary role label" primary)
                      :hint "Services that select the primary by role reach no pod, so clients cannot write - the instance manager sets the label; check its log"})
                   (when-let [others (seq (sort (disj labelled primary)))]
                     {:severity :critical
                      :component (kubectl/namespace-name-of cluster)
                      :summary (format "%s carr%s the primary role label but %s is the designated primary"
                                       (str/join ", " others) (if (next others) "y" "ies") primary)
                      :hint "Services that select the primary by role also reach a pod that is not the primary, and writes sent there fail"})]
          :when finding]
      finding)))

;; --- scheduled backups ----------------------------------------------------------

(defn schedule-interval
  "The longest gap between two runs of a ScheduledBackup schedule. Its six
   fields start with seconds; the remaining five read like a CronJob's, and
   dropping the seconds only rounds the gap up."
  [schedule]
  (let [fields (str/split (str/trim (str schedule)) #"\s+")]
    (cron/interval (if (= 6 (count fields)) (str/join " " (rest fields)) schedule))))

(defn detect-overdue-scheduled-backups
  "ScheduledBackups that should have run by now: more than twice their
   interval, and at least two hours, since the last run (or since creation,
   when it never ran)."
  [evidence]
  (let [now ^java.time.Instant (:now evidence)]
    (for [scheduled (kubectl/items-of (get evidence scheduled-backup-type))
          :when (not (true? (-> scheduled :spec :suspend)))
          :let [interval (schedule-interval (-> scheduled :spec :schedule))
                last-run (onset/instant (-> scheduled :status :lastScheduleTime))
                reference (or last-run (onset/instant (-> scheduled :metadata :creationTimestamp)))
                allowed (when interval (let [twice (.multipliedBy ^java.time.Duration interval 2)]
                                         (if (neg? (.compareTo twice (java.time.Duration/ofHours 2)))
                                           (java.time.Duration/ofHours 2) twice)))]
          :when (and now reference allowed (.isBefore ^java.time.Instant (.plus ^java.time.Instant reference ^java.time.Duration allowed) now))]
      {:severity :warning
       :component (kubectl/namespace-name-of scheduled)
       :summary (if last-run
                  (format "scheduled backup has not run since %s" last-run)
                  "scheduled backup has never run")
       :hint (format "schedule %s - check the operator log and the ScheduledBackup's events; the cluster's backups stop with it"
                     (-> scheduled :spec :schedule))
       :since (str reference)})))

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
    ;; the pods are what the check judges; the Cluster read only names the
    ;; designated primary in the hint, so it stays optional
    :joins ["pods"]
    :detect detect-crashlooping-instances}
   {:name "cnpg-certificates"
    :description "CloudNativePG certificates that expired or expire within a week"
    :requires [cluster-type]
    :detect detect-certificate-expiry}
   {:name "cnpg-primary-labels"
    :description "Instance pods whose primary role label disagrees with the designated primary"
    :requires primary-label-joins
    :joins primary-label-joins
    :detect detect-primary-label-mismatch}
   {:name "cnpg-scheduled-backups"
    :description "CloudNativePG ScheduledBackups that stopped running"
    :requires [scheduled-backup-type]
    :detect detect-overdue-scheduled-backups}
   {:name "cnpg-backups"
    :description "CloudNativePG backups that failed or are suspended"
    :requires [backup-type scheduled-backup-type]
    :detect detect-backup-problems}])
