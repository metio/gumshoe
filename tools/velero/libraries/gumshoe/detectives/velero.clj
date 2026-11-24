;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.velero
  "Detectives for Velero: storage locations it cannot reach, backups and
   restores that failed, schedules that stopped producing backups, operations
   that never finish, and backup repositories that are not ready. A schedule is
   judged by its newest backup; older failures are history."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def backup-type "backups.velero.io")
(def schedule-type "schedules.velero.io")
(def location-type "backupstoragelocations.velero.io")
(def restore-type "restores.velero.io")
(def repository-type "backuprepositories.velero.io")

(def schedule-label (keyword "velero.io/schedule-name"))

(def ^:private recent-hours
  "How far back a backup or restore nobody scheduled is judged - long enough to
   see last night's manual run, short enough that a failure fixed since fades."
  24)

(def ^:private stuck-hours
  "How long a backup may stay in a running phase before it counts as stuck.
   Plugin operations (CSI snapshots, data movers) run long, so this is generous."
  12)

(def ^:private running-phases
  #{"InProgress" "WaitingForPluginOperations" "WaitingForPluginOperationsPartiallyFailed"
    "Finalizing" "FinalizingPartiallyFailed"})

(defn- hours-since
  [value now]
  (when-let [at (onset/instant value)]
    (when now (/ (.toMinutes (java.time.Duration/between at ^java.time.Instant now)) 60.0))))

(defn- started-at
  [object]
  (or (-> object :status :startTimestamp) (-> object :metadata :creationTimestamp)))

(defn- finished-at
  [object]
  (or (-> object :status :completionTimestamp) (started-at object)))

;; --- storage locations -----------------------------------------------------------

(defn detect-unavailable-locations
  [evidence]
  (for [location (kubectl/items-of (get evidence location-type))
        :when (= "Unavailable" (-> location :status :phase))]
    {:severity :critical
     :component (kubectl/namespace-name-of location)
     :summary (str (if (-> location :spec :default) "default " "")
                   "BackupStorageLocation is Unavailable")
     :hint (str (some-> (-> location :status :message) not-empty (str " - "))
                "backups that write to it fail - check the bucket, its credentials Secret and the network path from the velero pod")}))

;; --- backups -----------------------------------------------------------------------

(defn backup-finding
  "The finding for a finished backup, or nil when it completed."
  [backup]
  (let [{:keys [phase errors warnings failureReason validationErrors]} (:status backup)
        base {:component (kubectl/namespace-name-of backup)}]
    (some->
     (case phase
       "Failed"
       (assoc base :severity :critical :summary "Backup failed"
              :hint (str (some-> failureReason not-empty (str " - "))
                         "velero backup logs " (kubectl/name-of backup) " shows the error"))
       "FailedValidation"
       (assoc base :severity :critical :summary "Backup failed validation"
              :hint (str (str/join "; " validationErrors) " - fix the spec of the backup or its schedule"))
       "PartiallyFailed"
       (assoc base :severity :warning
              :summary (format "Backup partially failed (%d error%s, %d warning%s)"
                               (or errors 0) (if (= 1 errors) "" "s") (or warnings 0) (if (= 1 warnings) "" "s"))
              :hint (str "some items or volumes are missing from it - velero backup describe "
                         (kubectl/name-of backup) " --details names them"))
       nil)
     (onset/since (finished-at backup)))))

(defn- newest
  [objects]
  (last (sort-by #(str (started-at %)) objects)))

(defn detect-failed-backups
  "The newest backup of every schedule, and every recent backup nobody
   scheduled, that did not complete."
  [evidence]
  (let [now (:now evidence)
        backups (remove #(contains? running-phases (-> % :status :phase))
                        (kubectl/items-of (get evidence backup-type)))
        {scheduled true manual false} (group-by #(some? (get-in % [:metadata :labels schedule-label])) backups)]
    (concat
     (keep (fn [[_ history]] (backup-finding (newest history)))
           (group-by (juxt kubectl/namespace-of #(get-in % [:metadata :labels schedule-label])) scheduled))
     (keep #(when (some-> (hours-since (finished-at %) now) (<= recent-hours)) (backup-finding %))
           manual))))

(defn detect-stuck-backups
  [evidence]
  (let [now (:now evidence)]
    (for [backup (kubectl/items-of (get evidence backup-type))
          :let [phase (-> backup :status :phase)
                hours (hours-since (started-at backup) now)]
          :when (and (contains? running-phases phase) hours (>= hours stuck-hours))]
      (onset/since
       {:severity :warning
        :component (kubectl/namespace-name-of backup)
        :summary (format "Backup has been %s for %d hours" phase (long hours))
        :hint "the velero pod or a plugin's data mover stopped making progress - check the velero and node-agent logs"}
       (started-at backup)))))

;; --- schedules ---------------------------------------------------------------------

(defn- go-duration-hours
  "Hours in a Go duration made of h and m parts (\"24h\", \"1h30m\"), or nil."
  [text]
  (when-let [[_ h m] (re-matches #"(?:(\d+)h)?(?:(\d+)m)?(?:0s)?" (str text))]
    (when (or h m)
      (+ (if h (parse-long h) 0) (/ (if m (parse-long m) 0) 60.0)))))

(defn interval-hours
  "The longest gap between two runs of a cron schedule, in hours, or nil when
   the expression is not one this can bound. Only the shape is read: a schedule
   restricted by day of month or month is left unjudged."
  [expression]
  (let [expression (str/trim (str/replace (str expression) #"^(?:CRON_)?TZ=\S+\s+" ""))]
    (cond
      (= "@hourly" expression) 1
      (contains? #{"@daily" "@midnight"} expression) 24
      (= "@weekly" expression) 168
      (str/starts-with? expression "@every ") (go-duration-hours (subs expression 7))
      :else
      (let [fields (str/split expression #"\s+")]
        (when (= 5 (count fields))
          (let [[minute hour dom month dow] fields]
            (cond
              (not (and (= "*" dom) (= "*" month))) nil
              (not= "*" dow) 168
              (and (= "*" hour) (not= "*" minute)) 1
              (re-matches #"\*/(\d+)" hour) (parse-long (second (re-matches #"\*/(\d+)" hour)))
              :else 24)))))))

(defn- hours-text
  [hours]
  (if (== hours (Math/floor hours)) (str (long hours)) (format "%.1f" (double hours))))

(defn schedule-findings
  [schedule now]
  (let [{:keys [phase lastBackup validationErrors]} (:status schedule)
        base {:component (kubectl/namespace-name-of schedule)}
        interval (interval-hours (-> schedule :spec :schedule))
        since (or lastBackup (-> schedule :metadata :creationTimestamp))
        age (hours-since since now)]
    (cond
      (= "FailedValidation" phase)
      [(assoc base :severity :critical :summary "Schedule failed validation"
              :hint (str (str/join "; " validationErrors) " - it creates no backups until its spec is fixed"))]

      (-> schedule :spec :paused)
      [(assoc base :severity :info :summary "Schedule is paused"
              :hint "it creates no backups - velero schedule unpause resumes it")]

      (and interval age (> age (* 1.5 interval)))
      [(onset/since
        (assoc base :severity :warning
               :summary (if lastBackup
                          (format "Schedule's last backup is %d hours old, it runs every %s hours" (long age) (hours-text interval))
                          (format "Schedule has not created a backup in %d hours" (long age)))
               :hint (format "expression %s - check the velero pod's logs for the schedule controller" (-> schedule :spec :schedule)))
        since)]

      :else nil)))

(defn detect-schedules
  [evidence]
  (mapcat #(schedule-findings % (:now evidence)) (kubectl/items-of (get evidence schedule-type))))

;; --- restores and repositories --------------------------------------------------------

(defn detect-failed-restores
  [evidence]
  (let [now (:now evidence)]
    (for [restore (kubectl/items-of (get evidence restore-type))
          :let [{:keys [phase errors failureReason validationErrors]} (:status restore)]
          :when (and (contains? #{"Failed" "PartiallyFailed" "FailedValidation"} phase)
                     (some-> (hours-since (finished-at restore) now) (<= recent-hours)))]
      (onset/since
       {:severity (if (= "PartiallyFailed" phase) :warning :critical)
        :component (kubectl/namespace-name-of restore)
        :summary (case phase
                   "PartiallyFailed" (format "Restore partially failed (%d error%s)" (or errors 0) (if (= 1 errors) "" "s"))
                   "FailedValidation" "Restore failed validation"
                   "Restore failed")
        :hint (str (some-> (not-empty (or (not-empty failureReason) (str/join "; " validationErrors))) (str " - "))
                   "velero restore describe " (kubectl/name-of restore) " --details names what was not restored")}
       (finished-at restore)))))

(defn detect-repositories
  [evidence]
  (for [repository (kubectl/items-of (get evidence repository-type))
        :when (= "NotReady" (-> repository :status :phase))]
    {:severity :critical
     :component (kubectl/namespace-name-of repository)
     :summary "BackupRepository is NotReady"
     :hint (str (some-> (-> repository :status :message) not-empty (str " - "))
                "file-system and data-mover backups of its namespace fail until the repository is reachable again")}))

(def detectives
  [{:name "velero-locations"
    :description "Velero BackupStorageLocations that are Unavailable"
    :requires [location-type]
    :detect detect-unavailable-locations}
   {:name "velero-backups"
    :description "Velero backups that failed: the newest of each schedule, and recent manual ones"
    :requires [backup-type]
    :detect detect-failed-backups}
   {:name "velero-stuck-backups"
    :description "Velero backups that stay in a running phase for 12 hours or more"
    :requires [backup-type]
    :detect detect-stuck-backups}
   {:name "velero-schedules"
    :description "Velero schedules that failed validation, are paused, or are overdue"
    :requires [schedule-type]
    :detect detect-schedules}
   {:name "velero-restores"
    :description "Velero restores from the last day that failed"
    :requires [restore-type]
    :detect detect-failed-restores}
   {:name "velero-repositories"
    :description "Velero BackupRepositories that are NotReady"
    :requires [repository-type]
    :detect detect-repositories}])
