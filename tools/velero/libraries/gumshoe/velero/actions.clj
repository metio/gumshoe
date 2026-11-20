;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.velero.actions
  "The decisions behind the Velero action books, pure over the objects: pausing
   and resuming a Schedule, a backup now from a Schedule's template, and deleting
   a backup. These functions say when to refuse, what the confirmation promises,
   what to write, and when Velero has done it.

   A backup now is built as `velero backup create --from-schedule` builds it: the
   schedule's template as the spec, the template's labels and annotations (else
   the schedule's own), the velero.io/schedule-name label, and the name
   <schedule>-<UTC yyyyMMddHHmmss>. A backup is deleted through a
   DeleteBackupRequest, as `velero backup delete` does it: deleting the Backup
   object alone leaves its data in object storage, and the backup sync puts the
   object back."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(def schedule-name-label "velero.io/schedule-name")
(def backup-name-label "velero.io/backup-name")
(def backup-uid-label "velero.io/backup-uid")

(def running-phases
  "Backup phases in which Velero still works on the backup."
  #{"New" "Queued" "ReadyToStart" "InProgress" "WaitingForPluginOperations"
    "WaitingForPluginOperationsPartiallyFailed" "Finalizing" "FinalizingPartiallyFailed"})

(defn parse-pick
  "\"<namespace>/<name>\" as {:namespace :name}, or nil."
  [pick]
  (when-let [[_ namespace name] (re-matches #"\s*([^/\s]+)/([^/\s]+)\s*" (str pick))]
    {:namespace namespace :name name}))

(defn location-for
  "The BackupStorageLocation a backup writes to: the one it names, else the
   default one."
  [locations name]
  (let [items (kubectl/items-of locations)]
    (if (str/blank? name)
      (first (filter #(true? (-> % :spec :default)) items))
      (first (filter #(= name (kubectl/name-of %)) items)))))

(defn- location-label
  [location name]
  (or (kubectl/name-of location) name "the default location"))

;; --- pause and resume ------------------------------------------------------------

(defn paused?
  [schedule]
  (true? (-> schedule :spec :paused)))

(defn pause-refusal
  [schedule wanted]
  (cond
    (nil? schedule) "the Schedule no longer exists"
    (= wanted (paused? schedule)) (if wanted "it is paused already" "it is not paused")
    :else nil))

(defn pause-items
  [schedule wanted]
  (let [target (kubectl/namespace-name-of schedule)
        cron (-> schedule :spec :schedule)]
    (if wanted
      [(format "pause Schedule %s - no backup starts on its schedule %s until it is resumed" target cron)
       "the newest backup ages from now on, and the backup scan reports the schedule as paused"]
      [(format "resume Schedule %s - backups start again on its schedule %s" target cron)
       (str "Velero counts the next run from the last backup, so a run missed while it was paused starts a backup at once;"
            " velero schedule unpause --skip-immediately resumes without it")])))

(defn pause-patch
  [wanted]
  (json/generate-string {:spec {:paused wanted}}))

;; --- backup now ------------------------------------------------------------------

(defn backup-name
  [schedule ^java.time.Instant now]
  (str (kubectl/name-of schedule) "-"
       (.format (java.time.format.DateTimeFormatter/ofPattern "yyyyMMddHHmmss")
                (.atZone now java.time.ZoneOffset/UTC))))

(defn backup-refusal
  [schedule locations]
  (let [wanted (-> schedule :spec :template :storageLocation)
        location (location-for locations wanted)]
    (cond
      (nil? schedule) "the Schedule no longer exists"
      (= "FailedValidation" (-> schedule :status :phase))
      (str "the Schedule failed validation, so its template does not make a valid backup: "
           (str/join "; " (or (seq (-> schedule :status :validationErrors)) ["no error recorded"])))
      (nil? location) (if (str/blank? wanted)
                        "the template names no storage location and none is the default"
                        (format "BackupStorageLocation %s does not exist" wanted))
      (not= "Available" (-> location :status :phase))
      (format "BackupStorageLocation %s is %s, so the backup cannot be written"
              (kubectl/name-of location) (or (-> location :status :phase) "not yet checked"))
      :else nil)))

(defn- metadata-from
  [schedule key]
  (let [template (-> schedule :spec :template :metadata key)]
    (if (some? template) template (-> schedule :metadata key))))

(defn backup-manifest
  [schedule name]
  (let [labels (assoc (or (metadata-from schedule :labels) {}) (keyword schedule-name-label) (kubectl/name-of schedule))
        annotations (dissoc (or (metadata-from schedule :annotations) {})
                            :kubectl.kubernetes.io/last-applied-configuration)]
    {:apiVersion "velero.io/v1"
     :kind "Backup"
     :metadata (cond-> {:namespace (kubectl/namespace-of schedule) :name name :labels labels}
                 (seq annotations) (assoc :annotations annotations))
     :spec (or (-> schedule :spec :template) {})}))

(defn backup-items
  [schedule name]
  (let [template (-> schedule :spec :template)
        namespaces (:includedNamespaces template)]
    [(format "start Backup %s/%s from Schedule %s's template" (kubectl/namespace-of schedule) name (kubectl/name-of schedule))
     (format "it covers %s and writes to %s"
             (if (or (empty? namespaces) (= ["*"] namespaces)) "every namespace" (str/join ", " namespaces))
             (location-label nil (:storageLocation template)))
     (format "it carries the label %s=%s, so it counts as one of the schedule's backups and expires after %s"
             schedule-name-label (kubectl/name-of schedule) (or (:ttl template) "the default TTL"))]))

(defn backup-completed?
  [backup]
  (= "Completed" (-> backup :status :phase)))

(defn backup-signals
  "Why the wait can end: a backup that ended in anything but Completed."
  [backup]
  (let [{:keys [phase failureReason validationErrors errors warnings]} (:status backup)]
    (case phase
      "Failed" [(str "the Backup failed: " (or failureReason "no reason recorded"))]
      "FailedValidation" [(str "the Backup failed validation: " (str/join "; " (or (seq validationErrors) ["no error recorded"])))]
      "PartiallyFailed" [(format "the Backup partially failed with %s errors and %s warnings - velero backup logs names them"
                                 (or errors "some") (or warnings 0))]
      nil)))

;; --- delete ----------------------------------------------------------------------

(defn delete-refusal
  [backup locations]
  (let [phase (-> backup :status :phase)
        name (-> backup :spec :storageLocation)
        location (location-for locations name)]
    (cond
      (nil? backup) "the Backup no longer exists"
      (= "Deleting" phase) "it is being deleted already"
      (or (nil? phase) (running-phases phase)) (format "it is still running (%s) - wait for it to finish" (or phase "New"))
      (nil? location) (format "BackupStorageLocation %s does not exist, so Velero cannot remove the backup's data" (location-label nil name))
      (= "ReadOnly" (-> location :spec :accessMode))
      (format "BackupStorageLocation %s is read-only" (kubectl/name-of location))
      (not= "Available" (-> location :status :phase))
      (format "BackupStorageLocation %s is %s, so Velero cannot remove the backup's data"
              (kubectl/name-of location) (or (-> location :status :phase) "not yet checked"))
      :else nil)))

(defn delete-items
  [backup restores]
  (let [name (kubectl/name-of backup)
        restored (filter #(= name (-> % :spec :backupName)) (kubectl/items-of restores))]
    (cond-> [(format "delete Backup %s - its files in BackupStorageLocation %s, its volume snapshots, file-system backups and data-mover copies are removed"
                     (kubectl/namespace-name-of backup) (location-label nil (-> backup :spec :storageLocation)))
             "nothing restores from it afterwards; this cannot be undone"]
      (seq restored)
      (conj (format "its Restores are deleted with it: %s" (str/join ", " (sort (map kubectl/name-of restored))))))))

(defn valid-label-value
  "A name as a label value, as Velero shortens one: names over 63 characters
   keep 57 and gain the first 6 hex digits of their SHA-256."
  [name]
  (if (<= (count name) 63)
    name
    (let [digest (.digest (java.security.MessageDigest/getInstance "SHA-256") (.getBytes ^String name "UTF-8"))
          hex (apply str (map #(format "%02x" (bit-and % 0xff)) digest))]
      (str (subs name 0 57) (subs hex 0 6)))))

(defn delete-request-manifest
  [backup]
  {:apiVersion "velero.io/v1"
   :kind "DeleteBackupRequest"
   :metadata {:namespace (kubectl/namespace-of backup)
              :generateName (str (kubectl/name-of backup) "-")
              :labels {(keyword backup-name-label) (valid-label-value (kubectl/name-of backup))
                       (keyword backup-uid-label) (-> backup :metadata :uid)}}
   :spec {:backupName (kubectl/name-of backup)}})

(defn delete-signals
  "Why the wait can end: a processed request for this backup that recorded errors."
  [requests uid]
  (vec (for [request (kubectl/items-of requests)
             :when (= uid (get-in request [:metadata :labels (keyword backup-uid-label)]))
             :let [{:keys [phase errors]} (:status request)]
             :when (and (= "Processed" phase) (seq errors))]
         (str "the DeleteBackupRequest failed: " (str/join "; " errors)))))

(defn to-json
  [manifest]
  (json/generate-string manifest))
