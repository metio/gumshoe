;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.velero-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.detectives.velero :as velero]
            [gumshoe.tools.velero]
            [gumshoe.velero.phases :as phases]))

(def ^:private now (java.time.Instant/parse "2026-11-19T12:00:00Z"))

(defn- object [name & {:keys [namespace labels created spec status] :or {namespace "velero" created "2026-11-19T00:00:00Z"}}]
  {:metadata (cond-> {:namespace namespace :name name :creationTimestamp created} labels (assoc :labels labels))
   :spec (or spec {})
   :status (or status {})})

(defn- backup [name phase & {:keys [schedule start end errors warnings reason]}]
  (object name
          :labels (when schedule {(keyword "velero.io/schedule-name") schedule})
          :created (or start "2026-11-19T00:00:00Z")
          :status (cond-> {:phase phase}
                    start (assoc :startTimestamp start)
                    end (assoc :completionTimestamp end)
                    errors (assoc :errors errors)
                    warnings (assoc :warnings warnings)
                    reason (assoc :failureReason reason))))

(defn- run [detect type items] (detect {:now now type {:items items}}))

;; --- storage locations -------------------------------------------------------

(deftest unavailable-locations
  (let [[finding & more] (run velero/detect-unavailable-locations velero/location-type
                              [(object "default" :spec {:default true} :status {:phase "Unavailable" :message "rpc error: bucket not found"})
                               (object "secondary" :status {:phase "Available"})])]
    (is (empty? more))
    (is (= :critical (:severity finding)))
    (is (= "velero/default" (:component finding)))
    (is (= "default BackupStorageLocation is Unavailable" (:summary finding)))
    (is (str/starts-with? (:hint finding) "rpc error: bucket not found - backups that write to it fail")))
  (testing "a location that is not the default says so less loudly"
    (is (= "BackupStorageLocation is Unavailable"
           (:summary (first (run velero/detect-unavailable-locations velero/location-type
                                 [(object "s" :status {:phase "Unavailable"})])))))))

;; --- backups -------------------------------------------------------------------

(deftest the-newest-backup-of-a-schedule-is-judged
  (testing "an old failure followed by a success is history"
    (is (empty? (run velero/detect-failed-backups velero/backup-type
                     [(backup "daily-1" "Failed" :schedule "daily" :start "2026-11-17T01:00:00Z")
                      (backup "daily-2" "Completed" :schedule "daily" :start "2026-11-18T01:00:00Z")]))))
  (testing "a newest failure reports, however old"
    (let [[finding] (run velero/detect-failed-backups velero/backup-type
                         [(backup "daily-1" "Completed" :schedule "daily" :start "2026-11-10T01:00:00Z")
                          (backup "daily-2" "Failed" :schedule "daily" :start "2026-11-11T01:00:00Z" :end "2026-11-11T01:05:00Z" :reason "error getting backup store")])]
      (is (= :critical (:severity finding)))
      (is (= "velero/daily-2" (:component finding)))
      (is (= "Backup failed" (:summary finding)))
      (is (str/starts-with? (:hint finding) "error getting backup store - velero backup logs daily-2"))
      (is (= "2026-11-11T01:05:00Z" (:since finding)))))
  (testing "a running backup does not hide the newest finished one"
    (is (= ["velero/daily-1"]
           (map :component (run velero/detect-failed-backups velero/backup-type
                                [(backup "daily-1" "PartiallyFailed" :schedule "daily" :start "2026-11-18T01:00:00Z")
                                 (backup "daily-2" "InProgress" :schedule "daily" :start "2026-11-19T01:00:00Z")])))))
  (testing "schedules are told apart by name and namespace"
    (is (= 2 (count (run velero/detect-failed-backups velero/backup-type
                         [(backup "a-1" "Failed" :schedule "a" :start "2026-11-19T01:00:00Z")
                          (backup "b-1" "Failed" :schedule "b" :start "2026-11-19T01:00:00Z")
                          (backup "b-2" "Completed" :schedule "b" :start "2026-11-19T02:00:00Z")
                          (assoc-in (backup "a-1" "Failed" :schedule "a" :start "2026-11-19T01:00:00Z") [:metadata :namespace] "other")]))))))

(deftest manual-backups-are-judged-for-a-day
  (is (= ["velero/manual-new"]
         (map :component (run velero/detect-failed-backups velero/backup-type
                              [(backup "manual-new" "Failed" :start "2026-11-19T06:00:00Z")
                               (backup "manual-old" "Failed" :start "2026-11-17T06:00:00Z")
                               (backup "manual-ok" "Completed" :start "2026-11-19T06:00:00Z")])))))

(deftest backup-findings-by-phase
  (is (= "Backup partially failed (1 error, 3 warnings)"
         (:summary (velero/backup-finding (backup "b" "PartiallyFailed" :errors 1 :warnings 3)))))
  (is (= "Backup partially failed (0 errors, 0 warnings)"
         (:summary (velero/backup-finding (backup "b" "PartiallyFailed")))))
  (is (= :warning (:severity (velero/backup-finding (backup "b" "PartiallyFailed")))))
  (let [finding (velero/backup-finding (assoc-in (backup "b" "FailedValidation") [:status :validationErrors] ["invalid included namespace" "no location"]))]
    (is (= :critical (:severity finding)))
    (is (str/starts-with? (:hint finding) "invalid included namespace; no location")))
  (doseq [phase ["Completed" "New" "Queued" "Deleting" nil]]
    (is (nil? (velero/backup-finding (backup "b" phase))) (str phase))))

(deftest stuck-backups
  (let [[finding & more] (run velero/detect-stuck-backups velero/backup-type
                              [(backup "slow" "WaitingForPluginOperations" :start "2026-11-18T22:00:00Z")
                               (backup "fresh" "InProgress" :start "2026-11-19T06:00:00Z")
                               (backup "done" "Completed" :start "2026-11-17T06:00:00Z")])]
    (is (empty? more))
    (is (= "Backup has been WaitingForPluginOperations for 14 hours" (:summary finding)))
    (is (= :warning (:severity finding)))
    (is (= "2026-11-18T22:00:00Z" (:since finding))))
  (testing "twelve hours is stuck"
    (is (seq (run velero/detect-stuck-backups velero/backup-type [(backup "b" "Finalizing" :start "2026-11-19T00:00:00Z")])))))

;; --- schedules -----------------------------------------------------------------

(deftest cron-intervals
  (doseq [[expression hours] {"0 1 * * *" 24
                              "CRON_TZ=Europe/Berlin 0 1 * * *" 24
                              "TZ=UTC 30 2 * * *" 24
                              "15 * * * *" 1
                              "0 */6 * * *" 6
                              "0 3 * * 0" 168
                              "0 3 * * MON-FRI" 168
                              "@hourly" 1 "@daily" 24 "@midnight" 24 "@weekly" 168
                              "@every 12h" 12 "@every 1h30m" 1.5 "@every 30m" 0.5
                              "0 3 1 * *" nil "0 3 * 6 *" nil "@monthly" nil "@yearly" nil
                              "not a cron" nil "" nil}]
    (let [actual (velero/interval-hours expression)]
      (if hours
        (is (and actual (== hours actual)) expression)
        (is (nil? actual) expression)))))

(defn- schedule [& {:keys [expression last paused phase created errors]
                    :or {expression "0 1 * * *" phase "Enabled" created "2026-11-01T00:00:00Z"}}]
  (object "daily" :created created
          :spec (cond-> {:schedule expression} paused (assoc :paused true))
          :status (cond-> {:phase phase} last (assoc :lastBackup last) errors (assoc :validationErrors errors))))

(deftest overdue-schedules
  (testing "a daily schedule that backed up last night is fine"
    (is (empty? (velero/schedule-findings (schedule :last "2026-11-19T01:00:00Z") now))))
  (testing "one missed run is overdue"
    (let [[finding] (velero/schedule-findings (schedule :last "2026-11-17T01:00:00Z") now)]
      (is (= :warning (:severity finding)))
      (is (= "Schedule's last backup is 59 hours old, it runs every 24 hours" (:summary finding)))
      (is (str/includes? (:hint finding) "expression 0 1 * * *"))
      (is (= "2026-11-17T01:00:00Z" (:since finding)))))
  (testing "fractional intervals read naturally"
    (is (str/ends-with? (:summary (first (velero/schedule-findings (schedule :expression "@every 1h30m" :last "2026-11-19T09:00:00Z") now)))
                        "it runs every 1.5 hours")))
  (testing "a schedule that never backed up counts from its creation"
    (is (= "Schedule has not created a backup in 444 hours"
           (:summary (first (velero/schedule-findings (schedule) now))))))
  (testing "a new schedule gets its first run"
    (is (empty? (velero/schedule-findings (schedule :created "2026-11-19T00:00:00Z") now))))
  (testing "an expression it cannot bound is not judged"
    (is (empty? (velero/schedule-findings (schedule :expression "0 3 1 * *" :last "2026-10-01T03:00:00Z") now)))))

(deftest paused-and-invalid-schedules
  (let [[finding] (velero/schedule-findings (schedule :paused true :last "2026-10-01T01:00:00Z") now)]
    (is (= :info (:severity finding)) "paused wins over overdue")
    (is (= "Schedule is paused" (:summary finding))))
  (let [[finding] (velero/schedule-findings (schedule :phase "FailedValidation" :errors ["invalid schedule: expected 5 fields"]) now)]
    (is (= :critical (:severity finding)))
    (is (str/starts-with? (:hint finding) "invalid schedule: expected 5 fields - it creates no backups"))))

;; --- restores and repositories --------------------------------------------------

(deftest failed-restores-from-the-last-day
  (let [restore (fn [name phase end & {:as status}]
                  (object name :status (merge {:phase phase :completionTimestamp end} status)))
        findings (run velero/detect-failed-restores velero/restore-type
                      [(restore "r1" "Failed" "2026-11-19T10:00:00Z" :failureReason "backup not found")
                       (restore "r2" "PartiallyFailed" "2026-11-19T10:00:00Z" :errors 2)
                       (restore "r3" "FailedValidation" "2026-11-19T10:00:00Z" :validationErrors ["backup x not found"])
                       (restore "r4" "Failed" "2026-11-17T10:00:00Z")
                       (restore "r5" "Completed" "2026-11-19T10:00:00Z")])
        by-name (into {} (map (juxt :component identity)) findings)]
    (is (= #{"velero/r1" "velero/r2" "velero/r3"} (set (keys by-name))))
    (is (= :critical (:severity (by-name "velero/r1"))))
    (is (str/starts-with? (:hint (by-name "velero/r1")) "backup not found - velero restore describe r1"))
    (is (= ["Restore partially failed (2 errors)" :warning] ((juxt :summary :severity) (by-name "velero/r2"))))
    (is (= "Restore failed validation" (:summary (by-name "velero/r3"))))
    (is (str/starts-with? (:hint (by-name "velero/r3")) "backup x not found - "))))

(deftest repositories
  (let [[finding & more] (run velero/detect-repositories velero/repository-type
                              [(object "app-default-kopia" :status {:phase "NotReady" :message "error to connect to storage"})
                               (object "ok" :status {:phase "Ready"})
                               (object "new" :status {:phase "New"})])]
    (is (empty? more))
    (is (= :critical (:severity finding)))
    (is (str/starts-with? (:hint finding) "error to connect to storage - "))))

;; --- wiring -----------------------------------------------------------------------

(deftest the-package-fills-the-backups-scope
  (is (= (set (map :name velero/detectives)) (set (map :name (registry/for-scope :backups)))))
  (testing "every detective survives a cluster without Velero"
    (is (empty? (detective/run-detectives velero/detectives {:now now})))))

(deftest a-pending-backup-does-not-hide-the-failure-before-it
  (doseq [phase ["New" "Queued" "ReadyToStart" nil]]
    (is (= ["velero/nightly-1"]
           (map :component (run velero/detect-failed-backups velero/backup-type
                                [(backup "nightly-1" "Failed" :schedule "nightly" :start "2026-11-18T03:00:00Z")
                                 (backup "nightly-2" phase :schedule "nightly" :start "2026-11-19T03:00:00Z")])))
        (str phase))))

(deftest a-backup-waiting-to-start-for-hours-is-stuck
  (doseq [[phase label] [["New" "New"] ["Queued" "Queued"] ["ReadyToStart" "ReadyToStart"] [nil "New"]]]
    (is (= [(format "Backup has been %s for 15 hours" label)]
           (map :summary (run velero/detect-stuck-backups velero/backup-type
                              [(backup "waiting" phase :start "2026-11-18T21:00:00Z")])))
        (str phase))))

(deftest every-running-phase-is-judged-as-running
  (doseq [phase phases/running-phases]
    (is (empty? (run velero/detect-failed-backups velero/backup-type
                     [(backup "b" phase :schedule "s" :start "2026-11-19T03:00:00Z")]))
        phase)
    (is (seq (run velero/detect-stuck-backups velero/backup-type
                  [(backup "b" phase :start "2026-11-18T21:00:00Z")]))
        phase)))
