;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.velero.actions-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.velero.action-books :as books]
            [gumshoe.velero.actions :as actions]))

(defn- schedule [& {:keys [paused template labels annotations phase errors]
                    :or {template {:includedNamespaces ["shop"] :ttl "720h0m0s"}}}]
  {:metadata (cond-> {:namespace "velero" :name "nightly"}
               labels (assoc :labels labels)
               annotations (assoc :annotations annotations))
   :spec (cond-> {:schedule "0 2 * * *" :template template}
           (some? paused) (assoc :paused paused))
   :status (cond-> {:phase (or phase "Enabled")} errors (assoc :validationErrors errors))})

(defn- location [name & {:keys [phase default mode] :or {phase "Available"}}]
  {:metadata {:namespace "velero" :name name}
   :spec (cond-> {:provider "aws"} default (assoc :default true) mode (assoc :accessMode mode))
   :status {:phase phase}})

(def ^:private locations {:items [(location "default" :default true) (location "offsite")]})

(defn- backup [& {:keys [name phase location uid] :or {name "nightly-20261120020000" phase "Completed" uid "u-1"}}]
  {:metadata {:namespace "velero" :name name :uid uid}
   :spec (cond-> {} location (assoc :storageLocation location))
   :status (cond-> {} phase (assoc :phase phase))})

(deftest picks
  (is (= {:namespace "velero" :name "nightly"} (actions/parse-pick " velero/nightly ")))
  (doseq [bad [nil "" "nightly" "a/b/c" "velero/nightly extra"]]
    (is (nil? (actions/parse-pick bad)) (pr-str bad))))

(deftest locations-resolve-by-name-else-default
  (is (= "offsite" (:name (:metadata (actions/location-for locations "offsite")))))
  (is (= "default" (:name (:metadata (actions/location-for locations nil)))))
  (is (= "default" (:name (:metadata (actions/location-for locations "")))))
  (is (nil? (actions/location-for locations "gone")))
  (is (nil? (actions/location-for {:items [(location "a")]} nil)) "no default"))

;; --- pause and resume ------------------------------------------------------------

(deftest pausing-refuses-what-is-already-so
  (is (nil? (actions/pause-refusal (schedule) true)))
  (is (= "it is paused already" (actions/pause-refusal (schedule :paused true) true)))
  (is (nil? (actions/pause-refusal (schedule :paused true) false)))
  (is (= "it is not paused" (actions/pause-refusal (schedule) false)))
  (is (= "the Schedule no longer exists" (actions/pause-refusal nil true))))

(deftest pause-items-say-what-follows
  (is (= ["pause Schedule velero/nightly - no backup starts on its schedule 0 2 * * * until it is resumed"
          "the newest backup ages from now on, and the backup scan reports the schedule as paused"]
         (actions/pause-items (schedule) true)))
  (testing "resuming warns of the catch-up backup"
    (is (= ["resume Schedule velero/nightly - backups start again on its schedule 0 2 * * *"
            (str "Velero counts the next run from the last backup, so a run missed while it was paused starts a backup at once;"
                 " velero schedule unpause --skip-immediately resumes without it")]
           (actions/pause-items (schedule :paused true) false)))))

(deftest pause-patch-sets-spec-paused
  (is (= {:spec {:paused true}} (json/parse-string (actions/pause-patch true) true)))
  (is (= {:spec {:paused false}} (json/parse-string (actions/pause-patch false) true))))

;; --- backup now ------------------------------------------------------------------

(deftest backup-names-follow-the-cli
  (is (= "nightly-20261120093005" (actions/backup-name (schedule) (java.time.Instant/parse "2026-11-20T09:30:05Z")))))

(deftest backup-now-refusals
  (is (nil? (actions/backup-refusal (schedule) locations)))
  (is (= "the Schedule no longer exists" (actions/backup-refusal nil locations)))
  (is (= "the Schedule failed validation, so its template does not make a valid backup: invalid included namespace"
         (actions/backup-refusal (schedule :phase "FailedValidation" :errors ["invalid included namespace"]) locations)))
  (is (= "BackupStorageLocation gone does not exist"
         (actions/backup-refusal (schedule :template {:storageLocation "gone"}) locations)))
  (is (= "the template names no storage location and none is the default"
         (actions/backup-refusal (schedule) {:items [(location "offsite")]})))
  (is (= "BackupStorageLocation default is Unavailable, so the backup cannot be written"
         (actions/backup-refusal (schedule) {:items [(location "default" :default true :phase "Unavailable")]})))
  (is (= "BackupStorageLocation default is not yet checked, so the backup cannot be written"
         (actions/backup-refusal (schedule) {:items [(location "default" :default true :phase nil)]})))
  (is (nil? (actions/backup-refusal (schedule :paused true) locations)) "a paused schedule's template still works"))

(deftest backup-manifest-mirrors-from-schedule
  (testing "the template's spec, the schedule's labels, the schedule-name label"
    (let [manifest (actions/backup-manifest (schedule :labels {:team "shop"}
                                                      :annotations {:kubectl.kubernetes.io/last-applied-configuration "{}"
                                                                    :owner "ops"})
                                            "nightly-1")]
      (is (= {:apiVersion "velero.io/v1"
              :kind "Backup"
              :metadata {:namespace "velero" :name "nightly-1"
                         :labels {:team "shop" :velero.io/schedule-name "nightly"}
                         :annotations {:owner "ops"}}
              :spec {:includedNamespaces ["shop"] :ttl "720h0m0s"}}
             manifest))))
  (testing "template metadata wins over the schedule's"
    (let [manifest (actions/backup-manifest (schedule :labels {:team "shop"}
                                                      :template {:metadata {:labels {:tier "gold"} :annotations {:a "b"}}
                                                                 :ttl "24h"})
                                            "n")]
      (is (= {:tier "gold" :velero.io/schedule-name "nightly"} (-> manifest :metadata :labels)))
      (is (= {:a "b"} (-> manifest :metadata :annotations)))))
  (testing "no annotations, no annotations key"
    (is (not (contains? (:metadata (actions/backup-manifest (schedule) "n")) :annotations)))))

(deftest backup-items-describe-the-backup
  (is (= ["start Backup velero/nightly-1 from Schedule nightly's template"
          "it covers shop and writes to the default location"
          "it carries the label velero.io/schedule-name=nightly, so it counts as one of the schedule's backups and expires after 720h0m0s"]
         (actions/backup-items (schedule) "nightly-1")))
  (is (re-find #"covers every namespace and writes to offsite"
               (second (actions/backup-items (schedule :template {:includedNamespaces ["*"] :storageLocation "offsite"}) "n")))))

(deftest backup-outcomes
  (is (actions/backup-completed? (backup)))
  (is (not (actions/backup-completed? (backup :phase "InProgress"))))
  (is (nil? (actions/backup-signals (backup :phase "InProgress"))))
  (is (nil? (actions/backup-signals nil)))
  (is (= ["the Backup failed: found a backup with status \"InProgress\" during the server starting"]
         (actions/backup-signals (assoc-in (backup :phase "Failed") [:status :failureReason]
                                           "found a backup with status \"InProgress\" during the server starting"))))
  (is (= ["the Backup failed validation: a; b"]
         (actions/backup-signals (assoc-in (backup :phase "FailedValidation") [:status :validationErrors] ["a" "b"]))))
  (is (= ["the Backup partially failed with 2 errors and 1 warnings - velero backup logs names them"]
         (actions/backup-signals (update (backup :phase "PartiallyFailed") :status assoc :errors 2 :warnings 1)))))

;; --- delete ----------------------------------------------------------------------

(deftest delete-refusals
  (is (nil? (actions/delete-refusal (backup) locations)))
  (is (nil? (actions/delete-refusal (backup :phase "Failed") locations)) "a failed backup can be deleted")
  (is (= "the Backup no longer exists" (actions/delete-refusal nil locations)))
  (is (= "it is being deleted already" (actions/delete-refusal (backup :phase "Deleting") locations)))
  (doseq [phase ["New" "InProgress" "WaitingForPluginOperations" "Finalizing"]]
    (is (= (format "it is still running (%s) - wait for it to finish" phase) (actions/delete-refusal (backup :phase phase) locations))))
  (is (= "it is still running (New) - wait for it to finish" (actions/delete-refusal (backup :phase nil) locations)))
  (is (= "BackupStorageLocation gone does not exist, so Velero cannot remove the backup's data"
         (actions/delete-refusal (backup :location "gone") locations)))
  (is (= "BackupStorageLocation offsite is read-only"
         (actions/delete-refusal (backup :location "offsite") {:items [(location "offsite" :mode "ReadOnly")]})))
  (is (= "BackupStorageLocation offsite is Unavailable, so Velero cannot remove the backup's data"
         (actions/delete-refusal (backup :location "offsite") {:items [(location "offsite" :phase "Unavailable")]}))))

(deftest delete-items-name-what-goes
  (is (= ["delete Backup velero/nightly-20261120020000 - its files in BackupStorageLocation the default location, its volume snapshots, file-system backups and data-mover copies are removed"
          "nothing restores from it afterwards; this cannot be undone"]
         (actions/delete-items (backup) nil)))
  (is (= "its Restores are deleted with it: r1, r2"
         (last (actions/delete-items (backup) {:items [{:metadata {:name "r2"} :spec {:backupName "nightly-20261120020000"}}
                                                       {:metadata {:name "other"} :spec {:backupName "weekly"}}
                                                       {:metadata {:name "r1"} :spec {:backupName "nightly-20261120020000"}}]})))))

(deftest label-values-shorten-as-velero-shortens-them
  (is (= "short" (actions/valid-label-value "short")))
  (let [long-name (apply str (repeat 70 "a"))
        value (actions/valid-label-value long-name)]
    (is (= 63 (count value)))
    (is (str/starts-with? value (apply str (repeat 57 "a"))))
    (is (re-matches #"[0-9a-f]{6}" (subs value 57))))
  (is (= 63 (count (actions/valid-label-value (apply str (repeat 63 "b"))))) "63 fits as is"))

(deftest delete-request-mirrors-the-cli
  (is (= {:apiVersion "velero.io/v1"
          :kind "DeleteBackupRequest"
          :metadata {:namespace "velero"
                     :generateName "nightly-20261120020000-"
                     :labels {:velero.io/backup-name "nightly-20261120020000" :velero.io/backup-uid "u-1"}}
          :spec {:backupName "nightly-20261120020000"}}
         (actions/delete-request-manifest (backup)))))

(deftest delete-signals-read-only-this-backups-requests
  (let [request (fn [uid phase errors] {:metadata {:labels {:velero.io/backup-uid uid}} :status {:phase phase :errors errors}})]
    (is (= ["the DeleteBackupRequest failed: backup storage location offsite not found"]
           (actions/delete-signals {:items [(request "u-1" "Processed" ["backup storage location offsite not found"])
                                            (request "u-2" "Processed" ["other backup"])
                                            (request "u-1" "InProgress" ["not yet"])]}
                                   "u-1")))
    (is (= [] (actions/delete-signals {:items [(request "u-1" "Processed" nil)]} "u-1")))))

(deftest books-confirm-and-gate
  (is (true? (-> books/delete-backup :confirm :destructive?)) "deleting asks for the destructive confirmation")
  (is (not (-> books/backup :confirm :destructive?)))
  (is (= ["deletebackuprequests.velero.io"] (-> books/delete-backup :prerequisites :kubectl-can-create)))
  (is (= ["backups.velero.io"] (-> books/backup :prerequisites :kubectl-can-create)))
  (is (= ["schedules.velero.io"] (-> (books/schedule-pause true) :prerequisites :kubectl-can-patch))))
