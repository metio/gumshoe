;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.snapshots
  "CSI VolumeSnapshots (snapshot.storage.k8s.io) that do not become usable. A
   restore from such a snapshot fails, and so does every backup built on it
   (Velero's CSI snapshots, CloudNativePG's volume-snapshot backups).

   The snapshot controller keeps retrying after an error and clears
   status.error on success, so an error present on a snapshot that is not
   ready is its current failure. A snapshot that has no VolumeSnapshotContent
   at all was never picked up: the snapshot controller is not running, or the
   VolumeSnapshotClass it names does not exist."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def snapshot-type "volumesnapshots.snapshot.storage.k8s.io")
(def class-type "volumesnapshotclasses.snapshot.storage.k8s.io")

(def unbound-grace
  "How long a new snapshot may go without a VolumeSnapshotContent."
  (java.time.Duration/ofMinutes 10))

(defn- instant
  [value]
  (try (java.time.Instant/parse (str value)) (catch Exception _ nil)))

(defn- source-of
  [snapshot]
  (let [{:keys [persistentVolumeClaimName volumeSnapshotContentName]} (-> snapshot :spec :source)]
    (cond
      persistentVolumeClaimName (str "PersistentVolumeClaim " persistentVolumeClaimName)
      volumeSnapshotContentName (str "VolumeSnapshotContent " volumeSnapshotContentName)
      :else "its source")))

(defn- error-finding
  [snapshot]
  (let [{:keys [message time]} (-> snapshot :status :error)]
    (onset/since
     {:severity :warning
      :component (kubectl/namespace-name-of snapshot)
      :summary "VolumeSnapshot is not ready: the snapshot controller reports an error"
      :hint (str (or message "no message") " - snapshot of " (source-of snapshot)
                 "; the controller retries, and restores or backups from it fail until it succeeds")}
     time)))

(defn- unbound-finding
  [snapshot classes now]
  (let [created (instant (-> snapshot :metadata :creationTimestamp))
        class (-> snapshot :spec :volumeSnapshotClassName)]
    (when (and created now (.isAfter ^java.time.Instant now (.plus ^java.time.Instant created unbound-grace)))
      (onset/since
       {:severity :warning
        :component (kubectl/namespace-name-of snapshot)
        :summary "VolumeSnapshot has no VolumeSnapshotContent"
        :hint (str "snapshot of " (source-of snapshot) " - "
                   (if (and class classes (not (contains? classes class)))
                     (format "its VolumeSnapshotClass %s does not exist" class)
                     "the snapshot controller has not picked it up; check that it runs and that the class names a CSI driver that is installed"))}
       (str created)))))

(defn detect-snapshot-problems
  [evidence]
  (let [classes (some->> (get evidence class-type) kubectl/items-of (into #{} (map kubectl/name-of)))
        now (get evidence :now)]
    (for [snapshot (kubectl/items-of (get evidence snapshot-type))
          :let [status (:status snapshot)]
          :when (and (not (true? (:readyToUse status)))
                     (not (-> snapshot :metadata :deletionTimestamp)))
          :let [finding (cond
                          (:error status) (error-finding snapshot)
                          (nil? (:boundVolumeSnapshotContentName status)) (unbound-finding snapshot classes now)
                          :else nil)]
          :when finding]
      finding)))

(def detectives
  [{:name "volume-snapshots"
    :description "VolumeSnapshots whose snapshot fails, or that never get a VolumeSnapshotContent"
    :requires [snapshot-type class-type]
    :detect detect-snapshot-problems}])
