;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.recoverability
  "Whether a cluster can be restored, read from its spec and its backups.

   WAL archiving and a backup are not the same thing. Archiving ships the
   segments that carry changes since a base backup; without a base backup there
   is nothing for them to be applied to, so a cluster can report archiving
   healthy and still have no restore path at all.

   Three shapes configure a destination: barmanObjectStore or volumeSnapshot
   under spec.backup, and a WAL archiver plugin under spec.plugins. A plugin
   counts only when it declares itself the WAL archiver and is not disabled,
   as CloudNativePG's GetEnabledWALArchivePluginName decides."
  (:require [gumshoe.kubectl :as kubectl]))

(def ^:private completed-phase "completed")

(defn wal-archiver-plugin?
  "A spec.plugins entry that archives WAL: isWALArchiver true and enabled not
   false (enabled defaults to true)."
  [plugin]
  (and (true? (:isWALArchiver plugin))
       (not (false? (:enabled plugin)))))

(defn archives-wal?
  "Whether the cluster ships WAL somewhere: a barmanObjectStore or an enabled
   WAL archiver plugin. A volumeSnapshot backup alone archives no WAL."
  [cluster]
  (boolean (or (get-in cluster [:spec :backup :barmanObjectStore])
               (some wal-archiver-plugin? (get-in cluster [:spec :plugins])))))

(defn destination?
  "Whether the spec names somewhere for a backup to go."
  [cluster]
  (boolean (or (archives-wal? cluster)
               (get-in cluster [:spec :backup :volumeSnapshot]))))

(defn backups-of
  [backups namespace cluster]
  (filter #(and (= namespace (kubectl/namespace-of %))
                (= cluster (get-in % [:spec :cluster :name])))
          backups))

(defn completed?
  [backup]
  (= completed-phase (get-in backup [:status :phase])))

(defn assess
  "One cluster as :recoverable, :unconfigured or :never-completed. The last of
   those is a destination with nothing in it, which reads as configured from
   the spec alone."
  [cluster backups]
  (let [own (backups-of backups (kubectl/namespace-of cluster) (kubectl/name-of cluster))]
    (cond
      (not (destination? cluster)) {:state :unconfigured}
      (some completed? own) {:state :recoverable :completed (count (filter completed? own))}
      :else {:state :never-completed :attempts (count own)})))

(defn exposed?
  [assessment]
  (contains? #{:unconfigured :never-completed} (:state assessment)))
