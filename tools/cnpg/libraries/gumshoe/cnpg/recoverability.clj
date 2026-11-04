;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.recoverability
  "Whether a cluster can be restored, read from its spec and its backups.

   WAL archiving and a backup are not the same thing. Archiving ships the
   segments that carry changes since a base backup; without a base backup there
   is nothing for them to be applied to, so a cluster can report archiving
   healthy and still have no restore path at all.

   Two shapes configure a destination: barmanObjectStore under spec.backup, and
   a WAL archiver plugin under spec.plugins. A plugin that does not declare
   itself the WAL archiver is not read as one."
  (:require [gumshoe.kubectl :as kubectl]))

(def ^:private completed-phase "completed")

(defn destination?
  "Whether the spec names somewhere for a backup to go."
  [cluster]
  (boolean (or (get-in cluster [:spec :backup :barmanObjectStore])
               (some :isWALArchiver (get-in cluster [:spec :plugins])))))

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
