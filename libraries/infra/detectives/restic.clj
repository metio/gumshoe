;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.detectives.restic
  "Detective for restic backups: repositories that can not be read, that hold
   no snapshots at all, or whose newest snapshot for a backup target has gone
   stale - the silent way backups stop."
  (:require [infra.restic :as restic]))

(defn- age-days
  [now instant]
  (when instant
    (.toDays (java.time.Duration/between instant now))))

(defn detect-backup-problems
  [evidence]
  (let [now (:now evidence)
        warn (:warn-days evidence)
        critical (:critical-days evidence)]
    (apply concat
           (for [[repository data] (sort-by first (get evidence "repositories"))]
             (cond
               (not (:reachable data))
               [{:severity :critical
                 :component repository
                 :summary "repository can not be read"
                 :hint (:error data)}]

               (empty? (:snapshots data))
               [{:severity :critical
                 :component repository
                 :summary "repository has no snapshots"
                 :hint "nothing has ever been backed up here, or the snapshots were pruned away"}]

               :else
               (for [{:keys [hostname paths latest]} (restic/latest-per-target (:snapshots data))
                     :let [days (age-days now latest)]
                     :when (or (nil? days) (>= days warn))]
                 (cond
                   (nil? days)
                   {:severity :warning
                    :component (format "%s %s %s" repository hostname paths)
                    :summary "no readable snapshot timestamp"
                    :hint "the target has snapshots but none with a parseable time"}

                   (>= days critical)
                   {:severity :critical
                    :component (format "%s %s %s" repository hostname paths)
                    :summary (format "newest backup is %d days old" days)
                    :hint (format "backups for this target stopped landing - the limit is %d days" critical)}

                   :else
                   {:severity :warning
                    :component (format "%s %s %s" repository hostname paths)
                    :summary (format "newest backup is %d days old" days)
                    :hint "backups are falling behind their schedule"})))))))

(def detectives
  [{:name "restic-backups"
    :description "Restic repositories readable, populated, and backing up on schedule"
    :requires ["repositories"]
    :detect detect-backup-problems}])
