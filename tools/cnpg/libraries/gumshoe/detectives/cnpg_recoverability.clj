;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.cnpg-recoverability
  "A detective for a cluster with no restore path.

   The backup detective reports a backup that failed. A cluster that has never
   attempted one looks clean there, and so does a cluster with no destination
   configured at all - the condition that reports WAL archiving healthy says
   nothing is failing, which is not the same as there being something to
   restore."
  (:require [gumshoe.cnpg.recoverability :as recoverability]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.evidence :as evidence]
            [gumshoe.kubectl :as kubectl]))

(defn- finding
  [cluster {:keys [state attempts]}]
  (merge
   {:severity :warning
    :component (kubectl/namespace-name-of cluster)}
   (case state
     :unconfigured
     {:summary "no backup destination is configured, so the cluster cannot be restored"
      :hint (str "healthy WAL archiving reports nothing failing, not a restore path: "
                 "archived segments apply to a base backup, and without one there is "
                 "nothing to apply them to")}
     :never-completed
     {:summary (format "a backup destination is configured but no backup has completed (%d attempted)"
                       attempts)
      :hint "take one and confirm it reaches the destination, before the cluster needs it"})))

(def recoverability-joins
  "The two reads the check joins: without the Backups every cluster with a
   destination would read as never having completed one."
  [cnpg/cluster-type cnpg/backup-type])

(defn detect-unrecoverable-clusters
  "Clusters with no backup destination, or one no backup has completed to.
   Nothing is judged when either read did not answer."
  [evidence]
  (let [backups (kubectl/items-of (get evidence cnpg/backup-type))]
    (for [cluster (when (evidence/joined? evidence recoverability-joins)
                    (kubectl/items-of (get evidence cnpg/cluster-type)))
          :let [assessment (recoverability/assess cluster backups)]
          :when (recoverability/exposed? assessment)]
      (finding cluster assessment))))

(def detectives
  [{:name "cnpg-recoverability"
    :description "Clusters with no backup to restore from"
    :requires recoverability-joins
    :joins recoverability-joins
    :detect detect-unrecoverable-clusters}])
