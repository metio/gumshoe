;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.cnpg
  "Detectives for CloudNativePG: unhealthy clusters, missing replicas, and
   failing WAL archiving."
  (:require [gumshoe.kubectl :as kubectl]))

(def cluster-type "clusters.postgresql.cnpg.io")
(def backup-type "backups.postgresql.cnpg.io")
(def scheduled-backup-type "scheduledbackups.postgresql.cnpg.io")

(def ^:private healthy-phase "Cluster in healthy state")

(defn- condition
  [resource type]
  (first (filter #(= type (:type %)) (-> resource :status :conditions))))

(defn detect-cnpg-problems
  [evidence]
  (let [clusters (kubectl/items-of (get evidence cluster-type))]
    (concat
     (for [cluster clusters
           :let [phase (-> cluster :status :phase)]
           :when (and phase (not= healthy-phase phase))]
       {:severity :warning
        :component (kubectl/namespace-name-of cluster)
        :summary (format "cluster phase: %s" phase)
        :hint (-> cluster :status :phaseReason)})
     (for [cluster clusters
           :let [instances (-> cluster :spec :instances)
                 ready (-> cluster :status :readyInstances)]
           :when (and instances ready (< ready instances))]
       {:severity :critical
        :component (kubectl/namespace-name-of cluster)
        :summary (format "only %d of %d instances are ready" ready instances)})
     (for [cluster clusters
           :let [archiving (condition cluster "ContinuousArchiving")]
           :when (= "False" (:status archiving))]
       {:severity :warning
        :component (kubectl/namespace-name-of cluster)
        :summary "continuous WAL archiving is failing"
        :hint (:message archiving)}))))

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
   {:name "cnpg-backups"
    :description "CloudNativePG backups that failed or are suspended"
    :requires [backup-type scheduled-backup-type]
    :detect detect-backup-problems}])
