;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.connections
  "Who holds each CloudNativePG cluster's connections, read on its primary.

   pg_stat_activity is not in the resource API, so this collector runs
   gumshoe.postgres's constant read-only statement in the primary's postgres
   container with kubectl exec. The primary is the pod the Cluster names in
   status.currentPrimary, or, when it names none, the pod of that cluster
   carrying cnpg.io/instanceRole=primary. A pod that answers from recovery
   was not the primary when it answered.

   The evidence is {:items <one row per cluster read> :unread <one row per
   cluster not read, with its :reason>}, so a cluster that could not be read
   is named and never reads as a cluster without connections. A failed
   Cluster read is no evidence at all."
  (:require [gumshoe.cnpg.instances :as inst]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.postgres :as postgres]))

(def evidence-key "cnpg-connections")

(def cluster-type "clusters.postgresql.cnpg.io")

(def container
  "The container CloudNativePG runs PostgreSQL in."
  "postgres")

(def instance-role-label :cnpg.io/instanceRole)

(def ^:private primary-selector "cnpg.io/instanceRole=primary")

(defn primary-pod
  "The name of the cluster's primary pod: status.currentPrimary, else the one
   pod of the cluster labelled primary among labelled-pods, else nil."
  [cluster labelled-pods]
  (or (not-empty (-> cluster :status :currentPrimary))
      (let [namespace (kubectl/namespace-of cluster)
            name (kubectl/name-of cluster)
            labelled (filter #(and (= namespace (kubectl/namespace-of %))
                                   (= name (inst/cluster-of %))
                                   (= "primary" (get (inst/labels-of %) instance-role-label)))
                             labelled-pods)]
        (when (= 1 (count labelled))
          (kubectl/name-of (first labelled))))))

(defn classify
  "Rows of {:namespace :cluster :pod :reading} or {:namespace :cluster
   :reason}, split into the clusters read and those not read. A missing
   reading is :no-answer and a reading from a standby is :in-recovery."
  [rows]
  (let [reason (fn [{:keys [reading] :as row}]
                 (cond (:reason row) (:reason row)
                       (nil? reading) :no-answer
                       (:in-recovery? reading) :in-recovery))
        {read false unread true} (group-by #(boolean (reason %)) rows)]
    {:items (vec read)
     :unread (mapv #(-> % (assoc :reason (reason %)) (dissoc :reading)) unread)}))

(defn collect
  "The evidence for clusters, a kubectl list document or nil when its read
   failed. read-labelled returns the pods labelled primary, or nil when that
   read failed, and is called at most once, only for a cluster naming no
   current primary. (read namespace pod) is the connection reading or nil.
   The primaries are read in parallel."
  [clusters read-labelled read]
  (when clusters
    (let [labelled (delay (read-labelled))
          targets (for [cluster (kubectl/items-of clusters)
                        :let [row {:namespace (kubectl/namespace-of cluster) :cluster (kubectl/name-of cluster)}
                              named (not-empty (-> cluster :status :currentPrimary))]]
                    (cond
                      named (assoc row :pod named)
                      (nil? (force labelled)) (assoc row :reason :primary-unread)
                      :else (if-let [pod (primary-pod cluster (kubectl/items-of (force labelled)))]
                              (assoc row :pod pod)
                              (assoc row :reason :no-primary))))]
      (classify (vec (pmap (fn [{:keys [namespace pod reason] :as row}]
                             (if reason row (assoc row :reading (read namespace pod))))
                           (vec targets)))))))

(defn collector
  "The evidence collector, for registration through the plugin seam."
  [context _key]
  (collect (kubectl/get-all context cluster-type)
           #(kubectl/get-selected context "pods" primary-selector)
           (fn [namespace pod] (postgres/connection-reading context namespace pod container))))
