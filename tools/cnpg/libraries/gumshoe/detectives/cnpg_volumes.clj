;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.cnpg-volumes
  "A detective for volume pressure on CloudNativePG instances.

   An instance that runs out of room does not slow down, it stops: the instance
   manager refuses to start postgres and the pod restarts for as long as the
   condition lasts. If that instance is the cluster's primary, nothing accepts
   writes, while the Cluster status still reports a ready count that looks
   survivable.

   A pod the kubelet did not report is reported as unknown. Silence there is
   missing data, and the pod most likely to be missing is the one whose
   containers are not running - the pod the operator is asking about."
  (:require [gumshoe.cnpg.volumes :as volumes]
            [gumshoe.kubectl :as kubectl]))

(defn- component [{:keys [namespace pod]}] (format "%s/%s" namespace pod))

(defn- instance-rows
  "Volume rows belonging to one of the instance pods, so a co-located pod on the
   same node is not reported by a cnpg detective."
  [{:keys [volumes instance-pods]}]
  (let [wanted (set (map (juxt :namespace :pod) instance-pods))]
    (filter #(contains? wanted ((juxt :namespace :pod) %)) volumes)))

(defn- unreported-pods
  [{:keys [volumes instance-pods]}]
  (let [seen (set (map (juxt :namespace :pod) volumes))]
    (remove #(contains? seen ((juxt :namespace :pod) %)) instance-pods)))

(defn detect-volume-pressure
  "Instance volumes at or past the headroom thresholds, and instance pods the
   kubelet reported nothing for."
  [evidence]
  (let [stats (get evidence volumes/evidence-key)]
    (concat
     (for [row (instance-rows stats)
           :let [severity (volumes/severity-for row)]
           :when severity]
       {:severity severity
        :component (component row)
        :summary (volumes/describe-row row)
        :hint (str "an instance with no room for WALs refuses to start, so this becomes "
                   "a crashloop rather than a slowdown; pg_wal can dominate the volume "
                   "when a replication slot is inactive, and raising the cluster's "
                   "storage.size is the fix that costs no data")})
     (for [pod (unreported-pods stats)]
       {:severity :warning
        :component (component pod)
        :summary "volume usage unknown - the kubelet reported none"
        :hint (format (str "node %s served no volume stats for this pod, which happens when its "
                           "containers are not running - the usage is unmeasured, not low")
                      (or (:node pod) "unknown"))}))))

(def detectives
  [{:name "cnpg-volumes"
    :description "Instance volumes running out of room, and instances with no usage reported"
    :requires [volumes/evidence-key]
    :detect detect-volume-pressure}])
