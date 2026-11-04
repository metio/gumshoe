;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.volumes
  "Volume consumption for CloudNativePG instance pods.

   A PersistentVolumeClaim's `status.capacity` reports the size that was
   requested, never the size in use, so volume pressure is not visible anywhere
   in the resource API. The kubelet serves it per pod at
   `/api/v1/nodes/<node>/proxy/stats/summary`.

   The endpoint has been seen to omit a pod whose containers were not running,
   which is exactly the pod an operator is asking about. `collect` therefore
   returns the instance pods it looked for alongside the rows it found, so a pod
   with no row can be reported as unknown instead of passing for healthy."
  (:require [gumshoe.cnpg.instances :as inst]
            [gumshoe.kubectl :as kubectl]))

(def evidence-key "cnpg-volume-stats")

(def warning-fraction
  "Headroom thresholds, not measurements. WAL can grow faster than anyone
   reacts, and an instance that runs out does not degrade - it refuses to
   start."
  0.80)

(def critical-fraction 0.90)

(defn summary->volumes
  "One node's stats summary flattened to volume rows. A row without both byte
   counts, or with a zero capacity, carries no usable fraction and is dropped."
  [summary]
  (for [pod (:pods summary)
        volume (:volume pod)
        :let [used (:usedBytes volume)
              capacity (:capacityBytes volume)]
        :when (and (number? used) (number? capacity) (pos? capacity))]
    {:namespace (-> pod :podRef :namespace)
     :pod (-> pod :podRef :name)
     :volume (:name volume)
     :used-bytes used
     :capacity-bytes capacity}))

(defn fraction
  [{:keys [used-bytes capacity-bytes]}]
  (/ (double used-bytes) capacity-bytes))

(defn severity-for
  [row]
  (let [used (fraction row)]
    (cond (>= used critical-fraction) :critical
          (>= used warning-fraction) :warning
          :else nil)))

(defn describe-size
  [bytes]
  (format "%.1fGi" (/ (double bytes) 1073741824)))

(defn describe-row
  [{:keys [volume] :as row}]
  (format "%s is %d%% full (%s of %s)"
          volume
          (int (Math/round (* 100 (fraction row))))
          (describe-size (:used-bytes row))
          (describe-size (:capacity-bytes row))))

(defn collect
  "Volume rows for every node that hosts a CloudNativePG instance pod, plus the
   pods that were looked for. Both reads are injected so this is exercised
   without a cluster.

   Only nodes hosting an instance pod are queried: the stats endpoint is
   per-node, and a sweep of every node in a cluster would cost far more than the
   question is worth."
  [fetch-pods fetch-summary]
  (let [pods (filter inst/instance-pod? (kubectl/items-of (fetch-pods)))
        nodes (into (sorted-set) (keep #(-> % :spec :nodeName) pods))]
    {:instance-pods (mapv (fn [pod]
                            {:namespace (kubectl/namespace-of pod)
                             :pod (kubectl/name-of pod)
                             :node (-> pod :spec :nodeName)})
                          pods)
     :nodes (vec nodes)
     :volumes (vec (mapcat #(summary->volumes (fetch-summary %)) nodes))}))

(defn collector
  "The evidence collector, for registration through the plugin seam."
  [context _key]
  (collect #(kubectl/get-all context "pods")
           #(kubectl/node-stats-summary context %)))
