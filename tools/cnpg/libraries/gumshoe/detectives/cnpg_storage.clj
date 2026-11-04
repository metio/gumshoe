;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.cnpg-storage
  "A detective for a volume enlargement that was asked for and never happened.

   The operator does not resize a PVC. It reports that the PVC group needs
   enlarging and waits, so a cluster can sit indefinitely with a spec that says
   one size and volumes that are another - while the instance it cannot start
   for want of room keeps restarting."
  (:require [gumshoe.cnpg.retention :as retention]
            [gumshoe.cnpg.storage :as storage]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.kubectl :as kubectl]))

(defn- size [bytes] (if bytes (retention/describe-bytes bytes) "unknown"))

(defn- finding
  [cluster {:keys [pvc state desired requested capacity]}]
  (merge
   {:component (format "%s/%s" (kubectl/namespace-of cluster) pvc)}
   (case state
     :unrequested
     {:severity :critical
      :summary (format "the cluster asks for %s and this volume requests %s"
                       (size desired) (size requested))
      :hint (str "the operator does not enlarge a volume: patch the claim's "
                 "spec.resources.requests.storage to match, on every volume of the "
                 "cluster, or postgres keeps running out of room at the old size")}
     :resizing
     {:severity :warning
      :summary (format "this volume requests %s and has %s"
                       (size requested) (size capacity))
      :hint (str "the driver is expanding it; a volume whose filesystem needs the pod "
                 "restarted stops here until the instance restarts")})))

(defn detect-pending-storage
  [evidence]
  (let [pvcs (kubectl/items-of (get evidence "persistentvolumeclaims"))]
    (for [cluster (kubectl/items-of (get evidence cnpg/cluster-type))
          row (storage/assess cluster pvcs)
          :when (storage/pending? row)]
      (finding cluster row))))

(def detectives
  [{:name "cnpg-storage-pending"
    :description "Clusters asking for more volume than their claims request"
    :requires [cnpg/cluster-type "persistentvolumeclaims"]
    :detect detect-pending-storage}])
