;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.cnpg-storage
  "A detective for a volume enlargement that was asked for and never happened.

   The operator patches each claim's request to the Cluster's size itself
   unless spec.storage.resizeInUseVolumes is false. A request that stays below
   the spec while resizing is on means that patch failed, and the cluster can
   sit indefinitely with a spec that says one size and volumes that are
   another - while the instance it cannot start for want of room keeps
   restarting."
  (:require [gumshoe.cnpg.retention :as retention]
            [gumshoe.cnpg.storage :as storage]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.evidence :as evidence]
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
      :hint (str "the operator patches the claim's request itself while resizeInUseVolumes "
                 "is unset or true, so a request still below the spec means its patch failed: "
                 "read the operator log, and check that the StorageClass sets "
                 "allowVolumeExpansion - a class without it refuses a hand patch as well")}
     :awaiting-recreate
     {:severity :info
      :summary (format (str "the cluster asks for %s and this volume requests %s, and "
                            "resizeInUseVolumes is false")
                       (size desired) (size requested))
      :hint (str "the operator leaves existing claims alone, which is CloudNativePG's re-create "
                 "procedure: one instance at a time on a cluster of more than one instance, "
                 "delete the instance's claims (data, and -wal when present) and its pod, and "
                 "the operator re-creates it at the new size")}
     :unknown
     {:severity :info
      :summary (if desired
                 (format "the cluster asks for %s and this volume's request does not parse" (size desired))
                 "the cluster declares no size this volume can be compared against")
      :hint (str "the size comes from size of the volume's storage section (storage, walStorage "
                 "or the tablespace's storage), else from its pvcTemplate.resources.requests.storage, "
                 "as the operator reads it; with neither readable the volume was not judged")}
     :resizing
     {:severity :warning
      :summary (format "this volume requests %s and has %s"
                       (size requested) (size capacity))
      :hint (str "the driver is expanding it; a volume whose filesystem needs the pod "
                 "restarted stops here until the instance restarts")})))

(def pending-storage-joins
  "The size each cluster asks for against what its claims request."
  [cnpg/cluster-type "persistentvolumeclaims"])

(defn detect-pending-storage
  "Volumes requesting less than their cluster asks for, or still being
   expanded, and volumes whose sizes could not be read, which are not
   judged. Nothing is judged when a joined read did not answer."
  [evidence]
  (let [pvcs (kubectl/items-of (get evidence "persistentvolumeclaims"))]
    (for [cluster (when (evidence/joined? evidence pending-storage-joins)
                    (kubectl/items-of (get evidence cnpg/cluster-type)))
          row (storage/assess cluster pvcs)
          :when (or (storage/pending? row) (= :unknown (:state row)))]
      (finding cluster row))))

(def detectives
  [{:name "cnpg-storage-pending"
    :description "Clusters asking for more volume than their claims request"
    :requires pending-storage-joins
    :joins pending-storage-joins
    :detect detect-pending-storage}])
