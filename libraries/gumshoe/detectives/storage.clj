;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.storage
  "Detectives for storage: claims without a volume, claims that lost theirs,
   resizes and modifications that failed, and claims nobody uses."
  (:require [clojure.string :as str]
            [gumshoe.evidence :as evidence]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def ^:private pending-grace-minutes 5)

(defn- binding-modes
  [storageclasses]
  (into {} (for [class (kubectl/items-of storageclasses)]
             [(kubectl/name-of class) (or (:volumeBindingMode class) "Immediate")])))

(defn- scheduled-consumers
  "The pods bound to a node that mount the claim. A claim waiting for its
   first consumer binds only once such a pod exists."
  [pods claim]
  (for [pod (kubectl/items-of pods)
        :when (and (= (kubectl/namespace-of claim) (kubectl/namespace-of pod))
                   (-> pod :spec :nodeName)
                   (some #(= (kubectl/name-of claim) (-> % :persistentVolumeClaim :claimName)) (-> pod :spec :volumes)))]
    (kubectl/name-of pod)))

(defn- latest-provisioning-failure
  [events claim]
  (->> (kubectl/items-of events)
       (filter #(and (= "ProvisioningFailed" (:reason %))
                     (= "PersistentVolumeClaim" (-> % :involvedObject :kind))
                     (= (kubectl/namespace-of claim) (-> % :involvedObject :namespace))
                     (= (kubectl/name-of claim) (-> % :involvedObject :name))))
       (sort-by #(str (or (:lastTimestamp %) (:eventTime %))))
       last
       :message))

(defn- young?
  [claim now]
  (when-let [created (some-> claim :metadata :creationTimestamp java.time.Instant/parse)]
    (and now (.isAfter created (.minus ^java.time.Instant now (java.time.Duration/ofMinutes pending-grace-minutes))))))

(defn- pending-finding
  [claim {:keys [modes pods events now]}]
  (let [class (-> claim :spec :storageClassName)
        component (kubectl/namespace-name-of claim)]
    (cond
      (young? claim now) nil

      (and (= "WaitForFirstConsumer" (get modes class)) (empty? (scheduled-consumers pods claim)))
      {:severity :info
       :component component
       :summary (format "PersistentVolumeClaim waits for its first consumer (StorageClass %s)" class)
       :hint "it binds once a pod that mounts it is scheduled - nothing is wrong unless that pod should exist"}

      :else
      (onset/since
       {:severity :critical
        :component component
        :summary "PersistentVolumeClaim is Pending"
        :hint (or (some->> (latest-provisioning-failure events claim) (str "the provisioner reports: "))
                  "no volume was bound - check the storage class and the provisioner")}
       (-> claim :metadata :creationTimestamp)))))

(def ^:private failure-conditions
  {"ControllerResizeError" "the volume could not be expanded by the storage backend"
   "NodeResizeError" "the filesystem could not be expanded on the node"
   "ModifyVolumeError" "the volume could not be modified to its VolumeAttributesClass"})

(defn- condition-findings
  [claim]
  (let [component (kubectl/namespace-name-of claim)
        active (filter #(= "True" (:status %)) (-> claim :status :conditions))]
    (concat
     (for [{:keys [type message lastTransitionTime]} active
           :let [explain (failure-conditions type)]
           :when explain]
       (onset/since
        {:severity :warning
         :component component
         :summary (format "PersistentVolumeClaim %s: %s" type explain)
         :hint (str/join " - " (remove str/blank? [(str message) "the volume keeps working at its current size or class"]))}
        lastTransitionTime))
     (for [[resource status] (-> claim :status :allocatedResourceStatuses)
           :when (contains? #{"ControllerResizeInfeasible" "NodeResizeInfeasible"} status)]
       {:severity :warning
        :component component
        :summary (format "PersistentVolumeClaim resize of %s is infeasible (%s)" (subs (str resource) 1) status)
        :hint "the backend refuses the requested size - lower spec.resources.requests to a size it accepts"})
     (for [{:keys [type]} active
           :when (= "FileSystemResizePending" type)]
       {:severity :info
        :component component
        :summary "PersistentVolumeClaim waits for a pod to mount it to finish growing its filesystem"
        :hint "the volume is larger already; the filesystem grows the next time a pod mounts it"}))))

(def storage-joins
  "The reads the claim check joins: without the pods every claim would read
   as unused, and without the StorageClasses a claim waiting for its first
   consumer would read as stuck Pending. Events only add the provisioner's
   message to a hint, so they are required but not joined."
  ["persistentvolumeclaims" "pods" "storageclasses"])

(defn detect-storage-problems
  "Claims that are Pending, Lost, failed to resize or modify, or unused.
   Nothing is judged when a joined read did not answer."
  [evidence]
  (let [pvcs (when (evidence/joined? evidence storage-joins) (get evidence "persistentvolumeclaims"))
        pods (get evidence "pods")
        context {:modes (binding-modes (get evidence "storageclasses"))
                 :pods pods :events (get evidence "events") :now (:now evidence)}]
    (concat
     (keep #(pending-finding % context)
           (filter #(= "Pending" (-> % :status :phase)) (kubectl/items-of pvcs)))
     (for [claim (kubectl/items-of pvcs)
           :when (= "Lost" (-> claim :status :phase))]
       {:severity :critical
        :component (kubectl/namespace-name-of claim)
        :summary (if-let [volume (-> claim :spec :volumeName)]
                   (format "PersistentVolumeClaim lost its volume %s" volume)
                   "PersistentVolumeClaim lost its volume")
        :hint "the PersistentVolume it was bound to is gone - restore the volume from a backup or recreate the claim"})
     (mapcat condition-findings (kubectl/items-of pvcs))
     (for [unused (kubectl/unused-pvcs pvcs pods)]
       {:severity :info
        :component unused
        :summary "PersistentVolumeClaim is not used by any pod"
        :hint "delete with runbooks/kubernetes/volumes/delete_unused.clj if it is truly abandoned"}))))

(def detectives
  [{:name "storage"
    :description "PersistentVolumeClaims that are pending, lost, failed to resize, or unused"
    :requires (conj storage-joins "events")
    :joins storage-joins
    :detect detect-storage-problems}])
