;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.storage
  "Pure planning and validation for volume enlargements - shared by every
   book that grows PVCs, whatever controller owns them. All the ways an
   enlargement can go wrong are caught before anything touches the cluster."
  (:require [infra.kubectl :as kubectl]))

(defn normalize-capacity
  "A bare number means Gi: '100' -> '100Gi'."
  [capacity]
  (if (re-matches #"\d+" (str capacity))
    (str capacity "Gi")
    (str capacity)))

(defn gi-value
  [capacity]
  (some-> (re-matches #"(\d+)Gi" (str capacity)) second parse-long))

(defn statefulset-pvc-names
  "The PVC names a StatefulSet's volume claim template creates:
   <template>-<statefulset>-<ordinal>."
  [statefulset template-name]
  (let [replicas (or (-> statefulset :spec :replicas) 1)]
    (vec (for [ordinal (range replicas)]
           (format "%s-%s-%d" template-name (kubectl/name-of statefulset) ordinal)))))

(defn expansion-plan
  "One entry per PVC: what changes from what to what, and whether its
   storage class allows it."
  [{:keys [pvc-names pvcs storage-classes capacity]}]
  (vec (for [pvc-name pvc-names
             :let [pvc (first (filter #(= pvc-name (kubectl/name-of %)) (kubectl/items-of pvcs)))
                   class-name (-> pvc :spec :storageClassName)
                   class (first (filter #(= class-name (kubectl/name-of %)) (kubectl/items-of storage-classes)))]]
         {:pvc pvc-name
          :exists? (some? pvc)
          :storage-class class-name
          :resizable? (true? (:allowVolumeExpansion class))
          :current (-> pvc :spec :resources :requests :storage)
          :target capacity})))

(defn expansion-problems
  "Every reason this plan must not run. An empty result means it is safe."
  [plan]
  (vec
   (concat
    (when (empty? plan)
      ["no volumes to enlarge found"])
    (for [{:keys [pvc exists?]} plan
          :when (false? exists?)]
      (format "PVC %s does not exist" pvc))
    (for [{:keys [pvc storage-class resizable? exists?]} plan
          :when (and (not (false? exists?)) (not resizable?))]
      (format "storage class %s of PVC %s does not allow volume expansion" storage-class pvc))
    (for [{:keys [pvc current target exists?]} plan
          :when (and (not (false? exists?))
                     (gi-value current) (gi-value target)
                     (< (gi-value target) (gi-value current)))]
      (format "PVC %s already has %s - shrinking to %s is not possible" pvc current target))
    (for [{:keys [pvc current target exists?]} plan
          :when (and (not (false? exists?))
                     (or (nil? (gi-value current)) (nil? (gi-value target))))]
      (format "PVC %s: cannot compare '%s' with '%s' - only Gi capacities are supported"
              pvc current target)))))

(defn recreate-manifest
  "A StatefulSet manifest ready for re-apply after an orphan-cascade delete:
   server-managed fields are dropped and the volume claim template carries
   the new capacity, so the recreated StatefulSet matches its PVCs again."
  [statefulset template-name capacity]
  (-> statefulset
      (dissoc :status)
      (update :metadata dissoc :uid :resourceVersion :creationTimestamp :generation :managedFields :selfLink)
      (update-in [:metadata :annotations] dissoc (keyword "kubectl.kubernetes.io/last-applied-configuration"))
      (update-in [:spec :volumeClaimTemplates]
                 (fn [templates]
                   (mapv (fn [template]
                           (if (= template-name (-> template :metadata :name))
                             (assoc-in template [:spec :resources :requests :storage] capacity)
                             template))
                         templates)))))
