;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.prometheus
  "Prometheus-operator specifics for storage resizes; the generic planning
   and validation lives in gumshoe.storage."
  (:require [gumshoe.kubectl :as kubectl]))

(defn volume-name-of
  [prometheus]
  (-> prometheus :spec :storage :volumeClaimTemplate :metadata :name))

(defn pvc-of-pod
  [pod volume-name]
  (->> (-> pod :spec :volumes)
       (filter #(= volume-name (:name %)))
       first
       :persistentVolumeClaim
       :claimName))

(defn resize-plan
  "One entry per prometheus pod: which PVC changes from what to what, and
   whether its storage class allows it."
  [{:keys [pods volume-name pvcs storage-classes capacity]}]
  (vec
   (for [pod (sort-by kubectl/name-of (kubectl/items-of pods))
         :let [pvc-name (pvc-of-pod pod volume-name)
               pvc (first (filter #(= pvc-name (kubectl/name-of %)) (kubectl/items-of pvcs)))
               class-name (-> pvc :spec :storageClassName)
               class (first (filter #(= class-name (kubectl/name-of %)) (kubectl/items-of storage-classes)))]
         :when pvc-name]
     {:pod (kubectl/name-of pod)
      :pvc pvc-name
      :exists? (some? pvc)
      :storage-class class-name
      :resizable? (true? (:allowVolumeExpansion class))
      :current (-> pvc :spec :resources :requests :storage)
      :target capacity})))
