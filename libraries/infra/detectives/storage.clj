;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.detectives.storage
  "Detectives for storage: claims without a volume and claims nobody uses."
  (:require [infra.kubectl :as kubectl]))

(defn detect-storage-problems
  [evidence]
  (let [pvcs (get evidence "persistentvolumeclaims")
        pods (get evidence "pods")]
    (concat
     (for [pvc (kubectl/items-of pvcs)
           :when (= "Pending" (-> pvc :status :phase))]
       {:severity :critical
        :component (kubectl/namespace-name-of pvc)
        :summary "PersistentVolumeClaim is Pending"
        :hint "no volume was bound - check the storage class and the provisioner"})
     (for [unused (kubectl/unused-pvcs pvcs pods)]
       {:severity :info
        :component unused
        :summary "PersistentVolumeClaim is not used by any pod"
        :hint "delete with runbooks/kubernetes/volumes/delete_unused.clj if it is truly abandoned"}))))

(def detectives
  [{:name "storage"
    :description "PersistentVolumeClaims that are pending or unused"
    :requires ["persistentvolumeclaims" "pods"]
    :detect detect-storage-problems}])
