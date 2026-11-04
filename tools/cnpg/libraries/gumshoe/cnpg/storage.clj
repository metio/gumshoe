;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.storage
  "Comparing a cluster's requested volume size against the volumes it has.

   The operator does not enlarge a PVC. Raising storage.size on the Cluster is
   necessary and inert on its own: the operator reports that the PVC group needs
   enlarging and waits for a human to patch the requests. Until that happens the
   spec says one size, the volumes are another, and nothing in the cluster's
   phase or ready count says so.

   Two distinct states follow, and they need different answers. A request below
   the spec means nobody has patched it and nothing is in motion. A capacity
   below the request means the CSI driver is resizing, or has resized and is
   waiting on the filesystem."
  (:require [clojure.string :as str]
            [gumshoe.cnpg.instances :as inst]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.quantity :as quantity]))

(defn desired-bytes
  [cluster]
  (quantity/quantity->number (get-in cluster [:spec :storage :size])))

(defn requested-bytes
  [pvc]
  (quantity/quantity->number (get-in pvc [:spec :resources :requests :storage])))

(defn capacity-bytes
  [pvc]
  (quantity/quantity->number (get-in pvc [:status :capacity :storage])))

(defn cluster-pvcs
  "The volumes belonging to one cluster. The operator labels them with
   `cnpg.io/cluster`; the name prefix is accepted too, so a volume the operator
   adopted without the label is still compared rather than silently skipped."
  [pvcs namespace cluster]
  (filter #(and (= namespace (kubectl/namespace-of %))
                (or (= cluster (get (-> % :metadata :labels) inst/cluster-label))
                    (str/starts-with? (kubectl/name-of %) (str cluster "-"))))
          pvcs))

(defn assess-volume
  "One volume against the size the cluster asks for."
  [desired pvc]
  (let [requested (requested-bytes pvc)
        capacity (capacity-bytes pvc)
        row {:pvc (kubectl/name-of pvc)
             :desired desired :requested requested :capacity capacity}]
    (cond
      (not (and desired requested)) (assoc row :state :unknown)
      (> desired requested) (assoc row :state :unrequested)
      (and capacity (> requested capacity)) (assoc row :state :resizing)
      :else (assoc row :state :matched))))

(defn assess
  [cluster pvcs]
  (let [desired (desired-bytes cluster)
        namespace (kubectl/namespace-of cluster)
        name (kubectl/name-of cluster)]
    (map #(assess-volume desired %) (cluster-pvcs pvcs namespace name))))

(defn pending?
  [row]
  (contains? #{:unrequested :resizing} (:state row)))
