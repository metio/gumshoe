;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.storage
  "Comparing a cluster's requested volume size against the volumes it has.

   The operator patches every claim's request up to the Cluster's size itself
   unless spec.storage.resizeInUseVolumes is false (CloudNativePG's
   ShouldResizeInUseVolumes reads that one flag for every volume role). Until
   the volumes follow, the spec says one size, the volumes are another, and
   nothing in the cluster's phase or ready count says so.

   Three states follow, and they need different answers. A request below the
   spec with resizing on means the operator's patch failed - typically a
   StorageClass without allowVolumeExpansion, which refuses a hand patch as
   well. A request below the spec with resizing off is the documented
   re-create procedure: the old claims stay small on purpose until each
   instance is re-created. A capacity below the request means the CSI driver
   is resizing, or has resized and is waiting on the filesystem."
  (:require [gumshoe.cnpg.instances :as inst]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.quantity :as quantity]))

(def ^:private role-label :cnpg.io/pvcRole)
(def ^:private tablespace-label :cnpg.io/tablespaceName)

(defn size-of
  "The size a storage section (spec.storage, spec.walStorage or a
   tablespace's storage) asks for, in bytes, as CloudNativePG's
   StorageConfiguration.GetSizeOrNil reads it: size when it is not empty -
   nil when it does not parse, with no fallback - and otherwise the storage request of
   pvcTemplate. A section with neither, or a template without a storage
   request (which GetSizeOrNil reads as zero), has no size here: nil, so the
   volume is judged unknown rather than compared against zero."
  [section]
  (let [size (:size section)]
    (if (seq (str size))
      (quantity/quantity->number size)
      (let [requested (quantity/quantity->number (get-in section [:pvcTemplate :resources :requests :storage]))]
        (when (and requested (pos? requested)) requested)))))

(defn desired-bytes
  "The size the cluster asks for a volume of the pvc's role (size-of): the
   data storage, walStorage for a WAL volume, and the tablespace's own
   storage for a tablespace volume. With one argument, the data size."
  ([cluster]
   (size-of (get-in cluster [:spec :storage])))
  ([cluster pvc]
   (let [labels (-> pvc :metadata :labels)]
     (case (get labels role-label)
       "PG_WAL" (size-of (get-in cluster [:spec :walStorage]))
       "PG_TABLESPACE" (some #(when (= (get labels tablespace-label) (:name %))
                                (size-of (:storage %)))
                             (get-in cluster [:spec :tablespaces]))
       (desired-bytes cluster)))))

(defn requested-bytes
  [pvc]
  (quantity/quantity->number (get-in pvc [:spec :resources :requests :storage])))

(defn capacity-bytes
  [pvc]
  (quantity/quantity->number (get-in pvc [:status :capacity :storage])))

(defn- operator-name?
  "Whether a volume name is one the operator gives the cluster's instances:
   <cluster>-<n>, its -wal volume, or a -tbs-<tablespace> volume."
  [cluster volume-name]
  (boolean (re-matches (re-pattern (str (java.util.regex.Pattern/quote cluster) "-\\d+(-wal|-tbs-.+)?"))
                       (str volume-name))))

(defn cluster-pvcs
  "The volumes belonging to one cluster. The operator labels them with
   `cnpg.io/cluster`; an unlabelled volume named the way the operator names
   the cluster's volumes is accepted too, so one it adopted without the label
   is still compared rather than silently skipped. A volume labelled for
   another cluster is that cluster's, whatever its name."
  [pvcs namespace cluster]
  (filter #(and (= namespace (kubectl/namespace-of %))
                (let [owner (get (-> % :metadata :labels) inst/cluster-label)]
                  (if owner
                    (= cluster owner)
                    (operator-name? cluster (kubectl/name-of %)))))
          pvcs))

(defn resizes-in-use-volumes?
  "Whether the operator enlarges existing claims: true unless
   spec.storage.resizeInUseVolumes is false, as the field defaults to true."
  [cluster]
  (not (false? (get-in cluster [:spec :storage :resizeInUseVolumes]))))

(defn assess-volume
  "One volume against the size the cluster asks for. With `resizes?` false a
   request below the spec is :awaiting-recreate rather than :unrequested."
  ([desired pvc] (assess-volume desired pvc true))
  ([desired pvc resizes?]
   (let [requested (requested-bytes pvc)
         capacity (capacity-bytes pvc)
         row {:pvc (kubectl/name-of pvc)
              :desired desired :requested requested :capacity capacity}]
     (cond
       (not (and desired requested)) (assoc row :state :unknown)
       (> desired requested) (assoc row :state (if resizes? :unrequested :awaiting-recreate))
       (and capacity (> requested capacity)) (assoc row :state :resizing)
       :else (assoc row :state :matched)))))

(defn assess
  [cluster pvcs]
  (let [namespace (kubectl/namespace-of cluster)
        name (kubectl/name-of cluster)
        resizes? (resizes-in-use-volumes? cluster)]
    (map #(assess-volume (desired-bytes cluster %) % resizes?)
         (cluster-pvcs pvcs namespace name))))

(defn pending?
  [row]
  (contains? #{:unrequested :awaiting-recreate :resizing} (:state row)))
