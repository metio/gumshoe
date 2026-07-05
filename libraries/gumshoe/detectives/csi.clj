;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.csi
  "Detectives for the CSI layer backed by ceph-csi: attachments that fail,
   volumes whose data is orphaned on the ceph cluster, and nodes where the
   ceph drivers are not registered."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(defn- ceph-driver?
  [driver-name]
  (str/ends-with? (str driver-name) "csi.ceph.com"))

(defn detect-attachment-problems
  [evidence]
  (for [attachment (kubectl/items-of (get evidence "volumeattachments"))
        :when (not (true? (-> attachment :status :attached)))
        :let [error (-> attachment :status :attachError :message)
              volume (-> attachment :spec :source :persistentVolumeName)]]
    {:severity (if error :critical :warning)
     :component (or volume (kubectl/name-of attachment))
     :summary (format "volume is not attached to node %s" (-> attachment :spec :nodeName))
     :hint error}))

(defn detect-volume-problems
  [evidence]
  (let [volumes (kubectl/items-of (get evidence "persistentvolumes"))]
    (concat
     (for [volume volumes
           :when (= "Failed" (-> volume :status :phase))]
       {:severity :critical
        :component (kubectl/name-of volume)
        :summary "PersistentVolume is Failed"
        :hint (-> volume :status :message)})
     (for [volume volumes
           :when (= "Released" (-> volume :status :phase))]
       {:severity :info
        :component (kubectl/name-of volume)
        :summary "PersistentVolume is Released - its claim is gone but the data still exists"
        :hint "reclaim or delete it, otherwise the ceph cluster keeps the image around forever"}))))

(defn expected-plugin-node?
  "Whether the ceph-csi nodeplugin is expected to run on a node. The nodeplugin
   is a DaemonSet; it stays off nodes carrying a NoSchedule or NoExecute taint it
   does not tolerate - which is exactly what the control-plane and the
   role-specific (gateway, ingress) nodes carry. Those nodes legitimately lack
   the driver, so they are not held to it here. A node with no such taint is a
   normal workload node where storage must work."
  [node]
  (not-any? #(contains? #{"NoSchedule" "NoExecute"} (:effect %))
            (-> node :spec :taints)))

(defn detect-driver-registration
  [evidence]
  (let [drivers (filter ceph-driver? (kubectl/names-of (get evidence "csidrivers")))
        csinodes (kubectl/items-of (get evidence "csinodes"))
        expected (set (map kubectl/name-of
                           (filter expected-plugin-node?
                                   (kubectl/items-of (get evidence "nodes")))))]
    (apply concat
           (for [driver drivers
                 :let [registered-on (set (for [csinode csinodes
                                                registered (-> csinode :spec :drivers)
                                                :when (= driver (:name registered))]
                                            (kubectl/name-of csinode)))
                       ;; only nodes where the plugin belongs but the driver is
                       ;; absent are a real problem - tainted role nodes are not
                       ;; expected to run it and are excluded above.
                       missing (sort (remove registered-on expected))]]
             (cond
               (and (seq expected) (empty? (filter registered-on expected)))
               [{:severity :critical
                 :component driver
                 :summary "no schedulable node has this ceph CSI driver registered"
                 :hint "the ceph-csi nodeplugin daemonset is not running anywhere - storage cannot attach"}]

               (seq missing)
               [{:severity :warning
                 :component driver
                 :summary (format "driver missing on schedulable node(s): %s" (str/join ", " missing))
                 :hint "the nodeplugin belongs on these nodes but its driver is not registered - check its pod there"}]

               :else
               [])))))

(defn- affinity-hostnames
  "The hostnames a volume's node affinity pins it to - local volumes (LVM,
   local-path) always carry one."
  [volume]
  (set (for [term (-> volume :spec :nodeAffinity :required :nodeSelectorTerms)
             expression (:matchExpressions term)
             :when (= "kubernetes.io/hostname" (:key expression))
             value (:values expression)]
         value)))

(defn detect-orphaned-local-volumes
  [evidence]
  (let [node-names (set (kubectl/names-of (get evidence "nodes")))]
    (for [volume (kubectl/items-of (get evidence "persistentvolumes"))
          :let [hostnames (affinity-hostnames volume)]
          :when (and (seq hostnames)
                     (not-any? node-names hostnames))]
      {:severity :critical
       :component (kubectl/name-of volume)
       :summary (format "local volume is pinned to missing node(s): %s" (str/join ", " (sort hostnames)))
       :hint "the node is gone - workloads using this volume can never schedule again"})))

(def detectives
  [{:name "csi-attachments"
    :description "VolumeAttachments that failed to attach"
    :requires ["volumeattachments"]
    :detect detect-attachment-problems}
   {:name "csi-volumes"
    :description "PersistentVolumes that are Failed or Released"
    :requires ["persistentvolumes"]
    :detect detect-volume-problems}
   {:name "csi-drivers"
    :description "Schedulable nodes missing the ceph CSI drivers registration"
    :requires ["csidrivers" "csinodes" "nodes"]
    :detect detect-driver-registration}
   {:name "local-volumes"
    :description "Local volumes pinned to nodes that no longer exist"
    :requires ["persistentvolumes" "nodes"]
    :detect detect-orphaned-local-volumes}])
