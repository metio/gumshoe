;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.csi
  "Detectives for the CSI layer: attachments that fail, PersistentVolumes that
   failed or were released, and nodes where the ceph-csi drivers are not
   registered."
  (:require [clojure.string :as str]
            [gumshoe.evidence :as evidence]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

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

(def ^:private delete-grace-minutes
  "How long a Released volume with the Delete policy may wait for its
   provisioner before the deletion counts as stalled."
  60)

(defn- released-minutes
  [volume now]
  (when-let [since (some-> volume :status :lastPhaseTransitionTime java.time.Instant/parse)]
    (when now (.toMinutes (java.time.Duration/between since ^java.time.Instant now)))))

(defn released-finding
  "A Released volume's finding. With the Delete policy the provisioner should
   remove it, so one that stays Released past the grace has a deletion that
   failed; with Retain (the default for volumes created by hand) the data is
   kept on purpose until someone decides."
  [volume now]
  (let [policy (or (-> volume :spec :persistentVolumeReclaimPolicy) "Retain")
        minutes (released-minutes volume now)
        since (-> volume :status :lastPhaseTransitionTime)
        base {:component (kubectl/name-of volume)}]
    (case policy
      "Delete"
      (when (or (nil? minutes) (>= minutes delete-grace-minutes))
        (onset/since
         (assoc base :severity :warning
                :summary (if minutes
                           (format "PersistentVolume with reclaim policy Delete stayed Released for %d minutes - deleting it failed" minutes)
                           "PersistentVolume with reclaim policy Delete stayed Released - deleting it failed")
                :hint (str (when-let [message (not-empty (-> volume :status :message))] (str message " - "))
                           "the provisioner could not delete the backing volume; its events and logs say why, and runbooks/kubernetes/volumes/delete_released.clj removes the object once the backend is clean"))
         since))

      (onset/since
       (assoc base :severity :info
              :summary "PersistentVolume is Released - its claim is gone but the data still exists"
              :hint (format "reclaim policy %s keeps the data until someone decides - delete it with runbooks/kubernetes/volumes/delete_released.clj once nobody needs it, or bind a new claim to it" policy))
       since))))

(defn detect-volume-problems
  [evidence]
  (let [volumes (kubectl/items-of (get evidence "persistentvolumes"))]
    (concat
     (for [volume volumes
           :when (= "Failed" (-> volume :status :phase))]
       (onset/since
        {:severity :critical
         :component (kubectl/name-of volume)
         :summary "PersistentVolume is Failed"
         :hint (-> volume :status :message)}
        (-> volume :status :lastPhaseTransitionTime)))
     (keep #(released-finding % (:now evidence))
           (filter #(= "Released" (-> % :status :phase)) volumes)))))

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

(def driver-registration-joins
  "The three reads the driver check joins: a driver is registered on a node
   through its CSINode. Without the CSINodes every driver would read as
   registered nowhere."
  ["csidrivers" "csinodes" "nodes"])

(defn detect-driver-registration
  "Ceph CSI drivers not registered on schedulable nodes. Nothing is judged
   when a joined read did not answer."
  [evidence]
  (let [drivers (when (evidence/joined? evidence driver-registration-joins)
                  (filter ceph-driver? (kubectl/names-of (get evidence "csidrivers"))))
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

(def orphaned-local-volume-joins
  "A local volume names its node, so the check needs both lists."
  ["persistentvolumes" "nodes"])

(defn detect-orphaned-local-volumes
  "Local volumes pinned to nodes that no longer exist. Nothing is judged
   when a joined read did not answer."
  [evidence]
  (let [node-names (set (kubectl/names-of (get evidence "nodes")))]
    ;; with no node evidence every hostname would look absent and every local
    ;; PV would be flagged - so an unanswered or empty node list means "can't
    ;; tell", not "all orphaned".
    (for [volume (when (and (evidence/joined? evidence orphaned-local-volume-joins) (seq node-names))
                   (kubectl/items-of (get evidence "persistentvolumes")))
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
    :requires driver-registration-joins
    :joins driver-registration-joins
    :detect detect-driver-registration}
   {:name "local-volumes"
    :description "Local volumes pinned to nodes that no longer exist"
    :requires orphaned-local-volume-joins
    :joins orphaned-local-volume-joins
    :detect detect-orphaned-local-volumes}])
