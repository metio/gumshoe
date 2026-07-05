;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.watch
  "Background watchers for post-checks. Each is a zero-argument function that,
   called once per poll interval, returns the diagnostic signals it sees right
   now; gumshoe.verify surfaces the new ones live. They read from the cluster, so
   while a book waits for a change to converge it is quietly watching the
   infrastructure react - a Warning event, a stuck resize - and tells the
   operator the moment something looks wrong."
  (:require [clojure.string :as str]
            [gumshoe.ceph :as ceph]
            [gumshoe.config :as config]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.net :as net]))

(defn- one-line
  [text]
  (str/replace (str text) #"\s+" " "))

(defn combine
  "Merges several watchers into one, concatenating their signals. nil watchers
   are ignored, so callers can conditionally include them."
  [watchers]
  (let [active (remove nil? watchers)]
    (when (seq active)
      (fn [] (mapcat #(% ) active)))))

(defn namespace-warning-events
  "Every Warning event in the namespace right now. This is where the
   kube-scheduler, the kubelet, and CSI's external-resizer all report trouble
   (FailedScheduling, VolumeResizeFailed, ...), so it catches a lot with one
   cheap read."
  [context namespace]
  (fn []
    (for [event (kubectl/items-of (kubectl/get-namespaced context namespace "events"))
          :when (= "Warning" (:type event))]
      (format "event %s on %s/%s: %s"
              (:reason event)
              (or (-> event :involvedObject :kind) "?")
              (or (-> event :involvedObject :name) "?")
              (one-line (:message event))))))

(defn pvc-resize-conditions
  "The resize-related conditions of the named PVCs - a direct read of how the
   storage layer is progressing (Resizing, FileSystemResizePending, or an
   error message from the controller)."
  [context namespace pvc-names]
  (fn []
    (for [pvc-name pvc-names
          :let [pvc (kubectl/get-namespaced-resource context namespace "persistentvolumeclaims" pvc-name)]
          condition (-> pvc :status :conditions)
          :when (contains? #{"Resizing" "FileSystemResizePending"} (:type condition))]
      (format "PVC %s: %s%s"
              pvc-name
              (:type condition)
              (if (str/blank? (str (:message condition)))
                ""
                (str " - " (one-line (:message condition))))))))

(defn ceph-cluster-log-errors
  "Warn-and-above lines from the ceph cluster log, over SSH to a mgr host -
   gated on the VPN interface being up, so it never reaches out to a private
   host off-network (where it would just time out). Returns nothing when the
   VPN is down."
  [connection vpn-interface]
  (fn []
    (when (net/interface-up? vpn-interface)
      (->> (str/split-lines (ceph/ceph-stdout connection "log" "last" "50" "warn" "cluster"))
           (remove str/blank?)
           (map #(str "ceph cluster log: " (str/trim %)))))))

(defn resize-watchers
  "The watcher set for a storage resize: the namespace's Warning events and the
   PVCs' own resize conditions always, plus - when env.edn names a ceph mgr
   host for the active cluster - the ceph cluster log over SSH, gated on that
   environment's VPN. Because the ceph host and VPN are resolved for the current
   cluster, resizing on the staging cluster tails staging's ceph, and production
   tails production's - the cluster you operate on selects the rest. On a machine
   with no env.edn this is just the two cluster-side watchers."
  [context cluster namespace pvc-names]
  (let [signals {:kubernetes-cluster cluster}]
    (combine
     [(namespace-warning-events context namespace)
      (pvc-resize-conditions context namespace pvc-names)
      (when-let [mgr-host (first (config/env-value signals [:ceph :mgr-hosts]))]
        (ceph-cluster-log-errors (ceph/connection {:host mgr-host})
                                 (config/env-value signals [:vpn :interface])))])))
