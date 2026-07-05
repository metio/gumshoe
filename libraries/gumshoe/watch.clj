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
            [gumshoe.config :as config]
            [gumshoe.kubectl :as kubectl]))

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

(defonce ^:private resize-watcher-builders (atom []))

(defn register-resize-watcher!
  "Registers an extra watcher for a storage resize: (fn [signals] -> a
   zero-arg watcher fn, or nil to contribute nothing). signals is
   {:kubernetes-cluster cluster}, so a builder resolves per-environment config.
   A tool package tails its own log during a resize this way - the ceph package
   tails the ceph cluster log for the active cluster's mgr host, gated on its VPN."
  [builder]
  (swap! resize-watcher-builders conj builder))

(defn resize-watchers
  "The watcher set for a storage resize: the namespace's Warning events and the
   PVCs' own resize conditions always, plus any a tool package registered (the
   ceph package tails the ceph cluster log). On a machine with no such package
   this is just the two cluster-side watchers."
  [context cluster namespace pvc-names]
  (let [signals {:kubernetes-cluster cluster}]
    (combine
     (concat
      [(namespace-warning-events context namespace)
       (pvc-resize-conditions context namespace pvc-names)]
      (keep #(% signals) @resize-watcher-builders)))))
