;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.ceph
  "The ceph tool package's plugin. The cephadm-over-SSH detectives and books live
   alongside this namespace (gumshoe.ceph, gumshoe.detectives.ceph, runbooks/,
   playbooks/); loading this plugin contributes ceph's one cross-cutting piece:
   the ceph cluster-log tailer for storage-resize watches. Storage resize is a
   generic CSI operation that stays in the engine - this is one storage provider's
   observability, added only when the ceph package is loaded. When a resize runs
   on a cluster whose env.edn names a ceph mgr host, warn-and-above lines from the
   ceph cluster log surface live alongside the PVC's own conditions, gated on the
   VPN so it never reaches a private host off-network."
  (:require [clojure.string :as str]
            [gumshoe.ceph :as ceph]
            [gumshoe.config :as config]
            [gumshoe.net :as net]
            [gumshoe.plugin :as plugin]))

(defn ceph-cluster-log-errors
  "A watcher: warn-and-above lines from the ceph cluster log over SSH, or nothing
   when the VPN is down."
  [connection vpn-interface]
  (fn []
    (when (net/interface-up? vpn-interface)
      (->> (str/split-lines (ceph/ceph-stdout connection "log" "last" "50" "warn" "cluster"))
           (remove str/blank?)
           (map #(str "ceph cluster log: " (str/trim %)))))))

(plugin/provide!
 {:resize-watchers
  [(fn [signals]
     (when-let [mgr-host (first (config/env-value signals [:ceph :mgr-hosts]))]
       (ceph-cluster-log-errors (ceph/connection {:host mgr-host})
                                (config/env-value signals [:vpn :interface]))))]})
