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
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [gumshoe.ceph :as ceph]
            [gumshoe.config :as config]
            [gumshoe.net :as net]
            [gumshoe.plugin :as plugin]
            [gumshoe.storage :as storage]))

(defn ceph-cluster-log-errors
  "A watcher: warn-and-above lines from the ceph cluster log over SSH, or nothing
   when the VPN is down."
  [connection vpn-interface]
  (fn []
    (when (net/interface-up? vpn-interface)
      (->> (str/split-lines (ceph/ceph-stdout connection "log" "last" "50" "warn" "cluster"))
           (remove str/blank?)
           (map #(str "ceph cluster log: " (str/trim %)))))))

(def ^:private bytes-per-gi (* 1024 1024 1024))

(defn available-bytes
  "The cluster's free capacity in bytes from `ceph df --format json`, or nil when
   it can not be read or parsed - nil forces the caller to fail closed rather than
   guess. Never throws."
  [connection]
  (try
    (let [out (ceph/ceph-stdout connection "df" "--format" "json")
          avail (when-not (str/blank? (str out))
                  (get-in (json/parse-string out true) [:stats :total_avail_bytes]))]
      (when (number? avail) (long avail)))
    (catch Exception _ nil)))

(defn growth-bytes
  "Pure: the additional bytes a resize plan consumes - sum of (target - current) -
   or nil when any entry is not a Gi capacity we can measure (the generic check
   already refuses those, so nil here means 'not our call', not 'unsafe')."
  [plan]
  (reduce (fn [acc {:keys [current target]}]
            (let [c (storage/gi-value current)
                  t (storage/gi-value target)]
              (if (and acc c t) (+ acc (* (- t c) bytes-per-gi)) (reduced nil))))
          0
          plan))

(defn human-gib [bytes] (format "%.1f GiB" (double (/ bytes bytes-per-gi))))

(defn capacity-preflight
  "Fail-closed: when a ceph mgr host is configured for the resize's cluster, the
   ceph cluster must be reachable AND have room for the growth. If it can not be
   verified (VPN down, unreadable df), the resize is refused. When no mgr host is
   configured this is not a ceph-checkable resize, so it returns no problem."
  [{:keys [cluster plan]}]
  (let [signals {:kubernetes-cluster cluster}
        mgr-host (first (config/env-value signals [:ceph :mgr-hosts]))]
    (cond
      (nil? mgr-host) []
      (not (net/interface-up? (config/env-value signals [:vpn :interface])))
      [(format "ceph mgr host %s is configured but the VPN is down - cannot verify ceph capacity, refusing the resize" mgr-host)]
      :else
      (let [available (available-bytes (ceph/connection {:host mgr-host}))
            growth (growth-bytes plan)]
        (cond
          (nil? available)
          [(format "could not read ceph free capacity from %s - refusing the resize (fail closed)" mgr-host)]
          (nil? growth) []
          (> growth available)
          [(format "ceph has %s free but the resize needs %s more - refusing to avoid filling the cluster"
                   (human-gib available) (human-gib growth))]
          :else [])))))

(plugin/provide!
 {:resize-watchers
  [(fn [signals]
     (when-let [mgr-host (first (config/env-value signals [:ceph :mgr-hosts]))]
       (ceph-cluster-log-errors (ceph/connection {:host mgr-host})
                                (config/env-value signals [:vpn :interface]))))]
  :resize-preflights
  [capacity-preflight]})
