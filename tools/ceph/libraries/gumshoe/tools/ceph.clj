;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.ceph
  "The ceph tool package's plugin. The cephadm-over-SSH detectives and books live
   alongside this namespace (gumshoe.ceph, gumshoe.detectives.ceph, runbooks/,
   playbooks/); loading this plugin contributes ceph's one cross-cutting piece:
   the ceph cluster-log tailer for storage-resize watches. Storage resize is a
   generic CSI operation that stays in the engine - this is one storage provider's
   observability, added only when the ceph package is loaded. When a resize runs
   on a cluster whose env.edn names ceph mgr hosts, warn-and-above lines from the
   ceph cluster log, read from the first of those hosts that answers, surface
   live alongside the PVC's own conditions, gated on the VPN so it never reaches
   a private host off-network."
  (:require [clojure.string :as str]
            [gumshoe.ceph :as ceph]
            [gumshoe.config :as config]
            [gumshoe.net :as net]
            [gumshoe.plugin :as plugin]
            [gumshoe.storage :as storage]))

(defn mgr-hosts
  "The ceph mgr hosts configured for signals, in their configured order, as
   trimmed non-blank strings without repeats. A single string counts as a list
   of one host, so it is never read character by character."
  [signals]
  (let [configured (config/env-value signals [:ceph :mgr-hosts])]
    (->> (if (string? configured) [configured] configured)
         (filter string?)
         (map str/trim)
         (remove str/blank?)
         distinct
         vec)))

(defn- host-list
  [tried]
  (str/join ", " (map :host tried)))

(defn cluster-log-signals
  "Pure: the watcher signals for one cluster-log read across the mgr hosts,
   given the first-answering result of reading each in turn. The answering
   host's warn-and-above lines, or one signal saying no host answered, so an
   unread cluster log never looks like a quiet one."
  [{:keys [host answer tried]}]
  (if host
    (->> (str/split-lines (:out answer))
         (remove str/blank?)
         (map #(str "ceph cluster log: " (str/trim %))))
    [(if (seq tried)
       (format "ceph cluster log: not read - no ceph mgr host answered (tried %s), so ceph errors during this resize are not shown"
               (host-list tried))
       "ceph cluster log: not read - no ceph mgr host is configured")]))

(defn cluster-log-answer
  "One host's answer to the cluster-log read: ok only when ssh and ceph exited
   cleanly, so an unreachable host is not read as one with an empty log."
  [host]
  (ceph/ceph-answer (ceph/connection {:host host}) "log" "last" "50" "warn" "cluster"))

(defn ceph-cluster-log-errors
  "A watcher: warn-and-above lines from the ceph cluster log over SSH, read
   from the first of hosts that answers, tried in order on every sample. When
   no host answers it returns one signal saying so. When the VPN is down it
   reads nothing and says that instead, so it never reaches a private host
   off-network. read defaults to cluster-log-answer."
  ([hosts vpn-interface]
   (ceph-cluster-log-errors hosts vpn-interface cluster-log-answer))
  ([hosts vpn-interface read]
   (fn []
     (if (net/interface-up? vpn-interface)
       (cluster-log-signals (ceph/first-answering hosts read))
       [(format "ceph cluster log: not read - the VPN interface %s is down" vpn-interface)]))))

(def ^:private bytes-per-gi (* 1024 1024 1024))

(defn min-pool-max-avail
  "Pure: the smallest per-pool max_avail in a parsed `ceph df --format json`
   document - the replication-adjusted usable free bytes of the most-constrained
   pool. This is what a resize can actually consume: the raw stats.total_avail_bytes
   ignores replication, so on a 3x pool a logical growth of N bytes really costs
   3N raw and comparing against the raw figure lets the pool overfill. nil when
   no pool capacity can be read."
  [df-json]
  (let [avails (keep #(get-in % [:stats :max_avail]) (:pools df-json))]
    (when (seq avails) (long (apply min avails)))))

(defn usable-answer
  "One host's answer to the capacity read, from `ceph df --format json`:
   {:ok? .. :usable <bytes> ..}. ok only when ssh and ceph exited cleanly, the
   output parsed and at least one pool's max_avail was read; a host that
   answers with an error or a document without pool capacity does not count,
   so the next host is tried. Never throws."
  [connection]
  (try
    (let [{:keys [ok? doc] :as answer} (ceph/ceph-json-answer connection "df")
          usable (when ok? (min-pool-max-avail doc))]
      (assoc (dissoc answer :doc) :ok? (some? usable) :usable usable))
    (catch Exception e {:ok? false :error (or (ex-message e) (str e))})))

(defn usable-bytes
  "The replication-adjusted free capacity a resize can consume, from
   `ceph df --format json`, or nil when it can not be read or parsed - nil forces
   the caller to fail closed rather than guess. Never throws."
  [connection]
  (:usable (usable-answer connection)))

(defn capacity-answer
  "The capacity read of one mgr host by name."
  [host]
  (usable-answer (ceph/connection {:host host})))

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

(defn capacity-problems
  "Pure: the preflight's problems for the configured hosts, the VPN state, the
   first-answering result of the capacity read (nil when it was not attempted)
   and the plan. Fail closed: when hosts are configured and no host answered,
   or the VPN is down, the resize is refused. The VPN refusal names the first
   configured host; the unread refusal names every host tried, in order, which
   for a single host is the same text it has always been."
  [hosts vpn-up? {:keys [host answer tried]} plan]
  (cond
    (empty? hosts) []
    (not vpn-up?)
    [(format "ceph mgr host %s is configured but the VPN is down - cannot verify ceph capacity, refusing the resize" (first hosts))]
    (nil? host)
    [(format "could not read ceph free capacity from %s - refusing the resize (fail closed)"
             (if (seq tried) (host-list tried) (str/join ", " hosts)))]
    :else
    (let [usable (:usable answer)
          growth (growth-bytes plan)]
      (cond
        (nil? usable)
        [(format "could not read ceph free capacity from %s - refusing the resize (fail closed)" host)]
        (nil? growth) []
        (> growth usable)
        [(format "ceph has %s usable free but the resize needs %s more - refusing to avoid filling the cluster"
                 (human-gib usable) (human-gib growth))]
        :else []))))

(defn capacity-preflight
  "Fail-closed: when ceph mgr hosts are configured for the resize's cluster, the
   ceph cluster must be readable through one of them AND have room for the
   growth. The hosts are tried in their configured order and the first that
   answers is used. If none can be read (VPN down, every host unreachable or
   answering with an error), the resize is refused. When no mgr host is
   configured this is not a ceph-checkable resize, so it returns no problem.
   read defaults to capacity-answer."
  ([context] (capacity-preflight context capacity-answer))
  ([{:keys [cluster plan]} read]
   (let [signals {:kubernetes-cluster cluster}
         hosts (mgr-hosts signals)]
     (if (empty? hosts)
       []
       (let [vpn-up? (net/interface-up? (config/env-value signals [:vpn :interface]))]
         (capacity-problems hosts vpn-up?
                            (when vpn-up? (ceph/first-answering hosts read))
                            plan))))))

(plugin/provide!
 {:resize-watchers
  [(fn [signals]
     (let [hosts (mgr-hosts signals)]
       (when (seq hosts)
         (ceph-cluster-log-errors hosts (config/env-value signals [:vpn :interface])))))]
  :resize-preflights
  [capacity-preflight]})
