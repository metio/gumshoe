;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.hosts
  "Host facts from node_exporter, read with PromQL: one row per host with
   its CPU count, memory, kernel, OS, boot time and whether its exporter is
   up, and a comparison with the hosts somebody declared.

   A host is the value of one label, chosen per source: `node` where a
   ServiceMonitor relabels the node name in, `hostname` or `nodename` for
   hosts scraped by their name, `instance` otherwise, whose port is
   stripped. Every query aggregates by that label, so a host scraped twice -
   over IPv4 and IPv6, or by two replicas - still counts once; cores count
   distinct `cpu` labels for the same reason."
  (:require [clojure.string :as str]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.promql :as promql])
  (:import (java.time Instant)))

(def facts
  "The facts read, in the order a row shows them."
  [:cores :memory-bytes :kernel :machine :os :os-version :boot-time :up])

(defn- matchers [base selector]
  (str "{" (str/join "," (remove str/blank? [base selector])) "}"))

(defn queries
  "PromQL per query name for hosts named by `host-label`, every metric
   narrowed by `selector` (a label selector body such as
   job=\"node\",cluster=\"a\") and `up` additionally by `job`, a regular
   expression naming the node_exporter jobs."
  [{:keys [host-label selector job]}]
  (let [h host-label
        sel #(matchers % selector)]
    {:cores (format "count by (%s) (count by (%s, cpu) (node_cpu_seconds_total%s))" h h (sel "mode=\"idle\""))
     :memory (format "max by (%s) (node_memory_MemTotal_bytes%s)" h (sel nil))
     :boot (format "max by (%s) (node_boot_time_seconds%s)" h (sel nil))
     :uname (format "max by (%s, release, machine) (node_uname_info%s)" h (sel nil))
     :os (format "max by (%s, name, version_id) (node_os_info%s)" h (sel nil))
     :up (format "max by (%s) (up%s)" h (sel (str "job=~" (pr-str (or job "node|.*node-exporter.*")))))}))

(defn host-name
  "The host a label value names: an instance loses its port, an IPv6
   instance its brackets too, and every name is lower-cased."
  [host-label value]
  (when (string? value)
    (str/lower-case
     (if (= "instance" host-label)
       (cond
         (str/starts-with? value "[") (second (re-find #"^\[([^\]]+)\]" value))
         (= 1 (count (re-seq #":" value))) (first (str/split value #":"))
         :else value)
       value))))

(defn- keyed
  "{host series} of one parsed answer, the first series per host."
  [host-label {:keys [ok series]}]
  (when ok
    (reduce (fn [m s] (let [h (host-name host-label (get-in s [:labels host-label]))]
                        (cond-> m (and h (not (contains? m h))) (assoc h s))))
            {} series)))

(defn- whole [v] (when (and v (Double/isFinite v)) (long v)))

(defn rows
  "One row per host found in any answer, sorted by host. `answers` maps each
   query name of `queries` to its parsed answer; a failed answer leaves its
   facts nil. :up is true when any target of the host is up, false when all
   are down, nil when the up query found none."
  [host-label answers]
  (let [k (into {} (map (fn [[q a]] [q (keyed host-label a)])) answers)
        hosts (sort (distinct (mapcat keys (vals k))))]
    (vec (for [h hosts
               :let [v #(get-in k [% h :value])
                     l #(get-in k [%1 h :labels %2])
                     up (v :up)
                     boot (whole (v :boot))]]
           {:host h
            :cores (whole (v :cores))
            :memory-bytes (whole (v :memory))
            :kernel (l :uname "release")
            :machine (l :uname "machine")
            :os (l :os "name")
            :os-version (l :os "version_id")
            :boot-time (when boot (str (Instant/ofEpochSecond boot)))
            :up (when (some? up) (pos? up))}))))

(def answer-keys
  "The evidence key per query whose facts a check judges: true when that
   query answered, nil when it failed, so a check joining it is unread
   rather than judging facts nobody read."
  {:up "up-answered" :cores "cores-answered" :memory "memory-answered"})

(defn answered
  "{evidence-key true-or-nil} for the queries of answer-keys."
  [answers]
  (into {} (for [[q k] answer-keys] [k (when (:ok (get answers q)) true)])))

(defn failures
  "{query-name error} of the answers that failed."
  [answers]
  (into (sorted-map) (keep (fn [[q a]] (when-not (:ok a) [q (:error a)]))) answers))

(defn compare-hosts
  "{:missing :unexpected} between the declared host names and the rows,
   compared lower-cased; each list sorted."
  [declared rows]
  (let [d (set (map str/lower-case declared))
        seen (set (map :host rows))]
    {:missing (vec (sort (remove seen d)))
     :unexpected (vec (sort (remove d seen)))}))

(defn declared-hosts
  "The host names of a declaration file: one per line, blank lines and #
   comments skipped."
  [text]
  (into [] (comp (map #(str/trim (first (str/split % #"#" 2))))
                 (remove str/blank?))
        (str/split-lines text)))

(defn read!
  "Runs every query of `opts` against the endpoint and returns the answers."
  [ep opts]
  (into {} (concurrent/map-all (fn [[q promql]] [q (promql/query! ep promql)]) (queries opts)
                               {:parallelism concurrent/default-parallelism})))
