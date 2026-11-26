;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.hosts
  "Detectives over host facts read from node_exporter: queries that failed,
   exporters that are down, hosts reporting too little to size them, and the
   difference between the hosts somebody declared and the hosts that report.

   Evidence keys: \"source\" (the URL read), \"hosts\" (the rows of
   gumshoe.hosts/rows), \"failed\" ({query error}), \"declared\" (host
   names, absent when none were given), and the markers of
   gumshoe.hosts/answer-keys, nil when the query behind a check's facts
   failed."
  (:require [clojure.string :as str]
            [gumshoe.evidence :as evidence]
            [gumshoe.hosts :as hosts]))

(defn detect-failed-queries
  [evidence]
  (for [[q error] (get evidence "failed")]
    {:severity :critical
     :component (get evidence "source")
     :summary (format "the %s query failed" (name q))
     :hint (str error " - the facts it reads are missing from every row, so do not read them as absent")
     :reports-unread true}))

(def ^:private up-joins [(hosts/answer-keys :up)])

(def ^:private facts-joins [(hosts/answer-keys :cores) (hosts/answer-keys :memory)])

(defn detect-down
  [evidence]
  (for [{:keys [host up]} (when (evidence/joined? evidence up-joins) (get evidence "hosts"))
        :when (false? up)]
    {:severity :critical
     :component host
     :summary "node_exporter is down"
     :hint "every scrape target of this host reports up == 0, so its facts are stale or missing"}))

(defn detect-incomplete
  [evidence]
  (for [{:keys [host up cores memory-bytes]} (when (evidence/joined? evidence facts-joins)
                                               (get evidence "hosts"))
        :when (not (false? up))
        :let [missing (cond-> [] (nil? cores) (conj "CPU count") (nil? memory-bytes) (conj "memory"))]
        :when (seq missing)]
    {:severity :warning
     :component host
     :summary (format "reports no %s" (str/join " or " missing))
     :hint "the scrape may drop node_cpu_seconds_total or node_memory_MemTotal_bytes, or the host label differs between metrics"}))

(defn- blind?
  "Whether no host was read because the queries failed, so a comparison with
   the declaration would report every declared host as missing."
  [evidence]
  (and (empty? (get evidence "hosts")) (seq (get evidence "failed"))))

(defn detect-missing
  [evidence]
  (when-let [declared (when-not (blind? evidence) (get evidence "declared"))]
    (for [h (:missing (hosts/compare-hosts declared (get evidence "hosts")))]
      {:severity :critical
       :component h
       :summary "declared host reports no node_exporter facts"
       :hint "it is not scraped, its exporter is not installed, or its name differs from the host label"})))

(defn detect-unexpected
  [evidence]
  (when-let [declared (get evidence "declared")]
    (for [h (:unexpected (hosts/compare-hosts declared (get evidence "hosts")))]
      {:severity :warning
       :component h
       :summary "host reports facts but is not declared"
       :hint "add it to the declaration, or find out what runs it"})))

(def detectives
  [{:name "host-queries"
    :description "Every host-facts query answered"
    :judges ["hosts"]
    :detect detect-failed-queries}
   {:name "host-exporters-up"
    :description "Every host's node_exporter is up"
    :judges ["hosts"]
    :joins up-joins
    :detect detect-down}
   {:name "host-facts-complete"
    :description "Every host reports its CPU count and memory"
    :judges ["hosts"]
    :joins facts-joins
    :detect detect-incomplete}
   {:name "declared-hosts-report"
    :description "Every declared host reports facts"
    :judges ["declared"]
    :detect detect-missing}
   {:name "reporting-hosts-declared"
    :description "Every reporting host is declared"
    :judges ["declared"]
    :detect detect-unexpected}])
