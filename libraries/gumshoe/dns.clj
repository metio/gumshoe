;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dns
  "Querying DNS servers with dig: short answers, tight timeouts, explicit
   transport selection. nil means the server did not answer at all, an empty
   vector means it answered with no records - the difference matters."
  (:require [clojure.string :as str]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(defn dig-args
  "Pure assembly of a dig invocation. Without a server the system resolver
   answers - that is the right view for deliverability questions. A :reverse?
   connection uses `dig -x`, which forms the in-addr.arpa / ip6.arpa query name
   from an IP; querying the bare IP as a PTR name never resolves."
  [{:keys [server transport reverse?]} type name]
  (vec (concat ["dig" "+short" "+time=3" "+tries=1"]
               (case transport
                 :ipv4 ["-4"]
                 :ipv6 ["-6"]
                 [])
               (when server [(str "@" server)])
               (if reverse? ["-x" name] [type name]))))

(defn query
  "Answers as a vector of lines; nil when the server did not reply."
  [connection type name]
  (let [result (apply shell/execute (dig-args connection type name))]
    (when (zero? (:exit result))
      (vec (remove str/blank? (str/split-lines (:out result)))))))

(defn ptr-records
  "Reverse-DNS (PTR) answers for an IP address, via `dig -x`. nil when the
   server did not reply."
  [connection address]
  (query (assoc connection :reverse? true) "PTR" address))

(defn strip-dot
  [name]
  (str/replace (str name) #"\.$" ""))

(defn parse-soa-serial
  "The serial is the third field of a +short SOA answer."
  [answer-line]
  (some-> answer-line (str/split #"\s+") (nth 2 nil)))

(defn soa-serial
  [connection zone]
  (some-> (first (query connection "SOA" zone)) parse-soa-serial))

(defn- answers?
  [connection zone]
  (boolean (seq (query connection "SOA" zone))))

(defn collect-evidence!
  "Probes the DNS setup in parallel: transports on the entry server, the
   zone's nameservers with their A/AAAA records, every nameserver's SOA
   serial, and the probe names on every nameserver."
  [{:keys [server zone probes]}]
  (stdout/print-section "🔍 Evidence (dig)")
  (stdout/err-println (str "  " (stdout/blue "▸") " zone " zone " via " server))
  (let [nameservers (mapv strip-dot (or (query {:server server} "NS" zone) []))
        transports (future {:ipv4 (answers? {:server server :transport :ipv4} zone)
                            :ipv6 (answers? {:server server :transport :ipv6} zone)})
        addresses (mapv (fn [ns]
                          (future [ns {:a (query {:server server} "A" ns)
                                       :aaaa (query {:server server} "AAAA" ns)}]))
                        nameservers)
        serials (mapv (fn [ns] (future [ns (soa-serial {:server ns} zone)])) nameservers)
        probed (mapv (fn [probe]
                       (future [probe (into {} (map (fn [ns] [ns (query {:server ns} "A" probe)])
                                                    nameservers))]))
                     probes)]
    (doseq [ns nameservers]
      (stdout/err-println (str "  " (stdout/blue "▸") " nameserver " ns)))
    {:now (java.time.Instant/now)
     "zone" zone
     "server" server
     "transports" @transports
     "nameservers" nameservers
     "addresses" (into {} (map deref) addresses)
     "serials" (into {} (map deref) serials)
     "probes" (into {} (map deref) probed)}))
