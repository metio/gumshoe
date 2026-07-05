;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.detectives.dns
  "Detectives for the DNS setup: dual-stack reachability, nameserver
   coherence, SOA serial replication, and answer consistency across every
   nameserver. Evidence comes from infra.dns/collect-evidence! - the detect
   functions stay pure."
  (:require [clojure.string :as str]))

(defn detect-transport-problems
  [evidence]
  (let [server (get evidence "server")
        {:keys [ipv4 ipv6]} (get evidence "transports")]
    (concat
     (when-not ipv4
       [{:severity :critical
         :component server
         :summary "server does not answer over IPv4"
         :hint "nothing that is v4-only can resolve anything right now"}])
     (when-not ipv6
       [{:severity :warning
         :component server
         :summary "server does not answer over IPv6"
         :hint "dual stack is broken - or this machine has no IPv6 connectivity"}]))))

(defn detect-nameserver-problems
  [evidence]
  (let [zone (get evidence "zone")
        nameservers (get evidence "nameservers")
        addresses (get evidence "addresses")]
    (concat
     (when (empty? nameservers)
       [{:severity :critical
         :component zone
         :summary "zone has no NS records"
         :hint "either the zone is broken or the entry server refused the query"}])
     (when (= 1 (count nameservers))
       [{:severity :warning
         :component zone
         :summary "zone has a single nameserver - no redundancy"
         :hint "one maintenance window away from a full DNS outage"}])
     (for [ns nameservers
           :when (empty? (:a (get addresses ns)))]
       {:severity :critical
        :component ns
        :summary "nameserver has no A record"
        :hint "v4-only resolvers can not reach this nameserver"})
     (for [ns nameservers
           :when (empty? (:aaaa (get addresses ns)))]
       {:severity :warning
        :component ns
        :summary "nameserver has no AAAA record"
        :hint "dual stack expects every nameserver to be reachable over IPv6"}))))

(defn detect-replication-problems
  [evidence]
  (let [zone (get evidence "zone")
        serials (get evidence "serials")
        answering (into {} (filter (comp some? val) serials))
        distinct-serials (distinct (vals answering))]
    (concat
     (for [[ns serial] serials
           :when (nil? serial)]
       {:severity :critical
        :component ns
        :summary "nameserver does not answer the SOA query"
        :hint "a dead nameserver serves stale NXDOMAINs to whoever still asks it"})
     (when (> (count distinct-serials) 1)
       [{:severity :critical
         :component zone
         :summary (format "SOA serials diverge: %s"
                          (str/join ", " (map (fn [[ns serial]] (str ns "=" serial))
                                              (sort answering))))
         :hint "replication is broken - the nameservers serve different zone versions"}]))))

(defn detect-probe-problems
  [evidence]
  (apply concat
         (for [[probe answers-by-ns] (sort (get evidence "probes"))]
           (let [answering (into {} (filter (comp some? val) answers-by-ns))
                 answer-sets (distinct (map set (vals answering)))]
             (concat
              (for [[ns answers] (sort answers-by-ns)
                    :when (and (some? answers) (empty? answers))]
                {:severity :critical
                 :component probe
                 :summary (format "no A record on nameserver %s" ns)
                 :hint "the name resolves nowhere or only on some nameservers"})
              (when (> (count answer-sets) 1)
                [{:severity :warning
                  :component probe
                  :summary "nameservers return different answers for this name"
                  :hint (str/join "; " (map (fn [[ns answers]] (format "%s -> %s" ns (str/join "," answers)))
                                            (sort answering)))}]))))))

(def detectives
  [{:name "dns-transports"
    :description "The entry server answers over both IPv4 and IPv6"
    :requires ["transports" "server"]
    :detect detect-transport-problems}
   {:name "dns-nameservers"
    :description "The zone has redundant, dual-stack reachable nameservers"
    :requires ["nameservers" "addresses" "zone"]
    :detect detect-nameserver-problems}
   {:name "dns-replication"
    :description "Every nameserver answers and serves the same SOA serial"
    :requires ["serials" "zone"]
    :detect detect-replication-problems}
   {:name "dns-probes"
    :description "Probe names resolve identically on every nameserver"
    :requires ["probes"]
    :detect detect-probe-problems}])
