;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.dns
  "Detectives for the DNS setup: dual-stack reachability, nameserver
   coherence, SOA serial replication, and answer consistency across every
   nameserver. Evidence comes from gumshoe.dns/collect-evidence!, each read
   a three-state result of gumshoe.reads.result - the detect functions stay
   pure, and judge a missing record only on :absent, never on a read that
   did not answer."
  (:require [clojure.string :as str]
            [gumshoe.evidence :as evidence]
            [gumshoe.reads.result :as result]))

(defn detect-transport-problems
  "The entry server answers the zone's SOA query over IPv4 and over IPv6.
   Any answer counts, NXDOMAIN included: the question is whether the
   transport reaches the server, not what the zone holds."
  [evidence]
  (let [server (get evidence "server")
        {:keys [ipv4 ipv6]} (get evidence "transports")]
    (concat
     (when-not (result/answered? ipv4)
       [{:severity :critical
         :component server
         :summary "server does not answer over IPv4"
         :hint (str (result/unread-line-of ipv4)
                    " - nothing that is v4-only can resolve anything right now")}])
     (when-not (result/answered? ipv6)
       [{:severity :warning
         :component server
         :summary "server does not answer over IPv6"
         :hint (str (result/unread-line-of ipv6)
                    " - dual stack is broken, or this machine has no IPv6 connectivity")}]))))

(def nameserver-joins
  "The two reads the nameserver check joins: without the NS answer the zone
   would read as having no nameserver, and without the address answers every
   nameserver would read as having no A or AAAA record."
  ["nameservers" "addresses"])

(defn- no-nameserver-finding
  "The finding for an NS query that answered without any NS record: the
   zone does not exist on the entry server (NXDOMAIN), or exists without
   nameservers (NOERROR)."
  [zone ns-read]
  (if (= "NXDOMAIN" (:status ns-read))
    {:severity :critical
     :component zone
     :summary "zone does not exist on the entry server"
     :hint (str "the entry server answered NXDOMAIN for " zone
                " - check the zone's name and that the entry server serves or delegates it")}
    {:severity :critical
     :component zone
     :summary "zone has no NS records"
     :hint (str "the entry server answered for " zone
                " without any NS record - the zone is broken")}))

(defn detect-zone-problems
  "The entry server answers the zone's NS query with nameservers. An NS
   query that did not answer is reported as unread, naming the read and its
   reason; one that answered without any NS record is critical, worded for
   NXDOMAIN (the zone does not exist there) apart from NOERROR (the zone
   exists without nameservers)."
  [evidence]
  (let [zone (get evidence "zone")
        ns-read (get evidence "nameservers")]
    (result/on-state
     ns-read
     {:present (constantly nil)
      :absent #(vector (no-nameserver-finding zone %))
      :unread (fn [r]
                [{:severity :warning
                  ;; names the read that did not answer, never what it would hold
                  :reports-unread true
                  :component zone
                  :summary (result/unread-line-of r)
                  :hint "whether the zone has nameservers is unknown, so the checks that ask each nameserver judge nothing"}])})))

(defn detect-nameserver-problems
  "A zone with a single nameserver, and nameservers without an A or AAAA
   record. An address query that did not answer is reported as unresolved,
   never as a missing record. Nothing is judged when a joined read did not
   answer; dns-zone reports the NS read itself."
  [evidence]
  (let [zone (get evidence "zone")
        addresses (get evidence "addresses")]
    (when (evidence/joined? evidence nameserver-joins)
      ;; joined? holds, so the NS read answered and its items are the answer
      (let [nameservers (vec (:items (get evidence "nameservers")))]
        (concat
         (when (= 1 (count nameservers))
           [{:severity :warning
             :component zone
             :summary "zone has a single nameserver - no redundancy"
             :hint "one maintenance window away from a full DNS outage"}])
         (for [ns nameservers
               :let [{:keys [a aaaa]} (get addresses ns)
                     unread (concat (when (result/unread? a) ["A record"])
                                    (when (result/unread? aaaa) ["AAAA record"]))]
               :when (seq unread)]
           {:severity :warning
            :component ns
            :summary (format "could not resolve the nameserver's %s" (str/join " and " unread))
            :hint (str "the entry server did not answer the address query - whether the record exists is unknown"
                       (when-let [why (seq (keep #(:reason %) [a aaaa]))]
                         (str " (" (str/join ", " (distinct why)) ")")))})
         (for [ns nameservers
               :when (result/absent? (:a (get addresses ns)))]
           {:severity :critical
            :component ns
            :summary "nameserver has no A record"
            :hint "v4-only resolvers can not reach this nameserver"})
         (for [ns nameservers
               :when (result/absent? (:aaaa (get addresses ns)))]
           {:severity :warning
            :component ns
            :summary "nameserver has no AAAA record"
            :hint "dual stack expects every nameserver to be reachable over IPv6"}))))))

(defn detect-replication-problems
  "Every nameserver answers the zone's SOA query with the same serial. A
   nameserver whose query did not answer is critical and does not count as
   divergence; one that answered without an SOA record does not serve the
   zone."
  [evidence]
  (let [zone (get evidence "zone")
        serials (get evidence "serials")
        answering (into {} (keep (fn [[ns r]]
                                   (when (and (result/present? r) (some? (:value r)))
                                     [ns (:value r)])))
                        serials)
        distinct-serials (distinct (vals answering))]
    (concat
     (for [[ns r] (sort-by key serials)
           :when (result/unread? r)]
       {:severity :critical
        :component ns
        :summary "nameserver does not answer the SOA query"
        :hint (str "a dead nameserver serves stale NXDOMAINs to whoever still asks it"
                   (when-let [why (:reason r)] (str " (" why ")")))})
     (for [[ns r] (sort-by key serials)
           :when (result/absent? r)]
       {:severity :critical
        :component ns
        :summary "nameserver serves no SOA record for the zone"
        :hint (format "it answered %s for %s - a lame delegation: resolvers asking it get no answer for the zone"
                      (or (:status r) "without an SOA record") zone)})
     (for [[ns r] (sort-by key serials)
           :when (and (result/present? r) (nil? (:value r)))]
       {:severity :warning
        :component ns
        :summary "nameserver's SOA record carries no serial"
        :hint "the SOA answer does not parse, so its replication cannot be judged"})
     (when (> (count distinct-serials) 1)
       [{:severity :critical
         :component zone
         :summary (format "SOA serials diverge: %s"
                          (str/join ", " (map (fn [[ns serial]] (str ns "=" serial))
                                              (sort answering))))
         :hint "replication is broken - the nameservers serve different zone versions"}]))))

(defn detect-probe-problems
  "Every probe name resolves on every nameserver, to the same answer. A
   nameserver whose query did not answer judges nothing for that name."
  [evidence]
  (apply concat
         (for [[probe answers-by-ns] (sort (get evidence "probes"))]
           (let [answering (into {} (keep (fn [[ns r]]
                                            (when (result/answered? r) [ns (result/items! r)])))
                                 answers-by-ns)
                 answer-sets (distinct (map set (vals answering)))]
             (concat
              (for [[ns r] (sort-by key answers-by-ns)
                    :when (result/absent? r)]
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
   {:name "dns-zone"
    :description "The entry server answers the zone's NS query with nameservers"
    :requires ["nameservers" "zone"]
    :detect detect-zone-problems}
   {:name "dns-nameservers"
    :description "The zone has redundant, dual-stack reachable nameservers"
    :requires (conj nameserver-joins "zone")
    :joins nameserver-joins
    :detect detect-nameserver-problems}
   {:name "dns-replication"
    :description "Every nameserver answers and serves the same SOA serial"
    :requires ["serials" "zone"]
    :joins ["serials"]
    :detect detect-replication-problems}
   {:name "dns-probes"
    :description "Probe names resolve identically on every nameserver"
    :requires ["probes"]
    :detect detect-probe-problems}])
