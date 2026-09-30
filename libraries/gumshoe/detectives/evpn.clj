;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.evpn
  "Detectives for an EVPN-VXLAN fabric: guest MACs the fabric attributes to the
  wrong VTEP, and the local overrides that hide or cause it.

  The question these answer is ownership. A guest MAC belongs to exactly one
  host, and every VTEP in the fabric should agree on which. When they disagree
  the guest is not down - it is unreachable, which looks identical from a
  cluster's point of view and nothing in the cluster can explain it.

  Evidence is per host, so a disagreement between two hosts about the same MAC
  is visible without asking a router anything."
  (:require [clojure.string :as str]))

(defn- gateway-vteps
  [evidence]
  (set (get-in evidence ["config" :evpn :gateway-vteps])))

(defn- guest-mac-prefixes
  "Prefixes that mark a MAC as a guest rather than fabric infrastructure. Empty
  means every MAC is treated as a guest, which is the safe default for a fabric
  whose addressing is not declared."
  [evidence]
  (set (get-in evidence ["config" :evpn :guest-mac-prefixes])))

(defn- guest-mac?
  [prefixes mac]
  (or (empty? prefixes)
      (some #(str/starts-with? mac %) prefixes)))

(defn- owners
  "host-vtep of every host that reports the MAC as local, keyed by [vni mac]."
  [macs]
  (reduce (fn [acc {:keys [vni mac type host host-vtep intf local-seq]}]
            (cond-> acc
              (= :local type) (assoc [vni mac] {:host host :vtep host-vtep
                                                :intf intf :seq local-seq})))
          {}
          macs))

(defn- gateway-hint
  [vtep gateways]
  (if (contains? gateways vtep)
    (str "a gateway VTEP has claimed a guest MAC, which is what "
         "proxy-macip-advertisement on a router's IRB does: the router proxies the "
         "guest's neighbour entry and advertises a type-2 MAC+IP route, and MAC+IP "
         "outranks the owner's MAC-only route no matter whose mobility sequence is "
         "higher. Removing the setting stops new proxy entries but does not withdraw "
         "the routes already advertised - those have to be cleared on the router")
    (str "two VTEPs disagree about the same MAC; the owner is authoritative, so "
         "whatever advertises " vtep " is stale or misconfigured")))

(defn detect-mac-ownership
  [evidence]
  (let [macs (get evidence "evpn-macs")
        gateways (gateway-vteps evidence)
        prefixes (guest-mac-prefixes evidence)
        owned (owners macs)]
    (concat
     ;; the provable case: some host has the tap, and someone points elsewhere
     (for [{:keys [vni mac type host vtep remote-seq]} macs
           :when (= :remote type)
           :let [owner (get owned [vni mac])]
           :when (and owner (not= vtep (:vtep owner)))]
       {:severity :critical
        :component (str "vni " vni "/" mac)
        :summary (str "MAC is local on " (:host owner) " (" (:intf owner) ") but "
                      host " reaches it via " vtep)
        :hint (str (gateway-hint vtep gateways)
                   (when (and (:seq owner) remote-seq (> (:seq owner) remote-seq))
                     (str ". The owner's mobility sequence (" (:seq owner)
                          ") is already higher than the winning route's (" remote-seq
                          "), so this will not resolve by itself")))})
     ;; the suspicious case: a guest MAC nobody claims, reachable only via a gateway
     (for [[[vni mac] entries] (group-by (juxt :vni :mac) macs)
           :when (and (guest-mac? prefixes mac)
                      (not (get owned [vni mac]))
                      (seq entries)
                      (every? #(contains? gateways (:vtep %)) entries))]
       {:severity :warning
        :component (str "vni " vni "/" mac)
        :summary (str "guest MAC is reachable only through a gateway VTEP ("
                      (str/join ", " (sort (distinct (map :vtep entries)))) ")")
        :hint (str "no scanned host reports it as local. Either its host is outside "
                   ":evpn :hosts, or a gateway has claimed it the way "
                   "proxy-macip-advertisement does")}))))

(defn detect-fdb-conflicts
  [evidence]
  (for [[[host mac] entries] (group-by (juxt :host :mac) (get evidence "evpn-fdb"))
        :let [tap (some #(when-not (:vxlan? %) %) entries)
              remote (some #(when (:dst %) %) entries)]
        :when (and tap remote)]
    {:severity :critical
     :component (str host "/" mac)
     :summary (str "host holds both a local tap entry (" (:dev tap)
                   ") and a remote entry (dst " (:dst remote) ") for one MAC")
     :hint (str "the host owns this MAC and is simultaneously told to send its "
                "traffic across the fabric. Traffic for the guest is black-holed "
                "rather than flooded, because a nolearning bridge has no fallback")}))

(defn detect-static-fdb
  [evidence]
  (for [{:keys [host mac dev dst static?]} (get evidence "evpn-fdb")
        :when static?]
    {:severity :warning
     :component (str host "/" mac)
     :summary (str "static fdb entry on " dev
                   (when dst (str " pointing at " dst)))
     :hint (str "a static entry outranks what EVPN learns, so it keeps working only "
                "while the guest stays put. If it was added to work around a fabric "
                "fault, remove it once the fault is fixed - after a migration it "
                "black-holes the guest exactly as the fault did")}))

(defn detect-host-problems
  [evidence]
  (for [{:keys [host vni error]} (get evidence "evpn-hosts")
        :when error]
    {:severity :warning
     :component (str host "/vni " vni)
     :summary (str "fabric evidence could not be collected: " error)
     :hint (str "the scan is incomplete, so an ownership conflict involving this "
                "host's guests cannot be seen. During a partition the unreachable "
                "host is often the symptom - check it before trusting a clean result")}))

(def detectives
  [{:name "evpn-mac-ownership"
    :description "guest MACs the fabric attributes to a VTEP that is not their host's"
    :requires ["evpn-macs" "config"]
    :detect detect-mac-ownership}
   {:name "evpn-fdb-conflict"
    :description "hosts holding both a local tap and a remote entry for one MAC"
    :requires ["evpn-fdb"]
    :detect detect-fdb-conflicts}
   {:name "evpn-hosts"
    :description "hosts whose fabric state could not be read, making the scan incomplete"
    :requires ["evpn-hosts"]
    :detect detect-host-problems}
   {:name "evpn-static-fdb"
    :description "static fdb entries, which outrank EVPN learning and go stale silently"
    :requires ["evpn-fdb"]
    :detect detect-static-fdb}])
