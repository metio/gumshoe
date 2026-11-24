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
  (:require [clojure.string :as str]
            [gumshoe.evpn :as evpn]))

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

(def ^:private not-collected
  {:read? false :error "not collected"})

(defn conflict-reading
  "What FRR says behind one fdb conflict, as finding data.

  `:bgp-route` is `present` when a BGP path names the stale remote VTEP as its
  next hop, `absent` when BGP answered with no such path, and `unread` when it
  did not answer; `:classification` follows it as `live-misadvertisement`,
  `orphan` or `unknown`. A path from any other VTEP - the host's own
  advertisement of its local MAC included - does not stand behind the stale
  entry. `:local-uptime` is FRR's uptime of the MAC only while FRR holds it as
  local, because the uptime of a remote entry says nothing about how long the
  guest has been here."
  [{:keys [stale-remote-vtep]} {:keys [mac-detail bgp-route]}]
  (let [mac-detail (or mac-detail not-collected)
        bgp-route (or bgp-route not-collected)
        local? (= :local (:state mac-detail))
        bgp (cond (not (:read? bgp-route)) "unread"
                  (some #{stale-remote-vtep} (:next-hops bgp-route)) "present"
                  :else "absent")]
    (cond-> {:frr-mac (if (:read? mac-detail) (name (:state mac-detail)) "unread")
             :local-uptime (when local? (:uptime mac-detail))
             :local-uptime-seconds (when local? (:uptime-seconds mac-detail))
             :bgp-route bgp
             :bgp-next-hops (:next-hops bgp-route)
             :classification (case bgp
                               "present" "live-misadvertisement"
                               "absent" "orphan"
                               "unknown")}
      (not (:read? mac-detail)) (assoc :frr-mac-error (:error mac-detail))
      (not (:read? bgp-route)) (assoc :bgp-route-error (:error bgp-route)))))

(defn- reading-hint
  [{:keys [stale-remote-vtep]} {:keys [frr-mac frr-mac-error local-uptime bgp-route
                                       bgp-route-error bgp-next-hops]}]
  (str (case frr-mac
         "local" (str "FRR has held the MAC as local for "
                      (or local-uptime "an uptime it did not print") ". ")
         "unread" (str "FRR's view of the MAC could not be read (" frr-mac-error
                       "), so how long it has been local is unknown. ")
         (str "FRR holds the MAC as " frr-mac ", not local, so it has no local uptime. "))
       (case bgp-route
         "absent" (str "No BGP path names " stale-remote-vtep
                       (when (seq bgp-next-hops)
                         (str " (paths name " (str/join ", " bgp-next-hops) ")"))
                       ", so this is an orphan: the route that installed the entry is "
                       "gone and a local delete holds. A migrated guest leaves two stale "
                       "rows, the dst self row and a vlan-tagged master row, so check "
                       "every row for the MAC before deleting. ")
         "present" (str "A BGP path from " stale-remote-vtep " still stands behind the "
                        "entry, so this is a live misadvertisement: a local delete is "
                        "relearned until whatever advertises it stops. ")
         (str "BGP could not be read (" bgp-route-error "), so whether a route stands "
              "behind the entry is unknown. "))))

(defn detect-fdb-conflicts
  [evidence]
  (let [context (get evidence "evpn-fdb-context")]
    (for [{:keys [host mac vni local-intf stale-remote-vtep] :as conflict}
          (evpn/fdb-conflicts (get evidence "evpn-fdb"))
          :let [reading (conflict-reading conflict (get context [host vni mac]))]]
      (merge
       {:severity :critical
        :component (str host "/" mac)
        :summary (str "host holds both a local tap entry (" local-intf
                      ") and a remote entry (dst " stale-remote-vtep ") for one MAC")
        :hint (str (reading-hint conflict reading)
                   "The host owns this MAC and is simultaneously told to send its "
                   "traffic across the fabric. Traffic for the guest is black-holed "
                   "rather than flooded, because a nolearning bridge has no fallback")
        :host host
        :mac mac
        :vni vni
        :local-intf local-intf
        :stale-remote-vtep stale-remote-vtep}
       reading))))

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
  "A host that could not be read is a gap in the evidence, and the severity says
  how big. One unreadable host among several leaves the other hosts' disagreements
  visible, so it is a warning. None readable means nothing was compared at all -
  and because a detective with no evidence reports nothing, the run would otherwise
  end by calling every other check clean. That is the one outcome worse than a
  finding, so it is critical."
  [evidence]
  (let [hosts (get evidence "evpn-hosts")
        failed (filter :error hosts)
        total-failure? (and (seq hosts) (= (count failed) (count hosts)))]
    (for [{:keys [host vni error]} failed]
      {:severity (if total-failure? :critical :warning)
       :component (str host "/vni " vni)
       :summary (str "fabric evidence could not be collected: " error)
       :hint (if total-failure?
               (str "no host could be read, so nothing was compared - treat the other "
                    "checks as unrun rather than clean. Inside a rootless namespace "
                    "ssh refuses a system config owned by an unmapped uid; gumshoe.ssh "
                    "names the user's own config with -F to avoid that, so a failure "
                    "here is more likely credentials, the host, or the network")
               (str "the scan is incomplete, so an ownership conflict involving this "
                    "host's guests cannot be seen. During a partition the unreachable "
                    "host is often the symptom - check it before trusting a clean "
                    "result"))})))

(def detectives
  [{:name "evpn-mac-ownership"
    :description "guest MACs the fabric attributes to a VTEP that is not their host's"
    :requires ["evpn-macs" "config"]
    :detect detect-mac-ownership}
   {:name "evpn-fdb-conflict"
    :description "hosts holding both a local tap and a remote entry for one MAC, with how long FRR has held it local and whether BGP still advertises the remote one"
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
