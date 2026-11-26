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
            [gumshoe.evpn :as evpn]
            [gumshoe.tally :as tally]))

(defn- gateway-vteps
  "The gateway VTEPs the scan was given, or nil when none were: an empty set
  would read every VTEP as no gateway, which is a judgement the scan cannot
  make without them."
  [evidence]
  (some-> (seq (get evidence "evpn-gateway-vteps")) set))

(defn- guest-mac-prefixes
  "Prefixes that mark a MAC as a guest rather than fabric infrastructure. Empty
  means every MAC is treated as a guest, which is the safe default for a fabric
  whose addressing is not declared: it reports more, never less."
  [evidence]
  (set (get evidence "evpn-guest-mac-prefixes")))

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
  (cond
    (nil? gateways)
    (str "two VTEPs disagree about the same MAC; the owner is authoritative. Whether "
         vtep " is a gateway VTEP could not be judged, because no gateway VTEPs were "
         "given (--gateway-vteps or :evpn :gateway-vteps in env.edn), so a router's "
         "proxy-advertised route cannot be told apart from a stale host route")

    (contains? gateways vtep)
    (str "a gateway VTEP has claimed a guest MAC, which is what "
         "proxy-macip-advertisement on a router's IRB does: the router proxies the "
         "guest's neighbour entry and advertises a type-2 MAC+IP route, and MAC+IP "
         "outranks the owner's MAC-only route no matter whose mobility sequence is "
         "higher. Removing the setting stops new proxy entries but does not withdraw "
         "the routes already advertised - those have to be cleared on the router")

    :else
    (str "two VTEPs disagree about the same MAC; the owner is authoritative, so "
         "whatever advertises " vtep " is stale or misconfigured")))

(defn detect-ownership-conflicts
  "The provable case: some host has the tap, and another host reaches the MAC
  through a VTEP that is not the owner's. It needs no gateway VTEPs to find
  one; they only decide which cause the hint names. An owner whose own VTEP
  was not read cannot be compared with anything, so its guests are left to
  evpn-hosts, which names the host as unjudged."
  [evidence]
  (let [macs (get evidence "evpn-macs")
        gateways (gateway-vteps evidence)
        owned (owners macs)]
    (for [{:keys [vni mac type host vtep remote-seq]} macs
          :when (= :remote type)
          :let [owner (get owned [vni mac])]
          :when (and owner (:vtep owner) (not= vtep (:vtep owner)))]
      {:severity :critical
       :component (str "vni " vni "/" mac)
       :summary (str "MAC is local on " (:host owner) " (" (:intf owner) ") but "
                     host " reaches it via " vtep)
       :hint (str (gateway-hint vtep gateways)
                  (when (and (:seq owner) remote-seq (> (:seq owner) remote-seq))
                    (str ". The owner's mobility sequence (" (:seq owner)
                         ") is already higher than the winning route's (" remote-seq
                         "), so this will not resolve by itself")))})))

(defn detect-gateway-only-macs
  "The suspicious case: a guest MAC no scanned host claims, reachable only
  through a gateway VTEP. Without gateway VTEPs nothing can be judged, and the
  check is marked unread through its :judges rather than passing."
  [evidence]
  (let [macs (get evidence "evpn-macs")
        gateways (gateway-vteps evidence)
        prefixes (guest-mac-prefixes evidence)
        owned (owners macs)]
    (when gateways
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

(defn detect-mac-ownership
  "Both ownership judgements over one evidence map: the conflicts a host's own
  tap proves, then the guest MACs only a gateway reaches."
  [evidence]
  (concat (detect-ownership-conflicts evidence)
          (detect-gateway-only-macs evidence)))

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

(defn- overlay-vni
  [dev]
  (some->> dev (re-matches #"^lo\.(\d+)$") second))

(defn- row-identity
  [row]
  (or (:line row) (dissoc row :read-vni)))

(defn- sightings
  "Each host's distinct fdb rows, each with `:seen`: how often one read of the
  table printed it. The table is read once per VNI scanned, so a row that every
  read prints once is one row seen once, and only a row one read printed twice
  counts twice."
  [rows]
  (for [{items :items} (tally/tally (juxt :host row-identity) rows)]
    (assoc (first items)
           :seen (apply max (map :count (tally/tally :read-vni items))))))

(defn- row-text
  "The row as `bridge fdb show` printed it, or rebuilt from its fields when it
  carries no line."
  [{:keys [line mac dev vlan dst entry-vni master self? state static?]}]
  (or line
      (str/join " " (concat [mac "dev" dev]
                            (when dst ["dst" dst])
                            (when vlan ["vlan" vlan])
                            (when entry-vni ["vni" entry-vni])
                            (when self? ["self"])
                            (when master ["master" master])
                            [(or state (when static? "static"))]))))

(defn- delete-command
  "The command that removes exactly this row. A row with a destination lives in
  the VXLAN device's own table and is addressed with `self` and its `dst`, a
  bridge row with `master` and its `vlan`; `bridge fdb del` without either flag
  addresses the device's own table only, so a bridge row needs `master` named."
  [{:keys [mac dev dst vlan entry-vni master self?]}]
  (str/join " " (concat ["bridge" "fdb" "del" mac "dev" dev]
                        (when dst ["dst" dst])
                        (when entry-vni ["vni" entry-vni])
                        (when vlan ["vlan" vlan])
                        (if (or dst self? (not master)) ["self"] ["master"]))))

(defn static-fdb-entries
  "The static fdb entries in the evidence, one per thing an operator acts on.

  One static entry spans several rows of `bridge fdb show`: the VXLAN device's
  own row naming the remote VTEP (`dst <vtep> self static`) and the bridge's rows
  sending the MAC out of that device (`master <bridge> static`, often twice, with
  and without a `vlan`). Rows for one host, device, entry VNI and MAC are one
  entry, and each distinct static destination among them is its own entry
  carrying the bridge rows with it, so two pins naming different VTEPs stay two.
  An entry with no static destination names what the VXLAN device's learned rows
  point at, a local port for a device that is no overlay, or `:unread` when no
  row names a destination at all.

  `:rows` holds each distinct row with how often one read printed it, and
  `:count` adds those up."
  [fdb]
  (for [{[host dev entry-vni mac] :key group :items}
        (tally/tally (juxt :host :dev :entry-vni :mac) (sightings (filter map? fdb)))
        :let [static (filter :static? group)]
        :when (seq static)
        :let [bridge-rows (remove :dst static)
              learned (->> group (remove :static?) (keep :dst) distinct vec)
              vni (or entry-vni (overlay-vni dev))
              base {:host host :dev dev :vni vni :mac mac}]
        entry (if-let [pinned (seq (filter :dst static))]
                (for [{dst :key rows :items} (tally/tally :dst pinned)]
                  (assoc base :dst dst :destination :static
                         :rows (vec (concat rows bridge-rows))))
                [(merge base
                        {:dst nil :rows (vec bridge-rows)}
                        (cond (nil? vni) {:destination :local-port}
                              (seq learned) {:destination :learned :learned-dsts learned}
                              :else {:destination :unread}))])]
    (assoc entry :count (reduce + (map :seen (:rows entry))))))

(defn- destination-text
  [{:keys [dst dev destination learned-dsts]}]
  (case destination
    :static (str "pointing at " dst)
    :learned (str "with no static destination; the device's learned rows send it to "
                  (str/join ", " learned-dsts))
    :local-port (str "pinned to the local port " dev ", so no remote VTEP")
    :unread (str "whose destination VTEP could not be read: no row for it on " dev
                 " names a dst")))

(defn- count-text
  [{:keys [rows] n :count}]
  (str (tally/counted n "fdb row" "fdb rows")
       (when (< (count rows) n)
         (str ", " (count rows) " distinct"))))

(defn detect-static-fdb
  "One warning per static entry, naming host, device, VNI, MAC and destination
  VTEP - or saying why there is none - with the rows behind it and how many
  there were, so an operator can reconcile the finding with `bridge fdb show`
  and remove every row of a pin rather than half of it."
  [evidence]
  (for [{:keys [host dev vni mac dst destination learned-dsts rows] n :count :as entry}
        (static-fdb-entries (get evidence "evpn-fdb"))]
    (cond-> {:severity :warning
             :component (str host "/" dev "/" mac)
             :summary (str "static fdb entry for " mac " on " dev
                           (when vni (str " (VNI " vni ")"))
                           " " (destination-text entry)
                           " - " (count-text entry))
             :hint (str "a static entry outranks what EVPN learns, so it keeps working only "
                        "while the guest stays put. If it was added to work around a fabric "
                        "fault, remove it once the fault is fixed - after a migration it "
                        "black-holes the guest exactly as the fault did. Removing it takes one "
                        "command per row, and a row left behind keeps part of the pin: "
                        (str/join "; " (map #(str "`" (delete-command %) "`") rows)))
             :host host
             :dev dev
             :vni vni
             :mac mac
             :dst dst
             :destination (name destination)
             :count n
             :rows (mapv (fn [row] {:row (row-text row) :seen (:seen row)}) rows)}
      (seq learned-dsts) (assoc :learned-dsts learned-dsts))))

(defn- vtep-unread-finding
  [{:keys [host vni vtep-error]}]
  {:severity :warning
   :component (str host "/vni " vni)
   :summary (str "host's own VTEP address could not be read: " vtep-error)
   :hint (str "evpn-mac-ownership compares the VTEP other hosts reach a guest through "
              "with its owner's own VTEP, so ownership of this host's guests is "
              "unjudged rather than clean. Check `show evpn vni " vni "` on the host: "
              "FRR prints the address as 'Local VTEP IP'")})

(defn detect-host-problems
  "A host that could not be read is a gap in the evidence, and the severity says
  how big. One unreadable host among several leaves the other hosts' disagreements
  visible, so it is a warning. None readable means nothing was compared at all -
  and because a detective with no evidence reports nothing, the run would otherwise
  end by calling every other check clean. That is the one outcome worse than a
  finding, so it is critical. A host that answered but whose own VTEP address
  was not read is a warning too: ownership of its guests is unjudged."
  [evidence]
  (let [hosts (get evidence "evpn-hosts")
        failed (filter :error hosts)
        total-failure? (and (seq hosts) (= (count failed) (count hosts)))]
    (concat
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
                     "result"))})
     (map vtep-unread-finding (filter #(and (not (:error %)) (:vtep-error %)) hosts)))))

(declare detectives)

(def ^:private input-wording
  "How a report names each judged input, and how a scan without it widens a
  check besides leaving the checks that judge it unjudged."
  {"gateway-vteps" {:label "gateway VTEPs"}
   "guest-mac-prefixes" {:label "guest MAC prefixes"
                         :consequence "every MAC counts as a guest"
                         :widens "evpn-gateway-only-mac also reports infrastructure MACs that sit behind a gateway"
                         :note "evpn-gateway-only-mac counts every MAC as a guest"}})

(defn- names
  [checks]
  (str/join ", " checks))

(defn missing-inputs
  "Every judged input the scan has no value for, from the \"evpn-inputs\"
  evidence: its option and env.edn path, the checks that cannot judge without
  it - those whose :judges hold its evidence key - and what it changes
  otherwise. Hosts and VNIs are never judged here: the scan refuses to start
  without them."
  [evidence]
  (let [declared (into {} (map (juxt :input identity)) evpn/inputs)]
    (for [{:keys [input option env-path source]} (get evidence "evpn-inputs")
          :let [key (get-in declared [input :evidence])]
          :when (and key (= "missing" source))]
      (merge {:input input :option option :env-path env-path
              :unjudged (vec (for [{:keys [name judges]} detectives
                                   :when (some #{key} judges)]
                               name))}
             (get input-wording input {:label input})))))

(defn- supply
  [{:keys [option env-path]}]
  (str "pass " option " or set " (str/join " " env-path) " in env.edn"))

(defn missing-input-lines
  "One line per missing judged input for stderr: what is missing, how to
  supply it, and which checks cannot judge without it."
  [evidence]
  (for [{:keys [label unjudged note] :as missing} (missing-inputs evidence)
        :when (or (seq unjudged) note)]
    (str "no " label " - " (supply missing) "; "
         (if (seq unjudged)
           (str (names unjudged) " cannot judge without them and "
                (if (= 1 (count unjudged)) "is" "are") " reported as unjudged")
           note))))

(defn detect-missing-inputs
  "A judged input nobody supplied. One that leaves a check unjudged is a
  warning naming that check, because the check's silence reads as clean to
  anyone who does not look at its unread mark; one that only widens a check
  is info. The inputs matter only to checks over the MAC tables, so without
  those there is nothing to report, and a collection that read nothing still
  ends as nothing checked."
  [evidence]
  (for [{:keys [label option unjudged consequence widens] :as missing}
        (when (some? (get evidence "evpn-macs")) (missing-inputs evidence))
        :when (or (seq unjudged) consequence)]
    (if (seq unjudged)
      {:severity :warning
       :component option
       :summary (str "no " label " were given, so " (names unjudged) " could not judge")
       :hint (str (supply missing) "; until then " (names unjudged)
                  (if (= 1 (count unjudged)) " is" " are")
                  " unjudged, and " (if (= 1 (count unjudged)) "its" "their")
                  " silence is not a clean result")}
      {:severity :info
       :component option
       :summary (str "no " label " were given, so " consequence)
       :hint (str (supply missing) "; until then " widens)})))

(def detectives
  [{:name "evpn-mac-ownership"
    :description "guest MACs the fabric attributes to a VTEP that is not their host's"
    :requires ["evpn-macs"]
    :detect detect-ownership-conflicts}
   {:name "evpn-gateway-only-mac"
    :description "guest MACs no scanned host owns that every VTEP reaches through a gateway VTEP"
    :requires ["evpn-macs"]
    :judges ["evpn-gateway-vteps"]
    :detect detect-gateway-only-macs}
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
    :detect detect-static-fdb}
   {:name "evpn-inputs"
    :description "values the scan judges against that neither an option nor env.edn supplied, and the checks that could not judge without them"
    :requires ["evpn-macs"]
    :detect detect-missing-inputs}])
