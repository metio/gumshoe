;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.evpn
  "Reads an EVPN-VXLAN fabric's MAC ownership from the hosts themselves.

  Every VTEP in a fabric holds its own opinion about where a MAC lives, and a
  partition is exactly the case where those opinions differ. So the evidence is
  gathered per host and kept per host - asking one host, or asking a router,
  cannot show a disagreement.

  Read-only throughout: three `show`-style commands per host, plus two per fdb
  conflict found, and no configuration is touched. Hosts are queried in parallel and a host that cannot be reached
  becomes evidence rather than an aborted scan, because the unreachable host is
  often the symptom being investigated."
  (:require [clojure.string :as str]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]))

(def ^:private mac-pattern #"(?i)^[0-9a-f]{2}(:[0-9a-f]{2}){5}$")
(def ^:private ipv4-pattern #"\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}")
(def ^:private flags-pattern #"^[NIPX]+$")
(def ^:private seq-pattern #"^(\d+)/(\d+)$")

(defn- parse-long-or-nil
  [s]
  (try (Long/parseLong s) (catch Exception _ nil)))

(def ^:private ipv6-pattern
  "A token shaped like an IPv6 address: hex groups and at least two colons. A
  MAC has the same alphabet, so the caller rules MACs out separately."
  #"^[0-9a-fA-F]{0,4}(:[0-9a-fA-F]{0,4}){2,7}(:\d{1,3}(\.\d{1,3}){3})?$")

(defn- line-address
  "The first IPv4 or IPv6 address among a line's tokens. FRR prints the local
  VTEP with %pIA, which prints either family, so an IPv6 underlay is read as
  readily as an IPv4 one."
  [line]
  (or (re-find ipv4-pattern line)
      (some (fn [token]
              (when (and (re-matches ipv6-pattern token)
                         (not (re-matches mac-pattern token)))
                token))
            (str/split (str/trim line) #"[\s,]+"))))

(defn parse-local-vtep
  "The host's own VTEP address, from `show evpn vni <vni>`. Matched loosely - any
  address on a line mentioning VTEP - so a column reshuffle between FRR releases
  does not silently yield the wrong host. nil when no such line carries an
  address."
  [output]
  (some (fn [line]
          (when (str/includes? line "VTEP")
            (line-address line)))
        (str/split-lines (or output ""))))

(defn parse-macs
  "Rows of `show evpn mac vni <vni>`.

  The Flags column is frequently empty, so columns are identified by shape rather
  than position: the first token is the MAC, the second the type, a short
  all-flag-letters token is skipped, and the trailing `local/remote` sequence
  pair is read from the end."
  [vni host host-vtep output]
  (for [line (str/split-lines (or output ""))
        :let [tokens (str/split (str/trim line) #"\s+")
              [mac kind & rest-tokens] tokens]
        :when (and mac kind (re-matches mac-pattern mac)
                   (#{"local" "remote" "auto"} kind))
        :let [rest-tokens (if (and (first rest-tokens)
                                   (re-matches flags-pattern (first rest-tokens)))
                            (next rest-tokens)
                            rest-tokens)
              where (first rest-tokens)
              seqs (re-matches seq-pattern (or (last rest-tokens) ""))
              local? (= "local" kind)]]
    (cond-> {:host host
             :host-vtep host-vtep
             :vni vni
             :mac (str/lower-case mac)
             :type (keyword kind)
             :local-seq (some-> seqs (nth 1) parse-long-or-nil)
             :remote-seq (some-> seqs (nth 2) parse-long-or-nil)}
      local? (assoc :intf where)
      (not local?) (assoc :vtep where))))

(def ^:private vxlan-dev-pattern #"^lo\.(\d+)$")

(defn parse-fdb
  "Rows of `bridge fdb show`, kept only for this VNI: its own VXLAN device and the
  guest taps.

  Every overlay on the host shows up in one table, so another VNI's VXLAN device
  has to be dropped rather than mistaken for a tap - otherwise every legitimate
  remote entry on a neighbouring VNI pairs with its own overlay device and reads as
  a conflict. A tap is any device that is not an `lo.<vni>`.

  `dst` marks an entry that sends the MAC across the fabric; its absence on a tap
  marks local ownership. Both existing for one MAC on one host is the contradiction
  the fdb-conflict detective looks for."
  [vni host output]
  (let [vxlan-dev (str "lo." vni)]
    (for [line (str/split-lines (or output ""))
          :let [tokens (str/split (str/trim line) #"\s+")
                mac (first tokens)]
          :when (and mac (re-matches mac-pattern mac))
          :let [pairs (apply hash-map (concat (rest tokens)
                                              (when (odd? (count (rest tokens))) [nil])))
                dev (get pairs "dev")
                dst (get pairs "dst")
                flags (set (rest tokens))
                other-overlay? (and dev
                                    (re-matches vxlan-dev-pattern dev)
                                    (not= dev vxlan-dev))]
          :when (and dev (not other-overlay?))]
      {:host host
       :mac (str/lower-case mac)
       :dev dev
       :dst dst
       :vxlan? (= dev vxlan-dev)
       :static? (contains? flags "static")
       :extern-learn? (contains? flags "extern_learn")})))

(def ^:private interval-patterns
  "The three shapes FRR's frrtime_to_interval prints an age in: under a day,
  under a week, and weeks."
  [[#"^(\d+):(\d{2}):(\d{2})$" (fn [h m s] (+ (* h 3600) (* m 60) s))]
   [#"^(\d+)d(\d{2})h(\d{2})m$" (fn [d h m] (+ (* d 86400) (* h 3600) (* m 60)))]
   [#"^(\d+)w(\d+)d(\d{2})h$" (fn [w d h] (+ (* w 7 86400) (* d 86400) (* h 3600)))]])

(defn uptime-seconds
  "Seconds in an FRR uptime such as `00:04:27`, `4d09h18m` or `05w0d22h`, or nil
  for anything else. FRR derives the days from gmtime's day of the year, so an
  age of a year or more prints modulo a year and reads back the same way."
  [s]
  (some (fn [[pattern f]]
          (when-let [[_ & parts] (re-matches pattern (str/trim (str s)))]
            (apply f (map parse-long-or-nil parts))))
        interval-patterns))

(defn- unread
  [output why]
  {:read? false
   :error (if (str/blank? (or output ""))
            "no output"
            (str why ": " (first (str/split-lines (str/trim output)))))})

(defn parse-mac-detail
  "What FRR knows about one MAC, from `show evpn mac vni <vni> mac <mac>`.

  `:uptime` is how long FRR has held the entry in its current role, so for a
  local MAC it is how long the guest has been local on this host. FRR answering
  that it does not know the MAC is a read (`:absent`); silence, an unknown VNI or
  output of another shape is `:read? false`, because none of those says anything
  about the MAC."
  [output]
  (let [text (or output "")
        field (fn [re] (some->> (re-find re text) second))]
    (cond
      (str/includes? text "% Requested MAC does not exist")
      {:read? true :state :absent}

      (re-find #"(?m)^MAC: " text)
      (let [intf (field #"(?m)^\s*Intf: ([^\s(]+)")
            remote-vtep (field #"Remote VTEP: (\S+)")
            uptime (field #"(?m)^\s*Uptime: (\S+)")]
        {:read? true
         :state (cond (and intf (not= "-" intf)) :local
                      (or remote-vtep (str/includes? text "Remote ES:")) :remote
                      :else :other)
         :intf (when (not= "-" intf) intf)
         :remote-vtep remote-vtep
         :local-seq (some-> (field #"Local Seq: (\d+)") parse-long-or-nil)
         :remote-seq (some-> (field #"Remote Seq: (\d+)") parse-long-or-nil)
         :uptime uptime
         :uptime-seconds (uptime-seconds uptime)})

      :else (unread output "unrecognised output"))))

(def ^:private next-hop-pattern
  "A path's next-hop line in route_vty_out_detail: the address, optionally its
  hostname in parentheses, then the peer it came from."
  #"^\s+(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})(?:\([^)]*\))?(?:\s.*)?\sfrom\s")

(defn parse-bgp-mac-route
  "The next hop of every path BGP holds for one MAC, from
  `show bgp l2vpn evpn route vni <vni> mac <mac>`. `% Network not in table` is a
  read with no paths; silence, an unknown VNI or output of another shape is
  `:read? false`, so a failed read never passes for an absent route."
  [output]
  (let [text (or output "")]
    (cond
      (str/includes? text "% Network not in table")
      {:read? true :next-hops []}

      (str/includes? text "BGP routing table entry for")
      {:read? true
       :next-hops (vec (keep #(second (re-find next-hop-pattern %))
                             (str/split-lines text)))}

      :else (unread output "unrecognised output"))))

(defn fdb-conflicts
  "Every host holding both a local tap entry and a remote entry for one MAC, with
  the VNI read from the remote entry's overlay device. Tap rows repeat once per
  VNI scanned because the fdb table is per host, so the VNI comes from the remote
  row, which is kept only for its own overlay."
  [fdb]
  (for [[[host mac] entries] (group-by (juxt :host :mac) fdb)
        :let [tap (some #(when-not (:vxlan? %) %) entries)
              remote (some #(when (:dst %) %) entries)]
        :when (and tap remote)]
    {:host host
     :mac mac
     :vni (some->> (:dev remote) (re-matches vxlan-dev-pattern) second parse-long-or-nil)
     :local-intf (:dev tap)
     :stale-remote-vtep (:dst remote)}))

(defn host-commands
  "The three read-only commands, with sudo only where it is needed: vtysh talks to
  FRR's control socket and needs it, `bridge fdb show` reads the kernel's table and
  does not. gumshoe.ssh carries :needs-sudo? but does not apply it - probing sudo
  and using it are separate concerns - so the prefix belongs here."
  [connection vni]
  (let [sudo (if (:needs-sudo? connection) ["sudo"] [])]
    {:vni (vec (concat sudo ["vtysh" "-c" (str "show evpn vni " vni)]))
     :macs (vec (concat sudo ["vtysh" "-c" (str "show evpn mac vni " vni)]))
     :fdb ["bridge" "fdb" "show"]}))

(defn ssh-runner
  "The default transport: one remote command, stdout back.

  ssh/stdout-of is variadic, so the argv has to be spread rather than handed over
  as a single value - passing the vector makes the whole thing one remote token,
  which the far end cannot run and which fails by printing nothing."
  [connection argv]
  (apply ssh/stdout-of connection argv))

(defn collect-host
  "Everything one host knows about one VNI. Never throws: an unreachable host or a
  missing command comes back as `:error` and the caller turns that into a finding.
  A host whose own VTEP address could not be read keeps its tables but carries
  `:vtep-error`, because ownership of its guests cannot be judged without it.

  The transport is injectable because every defect found while building this lived
  in the seam rather than in the parsing - a pure test of the parsers passes
  happily while nothing can actually execute."
  ([connection vni]
   (collect-host connection vni ssh-runner (partial ssh/connects?)))
  ([{:keys [timeout-ms] :as connection} vni run reachable?]
   (let [cmds (host-commands connection vni)
         host (:host connection)]
     (binding [shell/*timeout-ms* (or timeout-ms 30000)]
       (if-not (reachable? connection)
         {:host host :vni vni :error "host is not reachable over ssh"}
         (let [out (reduce-kv (fn [acc k argv]
                                (assoc acc k (run connection argv)))
                              {}
                              cmds)
               host-vtep (parse-local-vtep (:vni out))]
           (if (str/blank? (or (:macs out) ""))
             {:host host :vni vni
              :error (str "no output from '" (str/join " " (:macs cmds))
                          "' - is FRR running, and does this account have sudo?")}
             (cond-> {:host host
                      :vni vni
                      :host-vtep host-vtep
                      :macs (vec (parse-macs vni host host-vtep (:macs out)))
                      :fdb (vec (parse-fdb vni host (:fdb out)))}
               (nil? host-vtep)
               (assoc :vtep-error
                      (str "no local VTEP address in the output of '"
                           (str/join " " (:vni cmds)) "'"))))))))))

(defn conflict-commands
  "The two reads one fdb conflict needs: FRR's view of the MAC, and the BGP
  paths behind it."
  [connection vni mac]
  (let [sudo (if (:needs-sudo? connection) ["sudo"] [])]
    {:mac-detail (vec (concat sudo ["vtysh" "-c" (str "show evpn mac vni " vni " mac " mac)]))
     :bgp-route (vec (concat sudo ["vtysh" "-c" (str "show bgp l2vpn evpn route vni " vni " mac " mac)]))}))

(defn collect-conflict-context
  "FRR's reading of each conflict's MAC, keyed by [host vni mac]. Only conflicts
  are read, two commands each, in parallel. Never throws: a host that does not
  answer leaves both reads `:read? false`."
  [connections conflicts run]
  (let [by-host (into {} (map (juxt :host identity)) connections)]
    (->> (for [{:keys [host vni mac]} conflicts
               :let [connection (get by-host host)]
               :when (and connection vni)]
           #(binding [shell/*timeout-ms* (or (:timeout-ms connection) 30000)]
              (let [cmds (conflict-commands connection vni mac)]
                [[host vni mac]
                 {:mac-detail (parse-mac-detail (run connection (:mac-detail cmds)))
                  :bgp-route (parse-bgp-mac-route (run connection (:bgp-route cmds)))}])))
         concurrent/all
         (into {}))))

(defn host-evidence
  "The per-host results flattened into the evidence the detectives read. The
  MAC and fdb tables are nil when no host answered: an empty table would read
  as a fabric without conflicts where nobody looked, so the checks over them
  are unread instead. Once any host answered, the tables hold what the
  answering hosts reported, and evpn-hosts names the others."
  [results]
  (let [answered (remove :error results)]
    {"evpn-macs" (when (seq answered) (vec (mapcat :macs answered)))
     "evpn-fdb" (when (seq answered) (vec (mapcat :fdb answered)))
     "evpn-hosts" results}))

(defn collect-evidence!
  "Fan out across every host and VNI, then flatten into the evidence a detective
  reads. Hosts are queried in parallel because a fabric scan is otherwise as slow
  as the sum of its hosts, and a partitioned fabric is when speed matters. The
  fdb conflicts found get a second, bounded round of reads for their context."
  [connections vnis]
  (let [results (concurrent/all (for [c connections vni vnis] #(collect-host c vni)))
        evidence (host-evidence results)]
    (assoc evidence "evpn-fdb-context"
           (when-let [fdb (get evidence "evpn-fdb")]
             (collect-conflict-context connections (fdb-conflicts fdb) ssh-runner)))))

(defn connections
  "Connection maps from a host list, carrying the sudo and timeout settings the
  whole scan shares."
  [hosts {:keys [user needs-sudo? timeout-ms]}]
  (for [host hosts]
    (cond-> {:host host :needs-sudo? (if (nil? needs-sudo?) true needs-sudo?)}
      user (assoc :user user)
      timeout-ms (assoc :timeout-ms timeout-ms))))

;; --- the values a scan judges against -----------------------------------------

(def inputs
  "The values a fabric scan reads from its options or from env.edn: the
  option that supplies each, where env.edn keeps it, how one option item is
  read, and the evidence key a detective judges it under. A value an option
  names wins over env.edn, so a shell without a kubectl context - which
  selects no env.edn environment - can still supply every one."
  [{:input "hosts" :option :hosts :env-path [:evpn :hosts]}
   {:input "vnis" :option :vni :env-path [:evpn :vnis] :parse parse-long}
   {:input "gateway-vteps" :option :gateway-vteps :env-path [:evpn :gateway-vteps]
    :evidence "evpn-gateway-vteps"}
   {:input "guest-mac-prefixes" :option :guest-mac-prefixes :env-path [:evpn :guest-mac-prefixes]
    :evidence "evpn-guest-mac-prefixes"}])

(defn split-list
  "The items of a comma-separated option value, trimmed, blanks dropped."
  [s]
  (when-not (nil? s)
    (->> (str/split (str s) #",")
         (map str/trim)
         (remove str/blank?)
         vec)))

(defn resolve-input
  "One input's value and where it came from: {:value (..) :source \"option\"}
  when its option names any item, else {:value (..) :source \"env.edn\"} when
  env-lookup (a (fn [path])) holds a non-empty list, else {:source
  \"missing\"}. An item :parse cannot read is dropped."
  [{:keys [option env-path parse]} opts env-lookup]
  (let [from-option (seq (keep (or parse identity) (split-list (get opts option))))
        from-env (seq (env-lookup env-path))]
    (cond from-option {:value from-option :source "option"}
          from-env {:value from-env :source "env.edn"}
          :else {:source "missing"})))

(defn resolve-inputs
  "Every input resolved against opts and env-lookup, keyed by input name."
  [opts env-lookup]
  (into {} (for [declared inputs]
             [(:input declared) (resolve-input declared opts env-lookup)])))

(defn input-evidence
  "The evidence the resolved inputs give the detectives: each judged value as
  a vector under its evidence key, nil when missing, and \"evpn-inputs\"
  recording where every value came from."
  [resolved]
  (into {"evpn-inputs" (vec (for [{:keys [input option env-path]} inputs]
                              {:input input
                               :option (str "--" (name option))
                               :env-path env-path
                               :source (get-in resolved [input :source])}))}
        (for [{:keys [input evidence]} inputs
              :when evidence]
          [evidence (some-> (get-in resolved [input :value]) vec)])))
