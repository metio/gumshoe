;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.evpn
  "Reads an EVPN-VXLAN fabric's MAC ownership from the hosts themselves.

  Every VTEP in a fabric holds its own opinion about where a MAC lives, and a
  partition is exactly the case where those opinions differ. So the evidence is
  gathered per host and kept per host - asking one host, or asking a router,
  cannot show a disagreement.

  Read-only throughout: three `show`-style commands per host, plus two per fdb
  conflict found and one or two per remote VTEP a learned fdb row names, and no
  configuration is touched. Hosts are queried in parallel and a host that cannot be reached
  becomes evidence rather than an aborted scan, because the unreachable host is
  often the symptom being investigated."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
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

(def ^:private fdb-valued-keys
  "The words iproute2's `bridge fdb show` prints before a value (print_fdb in
  bridge/fdb.c): `dev`, `dst`, `vlan`, `port`, `vni`, `src_vni`, `via`, `nhid`,
  `link-netnsid` and `master <bridge>`."
  #{"dev" "dst" "vlan" "port" "vni" "src_vni" "via" "nhid" "link-netnsid" "master"})

(def ^:private fdb-state-words
  "The NUD state word print_fdb ends a row with: `permanent` (NUD_PERMANENT),
  `static` (NUD_NOARP) or `stale` (NUD_STALE). A reachable entry prints none,
  and any other state prints as `state=0x…`."
  #{"permanent" "static" "stale"})

(def ^:private fdb-flag-words
  "The flag words print_fdb prints after the values, from ndm_flags and, with
  details, the activity bits. `master` doubles as the flag NTF_MASTER, printed
  bare, and as the key of `master <bridge>`."
  #{"self" "router" "extern_learn" "offload" "master" "sticky" "locked"
    "activity_notify" "inactive"})

(defn- bare-word?
  [token]
  (or (nil? token)
      (contains? fdb-valued-keys token)
      (contains? fdb-state-words token)
      (contains? fdb-flag-words token)
      (str/starts-with? token "state=")))

(defn parse-fdb-line
  "One row of `bridge fdb show` as its MAC, its keyed values, its flag words and
  its state, or nil for a line that does not start with a MAC.

  Words are read by what they are rather than by position, so a row is read the
  same whatever subset of fields iproute2 printed: `master` followed by a bridge
  name is the bridge, `master` followed by another keyword is the bare flag, and
  `via ifindex N` (printed when the device sits in another namespace) keeps both
  words. A word nothing here knows is kept among the flags rather than dropped,
  so it still tells two otherwise equal rows apart."
  [line]
  (let [tokens (str/split (str/trim (or line "")) #"\s+")
        mac (first tokens)]
    (when (and mac (re-matches mac-pattern mac))
      (loop [[token & more] (rest tokens)
             row {:mac (str/lower-case mac) :values {} :flags #{} :state nil}]
        (cond
          (nil? token) row

          (and (= "via" token) (= "ifindex" (first more)) (second more))
          (recur (drop 2 more) (assoc-in row [:values "via"] (str "ifindex " (second more))))

          (and (contains? fdb-valued-keys token) (not (bare-word? (first more))))
          (recur (rest more) (assoc-in row [:values token] (first more)))

          (or (contains? fdb-state-words token) (str/starts-with? token "state="))
          (recur more (assoc row :state token))

          :else
          (recur more (update row :flags conj token)))))))

(defn fdb-row
  "One row of `bridge fdb show` on host as the checks read it, with no VNI
  read attached, or nil for a line that does not start with a MAC or names no
  device."
  [host line]
  (let [{:keys [mac values flags state]} (parse-fdb-line line)
        dev (get values "dev")]
    (when (and mac dev)
      {:host host
       :mac mac
       :dev dev
       :dst (get values "dst")
       :vlan (get values "vlan")
       :entry-vni (get values "vni")
       :port (get values "port")
       :via (get values "via")
       :master (get values "master")
       :self? (contains? flags "self")
       :state state
       :flags flags
       :line (str/trim line)
       :static? (= "static" state)
       :extern-learn? (contains? flags "extern_learn")})))

(defn parse-fdb
  "Rows of `bridge fdb show`, kept only for this VNI: its own VXLAN device and the
  guest taps.

  Every overlay on the host shows up in one table, so another VNI's VXLAN device
  has to be dropped rather than mistaken for a tap - otherwise every legitimate
  remote entry on a neighbouring VNI pairs with its own overlay device and reads as
  a conflict. A tap is any device that is not an `lo.<vni>`.

  `dst` marks an entry that sends the MAC across the fabric; its absence on a tap
  marks local ownership. Both existing for one MAC on one host is the contradiction
  the fdb-conflict detective looks for.

  One host's table is read once per VNI scanned, so a tap row comes back once per
  read. Each row carries the VNI whose read produced it as `:read-vni`, and its
  trimmed text as `:line`, so a check can tell a row the table printed twice from
  one row read twice."
  [vni host output]
  (let [vxlan-dev (str "lo." vni)]
    (for [line (str/split-lines (or output ""))
          :let [{:keys [dev] :as row} (fdb-row host line)
                other-overlay? (and dev
                                    (re-matches vxlan-dev-pattern dev)
                                    (not= dev vxlan-dev))]
          :when (and row (not other-overlay?))]
      (assoc row
             :read-vni vni
             :vxlan? (= dev vxlan-dev)))))

(defn overlay-vni
  "The VNI an `lo.<vni>` overlay device is named after, or nil for any other
  device, a guest tap or a bridge among them."
  [dev]
  (some->> dev (re-matches vxlan-dev-pattern) second parse-long-or-nil))

(defn parse-fdb-table
  "Every row of one host's `bridge fdb show`, whatever device or VNI it
  belongs to, in the order the table printed them."
  [host output]
  (vec (keep #(fdb-row host %) (str/split-lines (or output "")))))

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

;; --- remote VTEPs and the BGP paths that name them ---------------------------

(defn row-vni
  "The VNI an fdb row belongs to: its `vni` value when it prints one, else the
  VNI its `lo.<vni>` overlay device is named after, else nil."
  [{:keys [entry-vni dev]}]
  (or (some-> entry-vni parse-long-or-nil)
      (some->> dev (re-matches vxlan-dev-pattern) second parse-long-or-nil)))

(defn learned-remote-row?
  "Whether an fdb row sends a MAC across the fabric because something learned
  it there: it names a destination VTEP and is neither a static pin, which
  evpn-static-fdb judges, nor a permanent entry, which nobody learned."
  [{:keys [dst state]}]
  (boolean (and dst (not (contains? #{"static" "permanent"} state)))))

(defn remote-vteps
  "Every [host vni vtep] a learned remote fdb row names, each once, in the
  order the rows name them: the VTEPs whose BGP paths decide whether those rows
  are orphans."
  [fdb]
  (->> fdb
       (filter map?)
       (filter learned-remote-row?)
       (keep (fn [row]
               (when-let [vni (row-vni row)]
                 [(:host row) vni (:dst row)])))
       distinct
       vec))

(defn vtep-commands
  "The reads that count the BGP paths naming one VTEP in one VNI. `show bgp vni
  … vtep …` walks both of the VNI's tables, MAC-only type-2 routes included;
  FRR before 8.5 has no such command and keeps one table per VNI, which
  `show bgp l2vpn evpn route vni … vtep …` walks whole."
  [connection vni vtep]
  (let [sudo (if (:needs-sudo? connection) ["sudo"] [])]
    {:both-tables (vec (concat sudo ["vtysh" "-c" (str "show bgp vni " vni " vtep " vtep " json")]))
     :one-table (vec (concat sudo ["vtysh" "-c" (str "show bgp l2vpn evpn route vni " vni
                                                     " vtep " vtep " json")]))}))

(defn- unknown-command?
  [output]
  (str/includes? (or output "") "% Unknown command"))

(defn parse-vtep-paths
  "How many BGP paths in one VNI name a VTEP as their next hop, from the JSON
  of either read in vtep-commands: `numPaths`, plus the MAC table's own
  `numPaths` when the output carries a `macTable`. FRR counts only the paths
  whose next hop is the VTEP, while its `numPrefix` counts every prefix the
  table holds, so the prefix count says nothing here.

  A count of zero is a read that found no path. Anything else - silence, an
  unknown command, a VNI BGP does not know (FRR then prints the JSON without
  any count), a MAC table without its count, output that is no JSON - is
  `:read? false`, so a failed read never passes for an absent path."
  [output]
  (let [text (or output "")
        parsed (try (json/parse-string text) (catch Exception _ ::unparsable))
        paths (fn [m] (let [n (when (map? m) (get m "numPaths"))]
                        (when (integer? n) n)))]
    (cond
      (str/blank? text) (unread output "no output")
      (unknown-command? text) (assoc (unread output "unknown command") :unknown-command? true)
      (not (map? parsed)) (unread output "unrecognised output")
      (nil? (paths parsed)) {:read? false :error "BGP printed no path count, so it does not know this VNI"}
      (and (contains? parsed "macTable") (nil? (paths (get parsed "macTable"))))
      {:read? false :error "BGP printed no path count for the VNI's MAC table"}
      :else {:read? true
             :paths (+ (paths parsed) (or (paths (get parsed "macTable")) 0))})))

(defn read-vtep-paths
  "The BGP path count for one VTEP in one VNI over `run`. The single-table read
  runs only when FRR answered that it does not know the two-table command,
  because on an FRR that has it the single-table read misses MAC-only routes
  and would report a live VTEP as advertising nothing."
  [connection vni vtep run]
  (let [cmds (vtep-commands connection vni vtep)
        both (parse-vtep-paths (run connection (:both-tables cmds)))]
    (if (:unknown-command? both)
      (parse-vtep-paths (run connection (:one-table cmds)))
      both)))

(defn collect-vtep-paths
  "The BGP path count of every VTEP a learned remote fdb row names, keyed by
  [host vni vtep], read on that host in parallel. Never throws: a host that
  does not answer leaves its counts `:read? false`."
  [connections fdb run]
  (let [by-host (into {} (map (juxt :host identity)) connections)]
    (->> (for [[host vni vtep :as k] (remote-vteps fdb)
               :let [connection (get by-host host)]
               :when connection]
           #(binding [shell/*timeout-ms* (or (:timeout-ms connection) 30000)]
              [k (read-vtep-paths connection vni vtep run)]))
         concurrent/all
         (into {}))))

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
  fdb conflicts found get a second, bounded round of reads for their context,
  and every remote VTEP a learned fdb row names gets its BGP paths counted."
  [connections vnis]
  (let [results (concurrent/all (for [c connections vni vnis] #(collect-host c vni)))
        evidence (host-evidence results)
        fdb (get evidence "evpn-fdb")]
    (assoc evidence
           "evpn-fdb-context"
           (when fdb (collect-conflict-context connections (fdb-conflicts fdb) ssh-runner))
           "evpn-vtep-paths"
           (when fdb (collect-vtep-paths connections fdb ssh-runner)))))

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
