;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.evpn
  "Reads an EVPN-VXLAN fabric's MAC ownership from the hosts themselves.

  Every VTEP in a fabric holds its own opinion about where a MAC lives, and a
  partition is exactly the case where those opinions differ. So the evidence is
  gathered per host and kept per host - asking one host, or asking a router,
  cannot show a disagreement.

  Read-only throughout: three `show`-style commands per host, no configuration
  is touched. Hosts are queried in parallel and a host that cannot be reached
  becomes evidence rather than an aborted scan, because the unreachable host is
  often the symptom being investigated."
  (:require [clojure.string :as str]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]))

(def ^:private mac-pattern #"(?i)^[0-9a-f]{2}(:[0-9a-f]{2}){5}$")
(def ^:private ipv4-pattern #"\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}")
(def ^:private flags-pattern #"^[NIPX]+$")
(def ^:private seq-pattern #"^(\d+)/(\d+)$")

(defn- parse-long-or-nil
  [s]
  (try (Long/parseLong s) (catch Exception _ nil)))

(defn parse-local-vtep
  "The host's own VTEP address, from `show evpn vni <vni>`. Matched loosely - any
  address on a line mentioning VTEP - so a column reshuffle between FRR releases
  does not silently yield the wrong host."
  [output]
  (some (fn [line]
          (when (str/includes? line "VTEP")
            (re-find ipv4-pattern line)))
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

(defn parse-fdb
  "Rows of `bridge fdb show`, kept only for the VXLAN device of this VNI and for
  the taps that carry the same MACs.

  `dst` marks an entry that sends the MAC across the fabric; its absence on a
  non-VXLAN device marks the local tap. Both existing for one MAC on one host is
  the contradiction the fdb-conflict detective looks for."
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
                flags (set (rest tokens))]
          :when dev]
      {:host host
       :mac (str/lower-case mac)
       :dev dev
       :dst dst
       :vxlan? (= dev vxlan-dev)
       :static? (contains? flags "static")
       :extern-learn? (contains? flags "extern_learn")})))

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

(defn collect-host
  "Everything one host knows about one VNI. Never throws: an unreachable host or a
  missing command comes back as `:error` and the caller turns that into a finding."
  [{:keys [timeout-ms] :as connection} vni]
  (let [cmds (host-commands connection vni)
        host (:host connection)]
    (binding [shell/*timeout-ms* (or timeout-ms 30000)]
      (if-not (ssh/connects? connection)
        {:host host :vni vni :error "host is not reachable over ssh"}
        (let [out (reduce-kv (fn [acc k argv]
                               (assoc acc k (ssh/stdout-of connection argv)))
                             {}
                             cmds)
              host-vtep (parse-local-vtep (:vni out))]
          (if (str/blank? (or (:macs out) ""))
            {:host host :vni vni
             :error (str "no output from '" (str/join " " (:macs cmds))
                         "' - is FRR running, and does this account have sudo?")}
            {:host host
             :vni vni
             :host-vtep host-vtep
             :macs (vec (parse-macs vni host host-vtep (:macs out)))
             :fdb (vec (parse-fdb vni host (:fdb out)))}))))))

(defn collect-evidence!
  "Fan out across every host and VNI, then flatten into the evidence a detective
  reads. Hosts are queried in parallel because a fabric scan is otherwise as slow
  as the sum of its hosts, and a partitioned fabric is when speed matters."
  [connections vnis]
  (let [results (->> (for [c connections vni vnis]
                       (future (collect-host c vni)))
                     doall
                     (mapv deref))]
    {"evpn-macs" (vec (mapcat :macs results))
     "evpn-fdb" (vec (mapcat :fdb results))
     "evpn-hosts" results}))

(defn connections
  "Connection maps from a host list, carrying the sudo and timeout settings the
  whole scan shares."
  [hosts {:keys [user needs-sudo? timeout-ms]}]
  (for [host hosts]
    (cond-> {:host host :needs-sudo? (if (nil? needs-sudo?) true needs-sudo?)}
      user (assoc :user user)
      timeout-ms (assoc :timeout-ms timeout-ms))))
