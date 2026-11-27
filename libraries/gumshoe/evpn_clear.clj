;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.evpn-clear
  "Clears the learned fdb rows that send MACs to a VTEP no BGP path
  advertises, the rows evpn-orphan-fdb reports. runbooks/fabric/
  clear_orphan_fdb.clj is the change book; this namespace holds what it reads,
  how it judges and what it deletes, so each part is testable without a host.

  A host's reading is its whole `bridge fdb show` table and, for every VNI a
  learned row naming the VTEP belongs to, BGP's path count for that VTEP in
  that VNI. The table read needs no sudo; vtysh does.

  The judgement is strict, most cautious first:
    - a row is a candidate only on a host where BGP answered an explicit
      count of 0 for the VTEP in every VNI the host's rows name it in;
    - a host whose table or any of those counts could not be read is
      unjudged: its rows are listed and none is deleted;
    - a VTEP any host reads a path for is live, and nothing naming it is
      deleted anywhere;
    - static and permanent rows are pins nobody learned (evpn-static-fdb
      reports them) and are kept, as is every row on a device that is not an
      `lo.<vni>` overlay, guest taps such as `one-247-0` included.

  Per MAC the deletes are the `dst <vtep> self` row and the bridge rows the
  MAC has on the same overlay device (`vlan 1 … master <bridge>` and the
  plain `… master <bridge>`), with the device, vlan and VNI read from the
  rows. A MAC whose device also holds a destination row naming another VTEP
  keeps its bridge rows, since those carry that other destination too.

  Each delete is its own ssh command, run by the :fdb-del effect registered
  here, so its output is shown under its own line and a row that is already
  gone - iproute2 answers `No such file or directory` - counts as deleted."
  (:require [clojure.string :as str]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.detectives.evpn :as evpn-detectives]
            [gumshoe.effect :as effect]
            [gumshoe.evpn :as evpn]
            [gumshoe.reads.result :as result]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]
            [gumshoe.tally :as tally]))

;; --- the VTEP an operator names --------------------------------------------------

(def ^:private ipv4-pattern #"^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$")

(defn- ipv4?
  [s]
  (when-let [[_ & octets] (re-matches ipv4-pattern s)]
    (every? #(<= 0 (parse-long %) 255) octets)))

(defn- ipv6?
  "Whether s is an IPv6 address written in hex groups: at most eight groups of
  up to four hex digits, and at most one `::` standing for the groups left
  out."
  [s]
  (and (re-matches #"^[0-9a-fA-F:]+$" s)
       (str/includes? s ":")
       (let [halves (str/split s #"::" -1)
             groups (fn [h] (if (str/blank? h) [] (str/split h #":" -1)))
             all (mapcat groups halves)]
         (and (<= (count halves) 2)
              (every? #(re-matches #"[0-9a-fA-F]{1,4}" %) all)
              (if (= 2 (count halves)) (< (count all) 8) (= 8 (count all)))))))

(defn vtep-problem
  "Why s cannot name a VTEP, or nil when it is an IPv4 or IPv6 address. A VTEP
  is an underlay address, so a hostname or a pattern is refused rather than
  matched loosely."
  [s]
  (cond
    (str/blank? (str s)) "no VTEP was named"
    (or (ipv4? s) (ipv6? s)) nil
    :else (format "%s is not an IPv4 or IPv6 address - name the VTEP by its address, as `bridge fdb show` prints it after dst" (pr-str s))))

;; --- reading the hosts --------------------------------------------------------------

(def fdb-command
  "The table read, which needs no sudo."
  ["bridge" "fdb" "show"])

(defn remote-result
  "{:exit :out :err} of argv run on the host over ssh. Never throws: a command
  that could not be started answers as a failure."
  [connection argv]
  (binding [shell/*timeout-ms* (or (:timeout-ms connection) 30000)]
    (try
      (apply shell/execute (ssh/ssh-args connection argv))
      (catch Exception e
        {:exit -1 :out "" :err (or (ex-message e) (str (type e)))}))))

(defn- first-line
  [{:keys [exit err out]}]
  (or (some->> [err out] (map #(str/trim (str %))) (remove str/blank?) first str/split-lines first)
      (str "exit " exit)))

(defn- vtysh-output
  "The output gumshoe.evpn/read-vtep-paths parses: stdout of a command that
  succeeded, or of one that failed by answering `% Unknown command`, which
  is how vtysh says the FRR is older than `show bgp vni`. Any other failure
  answers nothing, which the parser reads as unread."
  [run]
  (fn [connection argv]
    (let [{:keys [exit out]} (run connection argv)]
      (if (or (zero? exit) (str/includes? (str out) "% Unknown command"))
        (str out)
        ""))))

(defn- naming
  "The learned rows of a table that send a MAC to vtep."
  [rows vtep]
  (filter #(and (evpn/learned-remote-row? %) (= vtep (:dst %))) rows))

(defn read-host
  "One host's reading for the VTEPs `vteps-of` picks from its table: the
  table, and for each [vni vtep] a learned row names, BGP's path count.
  `run` is (fn [connection argv] -> {:exit :out :err}). Never throws."
  [connection vteps-of run]
  (let [host (:host connection)
        result (run connection fdb-command)]
    (if-not (zero? (:exit result))
      {:host host :read? false :error (first-line result)}
      (let [rows (evpn/parse-fdb-table host (:out result))
            keys (->> (vteps-of rows)
                      (mapcat (fn [vtep] (for [vni (distinct (keep evpn/row-vni (naming rows vtep)))]
                                           [vni vtep])))
                      distinct)
            vtysh (vtysh-output run)]
        {:host host
         :read? true
         :output (:out result)
         :rows rows
         :paths (into {} (for [[vni vtep :as k] keys]
                           [k (evpn/read-vtep-paths connection vni vtep vtysh)]))}))))

(defn read-fabric
  "Every host's reading for one VTEP, in host order, read in parallel."
  [connections vtep run]
  (concurrent/all (for [c connections] #(read-host c (constantly [vtep]) run))))

(defn scan-fabric
  "Every host's reading for every VTEP its learned rows name, in host order,
  read in parallel: what an operator picks a VTEP from."
  [connections run]
  (concurrent/all (for [c connections]
                    #(read-host c (fn [rows] (distinct (keep :dst (filter evpn/learned-remote-row? rows)))) run))))

;; --- judging --------------------------------------------------------------------------

(defn- overlay-row?
  [row]
  (some? (evpn/overlay-vni (:dev row))))

(defn- bridge-row?
  "A row the bridge sends a MAC out of an overlay device with, naming no VTEP,
  and which something learned: neither static nor permanent."
  [{:keys [dst master state] :as row}]
  (and (nil? dst) master (overlay-row? row)
       (not (contains? #{"static" "permanent"} state))))

(defn- delete-argv
  "The `bridge fdb del` that removes exactly this row: a row with a
  destination lives in the overlay device's own table and is addressed with
  `self` and its `dst`, a bridge row with `master` and its `vlan`."
  [{:keys [mac dev dst vlan entry-vni]}]
  (vec (concat ["bridge" "fdb" "del" mac "dev" dev]
               (when dst ["dst" dst])
               (when entry-vni ["vni" entry-vni])
               (when vlan ["vlan" vlan])
               [(if dst "self" "master")])))

(defn- delete-of
  [kind row]
  {:kind kind
   :mac (:mac row)
   :dev (:dev row)
   :vni (evpn/row-vni row)
   :bridge (:master row)
   :row (:line row)
   :argv (delete-argv row)})

(defn- pins
  "The deletes per [dev mac] of the rows naming vtep: each dst row, then the
  MAC's bridge rows on that device, those with a vlan first. A device that
  also sends the MAC to another VTEP keeps the bridge rows."
  [rows named vtep]
  (vec
   (for [[[dev mac] dst-rows] (sort-by key (group-by (juxt :dev :mac) named))
         :let [same (filter #(and (= dev (:dev %)) (= mac (:mac %))) rows)
               elsewhere (filter #(and (:dst %) (not= vtep (:dst %))) same)
               bridged (when (empty? elsewhere)
                         (sort-by #(if (:vlan %) 0 1) (filter bridge-row? same)))]]
     {:mac mac
      :dev dev
      :vni (evpn/row-vni (first dst-rows))
      :bridge (some :master same)
      :kept-bridge-rows (when (seq elsewhere) (vec (keep :line (filter bridge-row? same))))
      :deletes (vec (concat (map #(delete-of :dst %) (distinct dst-rows))
                            (map #(delete-of :bridge %) (distinct bridged))))})))

(defn judge-host
  "What may be cleared on one host for vtep, as {:host :status ..}:
    :unread      the table could not be read (:reason);
    :clear       no learned row names the VTEP;
    :unjudged    a row naming it lies outside an overlay device or any VNI,
                 or a path count did not answer (:reason, :rows);
    :advertised  BGP reads a path for it in a VNI (:vnis);
    :orphaned    BGP read 0 paths in every VNI its rows lie in (:pins).
  Every status but :unread names the static or permanent rows naming the VTEP
  as :kept-pins, and :orphaned names the tap rows of its MACs as :kept-taps."
  [{:keys [host read? error rows paths]} vtep]
  (if-not read?
    {:host host :status :unread :reason (str "its fdb table could not be read: " error)}
    (let [named (vec (distinct (naming rows vtep)))
          kept-pins (vec (keep #(when (and (= vtep (:dst %)) (not (evpn/learned-remote-row? %))) (:line %)) rows))
          vnis (vec (distinct (map evpn/row-vni named)))
          readings (for [vni vnis :when vni] [vni (get paths [vni vtep])])
          unread (filter #(not (:read? (second %))) readings)
          lines (mapv :line named)
          base {:host host :kept-pins kept-pins}]
      (cond
        (empty? named)
        (assoc base :status :clear)

        (some (complement overlay-row?) named)
        (assoc base :status :unjudged :rows lines
               :reason (str "a row naming " vtep " sits on "
                            (str/join ", " (distinct (map :dev (remove overlay-row? named))))
                            ", which is no lo.<vni> overlay device"))

        (some nil? vnis)
        (assoc base :status :unjudged :rows lines
               :reason (str "a row naming " vtep " belongs to no VNI BGP could be asked about"))

        (seq unread)
        (assoc base :status :unjudged :rows lines
               :reason (str "BGP's path count for " vtep " could not be read in "
                            (str/join "; " (for [[vni reading] unread]
                                             (str "VNI " vni " (" (or (:error reading) "not read") ")")))))

        (some #(pos? (:paths (second %))) readings)
        (assoc base :status :advertised :rows lines
               :vnis (vec (for [[vni reading] readings :when (pos? (:paths reading))] vni)))

        :else
        (let [macs (set (map :mac named))]
          (assoc base :status :orphaned :vnis vnis
                 :pins (pins rows named vtep)
                 :kept-taps (vec (keep #(when (and (contains? macs (:mac %)) (not (overlay-row? %)) (nil? (:dst %)))
                                          (:line %))
                                       rows))))))))

(defn judge-fabric
  "Every host's judgement for vtep, in host order, with the hosts by status."
  [readings vtep]
  (let [hosts (mapv #(judge-host % vtep) readings)
        by (group-by :status hosts)]
    {:vtep vtep
     :hosts hosts
     :orphaned (vec (:orphaned by))
     :advertised (vec (:advertised by))
     :unjudged (vec (concat (:unread by) (:unjudged by)))}))

(defn deletes
  "Every delete of a judged host, in order."
  [{:keys [pins]}]
  (vec (mapcat :deletes pins)))

(defn candidates
  "The VTEPs a scan found orphaned on at least one host and advertised on
  none, sorted. Throws an ex-info carrying gumshoe.kubectl's unread mark when
  it found none while a host's table or a path count did not answer, so an
  unread fabric is never reported as having nothing to clear."
  [readings]
  (let [vteps (sort (distinct (for [{:keys [rows]} readings
                                    row (filter evpn/learned-remote-row? rows)]
                                (:dst row))))
        judged (map #(judge-fabric readings %) vteps)
        found (vec (for [{:keys [vtep orphaned advertised]} judged
                         :when (and (seq orphaned) (empty? advertised))]
                     vtep))
        unread (distinct (concat (keep #(when-not (:read? %) (str (:host %) ": " (:error %))) readings)
                                 (for [{:keys [unjudged]} judged
                                       {:keys [host status reason]} unjudged
                                       :when (= :unjudged status)]
                                   (str host ": " reason))))]
    (if (and (empty? found) (seq unread))
      (throw (result/unread-exception
              (str "no VTEP could be judged orphaned while some of the fabric could not be read - "
                   (str/join "; " unread))))
      found)))

(defn cleared?
  "Whether a judged fabric holds nothing left to clear: every host's table was
  read and none holds a learned row naming the VTEP."
  [{:keys [hosts]}]
  (every? #(= :clear (:status %)) hosts))

(defn- host-names
  [hosts]
  (str/join ", " (map :host hosts)))

(defn- advertised-text
  [advertised]
  (str/join "; " (for [{:keys [host vnis]} advertised]
                   (str "in VNI " (str/join ", " vnis) " on " host))))

(defn- touched
  "{host -> its deletes} of a judged fabric's orphaned hosts."
  [{:keys [orphaned]}]
  (into {} (for [h orphaned] [(:host h) (deletes h)])))

(defn refusal
  "Why the change must not run on the fabric as judged now, or nil.
  `planned` is the judgement the confirmation showed; its touched hosts must
  hold exactly the deletes it planned, or the change is refused naming them.
  Before the confirmation the two are the same judgement."
  [{:keys [vtep advertised orphaned unjudged] :as now} planned]
  (let [moved (when planned
                (let [then (touched planned) fresh (touched now)]
                  (vec (for [host (keys then) :when (not= (get then host) (get fresh host))] host))))]
    (cond
      (seq advertised)
      (str "BGP advertises " vtep " " (advertised-text advertised)
           ", so the VTEP is live and the rows naming it are correct - nothing was deleted")

      (seq moved)
      (str "the rows naming " vtep " on " (str/join ", " (sort moved))
           " are no longer the ones confirmed, or can no longer be judged - nothing was deleted; run the book again to see them as they are now")

      (empty? orphaned)
      (if (seq unjudged)
        (str "no row naming " vtep " can be judged, so none is deleted: "
             (str/join "; " (for [{:keys [host reason]} unjudged] (str host " - " reason))))
        (str "no scanned host holds a learned row naming " vtep)))))

;; --- what the confirmation shows ---------------------------------------------------

(defn- counted
  [n one many]
  (tally/counted n one many))

(defn- host-line
  [{:keys [host vnis pins kept-pins kept-taps] :as h}]
  (let [ds (deletes h)
        bridges (distinct (keep #(when (:bridge %) (str (:dev %) " master " (:bridge %))) pins))]
    (str host ": delete " (counted (count ds) "row" "rows")
         " (" (counted (count (filter #(= :dst (:kind %)) ds)) "dst row" "dst rows") ", "
         (counted (count (filter #(= :bridge (:kind %)) ds)) "bridge row" "bridge rows") ")"
         " for " (counted (count pins) "MAC" "MACs") " in VNI " (str/join ", " vnis)
         (when (seq bridges) (str " via " (str/join ", " bridges)))
         ": " (str/join ", " (map :mac pins))
         (when (seq kept-taps) (str "; keeps " (counted (count kept-taps) "tap row" "tap rows")))
         (when (seq kept-pins) (str "; keeps " (counted (count kept-pins) "static or permanent pin" "static or permanent pins"))))))

(defn items
  "The confirmation's lines: the VTEP, one line per host the change deletes
  on, and one per host it leaves alone and why."
  [{:keys [vtep hosts orphaned]}]
  (vec (concat
        [(str "VTEP " vtep " - no BGP path advertises it on "
              (counted (count orphaned) "host" "hosts") ", "
              (counted (reduce + (map (comp count deletes) orphaned)) "row" "rows") " to delete")]
        (for [{:keys [host status reason rows] :as h} hosts]
          (case status
            :orphaned (host-line h)
            :clear (str host ": no learned row names " vtep " - nothing to delete")
            :unread (str host ": unjudged, deletes nothing - " reason)
            :unjudged (str host ": unjudged, deletes nothing - " reason "; "
                           (counted (count rows) "row" "rows") " left: " (str/join " | " rows))
            :advertised (str host ": " vtep " is advertised - deletes nothing"))))))

;; --- the deletes ---------------------------------------------------------------------

(defn already-gone?
  "Whether a failed delete failed because the row is not there: iproute2
  answers ENOENT as `No such file or directory`. An ssh failure (exit 255)
  is never read so, whatever it printed."
  [{:keys [exit out err]}]
  (boolean (and (not (zero? exit)) (not= 255 exit)
                (re-find #"No such file or directory" (str err "\n" out)))))

(defn delete-command
  "The remote argv of one delete on connection: sudo -n, because changing the
  table needs root and a password prompt must fail rather than hang."
  [connection argv]
  (vec (concat (when (:needs-sudo? connection) ["sudo" "-n"]) argv)))

(defn fdb-del
  "The effect that runs one delete over ssh."
  [connection argv]
  [:fdb-del connection (delete-command connection argv)])

(defn- describe-delete
  [[connection argv]]
  (str "ssh " (ssh/target connection) " -- " (str/join " " argv)))

(defn- perform-delete
  "Runs one delete and shows what it printed. A row already gone counts as
  deleted; any other failure stops the plan."
  [[connection argv]]
  (let [{:keys [exit out err] :as result} (remote-result connection argv)]
    (doseq [l (remove str/blank? (str/split-lines (str out "\n" err)))]
      (stdout/detail l))
    (cond
      (zero? exit) true
      (already-gone? result) (do (stdout/detail "the row is already gone - counted as deleted") true)
      :else (do (stdout/error (format "the delete failed with exit %s: %s" exit (first-line result))) false))))

(effect/register-effect-type! :fdb-del {:describe describe-delete :perform perform-delete})

(defn plan
  "The effect plan of a judged fabric: per orphaned host a note, then each
  delete as its own command. `connection-of` maps a host to its connection."
  [{:keys [vtep orphaned]} connection-of]
  (vec (mapcat (fn [{:keys [host] :as h}]
                 (let [conn (connection-of host)]
                   (cons (effect/note (str host ": clear the rows naming " vtep))
                         (map #(fdb-del conn (:argv %)) (deletes h)))))
               orphaned)))

;; --- the post-check -----------------------------------------------------------------

(defn evidence
  "The evidence the fabric detectives read, from one host's reading: its table
  as each overlay VNI's read keeps it, and the path counts read for it."
  [{:keys [host output rows paths]}]
  (let [vnis (sort (distinct (keep #(evpn/overlay-vni (:dev %)) rows)))]
    {"evpn-fdb" (vec (mapcat #(evpn/parse-fdb % host output) vnis))
     "evpn-vtep-paths" (into {} (for [[[vni vtep] reading] paths] [[host vni vtep] reading]))}))

(defn remaining
  "What still names vtep on a host after the deletes, as reasons; empty when
  it is cleared. A table that could not be read is a reason, so the check
  fails closed. Besides the learned rows, evpn-orphan-fdb and
  evpn-fdb-conflict are run over the host's evidence, and every finding of
  theirs naming the VTEP is one."
  [{:keys [host read? error rows] :as reading} vtep]
  (if-not read?
    [(str host ": the fdb table could not be read: " error)]
    (let [ev (evidence reading)]
      (vec (concat
            (map #(str host ": " (:line %)) (naming rows vtep))
            (for [f (evpn-detectives/detect-orphan-fdb ev) :when (= vtep (:vtep f))]
              (str host ": evpn-orphan-fdb " (:summary f)))
            (for [f (evpn-detectives/detect-fdb-conflicts ev) :when (= vtep (:stale-remote-vtep f))]
              (str host ": evpn-fdb-conflict " (:summary f))))))))

(defn post-check
  "The post-check of one touched host: no learned row names the VTEP, and
  neither detective reports one naming it. `read` re-reads the host."
  [host vtep read]
  {:description (format "no learned fdb row on %s names %s, and evpn-orphan-fdb and evpn-fdb-conflict report none for it" host vtep)
   :timeout 30
   :interval 5
   :check (fn []
            (let [left (remaining (read) vtep)]
              (doseq [l left] (stdout/detail l))
              (empty? left)))})

;; --- what the book acts on ------------------------------------------------------------

(def no-hosts-message
  "The same words the fabric detective uses when it has no hosts."
  "no fabric hosts - pass --hosts or set :evpn :hosts in env.edn")

(defn connect
  "The :connect of the change book: the hosts --hosts names, else env.edn's
  :evpn :hosts through env-lookup (a (fn [path])), as connections that use
  sudo, and the VTEP --vtep names. nil, having said why, when there is no
  host or the VTEP is no address, so the run fails rather than finding
  nothing to do."
  [opts env-lookup]
  (let [hosts (:value (evpn/resolve-input (first (filter #(= "hosts" (:input %)) evpn/inputs)) opts env-lookup))
        named (:vtep opts)
        problem (when (some? named) (vtep-problem named))]
    (stdout/print-section "🔌 Fabric")
    (cond
      (empty? hosts) (do (stdout/error no-hosts-message) nil)
      problem (do (stdout/error problem) nil)
      :else {:context {:connections (vec (evpn/connections hosts {:user (:user opts)}))
                       :vtep named}
             :cluster (str "fabric (" (str/join ", " hosts) ")")
             :announce-as "fabric"})))

(defn connection-of
  "The connection of host among a context's connections."
  [{:keys [connections]} host]
  (some #(when (= host (:host %)) %) connections))

(defn offered
  "The VTEPs the book offers: the one --vtep names, so a VTEP already cleared
  is judged by the change and skipped as done, else those a scan of every
  host finds orphaned."
  [{:keys [connections vtep]}]
  (if vtep [vtep] (candidates (scan-fabric connections remote-result))))
