;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.clear-orphan-fdb-test
  "runbooks/fabric/clear_orphan_fdb.clj run whole against a simulated fabric
  over stubbed ssh.

  The fixtures follow the decommission the book was asked for: the hypervisor
  whose VTEP was 10.21.2.70 was emptied and left both VNIs, so BGP holds no
  path from it, while secure-emu (whose own, live VTEP is 10.21.2.71) and
  upward-cobra kept the learned rows pointing at it. Some of those MACs now
  sit on a local tap, upward-cobra also holds correct rows naming the live
  10.21.2.71, and secure-emu holds a static pin naming the dead VTEP. The
  bridges are named as unevenly as on the real hosts: onebr8 for VNI 4208,
  onebr.4201 for 4201.

  The simulated hosts answer `bridge fdb show` from their tables, `vtysh -c
  'show bgp vni <vni> vtep <vtep> json'` from their path counts, and apply
  `bridge fdb del` to their tables, answering ENOENT for a row that is not
  there as iproute2 does. A scenario can change the fabric while the
  operator is asked, so the judgement after the confirmation is tested on a
  real run. Transcripts of every scenario are kept in
  tests/golden/clear_orphan_fdb.edn; regenerate them after an intended
  change with (gumshoe.clear-orphan-fdb-test/write-golden!) and review the
  diff."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.config :as config]
            [gumshoe.detectives.evpn]
            [gumshoe.effect :as effect]
            [gumshoe.evpn :as evpn]
            [gumshoe.evpn-clear :as clear]
            [gumshoe.flow :as flow]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.outcome :as outcome]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]
            [gumshoe.streams-support :as streams]
            [gumshoe.verify :as verify]))

(def golden-path "tests/golden/clear_orphan_fdb.edn")
(def book-path "runbooks/fabric/clear_orphan_fdb.clj")

(def emu "secure-emu.srv.g60.bastelgenosse.de")
(def cobra "upward-cobra.srv.g60.bastelgenosse.de")
(def lynx "steady-lynx.srv.g60.bastelgenosse.de")
(def dead "10.21.2.70")
(def live "10.21.2.71")

(def ^:private bridge {4208 "onebr8" 4201 "onebr.4201"})

(defn mac
  [vni n]
  (format "02:00:0a:%s:%02x" (if (= 4208 vni) "18:00" "15:09") n))

(defn- remote-pin
  "The three rows FRR installs for a MAC learned from a remote VTEP."
  [vni n vtep]
  (let [m (mac vni n)]
    [(str m " dev lo." vni " vlan 1 extern_learn master " (bridge vni))
     (str m " dev lo." vni " extern_learn master " (bridge vni))
     (str m " dev lo." vni " dst " vtep " self extern_learn")]))

(defn- collision
  "A guest that migrated here: its tap row beside the vlan-tagged bridge row
  and the dst row its old VTEP left behind."
  [vni n tap]
  (let [m (mac vni n)]
    [(str m " dev lo." vni " vlan 1 extern_learn master " (bridge vni))
     (str m " dev lo." vni " dst " dead " self extern_learn")
     (str m " dev " tap " master " (bridge vni))]))

(def static-pin
  "A pin an operator left: three static rows naming the dead VTEP."
  ["02:00:0a:18:00:7f dev lo.4208 vlan 1 master onebr8 static"
   "02:00:0a:18:00:7f dev lo.4208 master onebr8 static"
   "02:00:0a:18:00:7f dev lo.4208 dst 10.21.2.70 self static"])

(def emu-lines
  (vec (concat (remote-pin 4208 0x01 dead)
               (remote-pin 4208 0x02 dead)
               (collision 4208 0x0b "one-247-0")
               (remote-pin 4201 0x01 dead)
               static-pin
               ["02:00:0a:18:00:13 dev one-278-0 master onebr8"
                "33:33:00:00:00:01 dev onebr8 self permanent"
                "00:00:00:00:00:00 dev lo.4208 dst 10.21.2.73 self permanent"])))

(def cobra-lines
  (vec (concat (collision 4208 0x09 "one-243-0")
               (remote-pin 4208 0x20 dead)
               (remote-pin 4201 0x10 dead)
               (remote-pin 4208 0x30 live)
               (remote-pin 4208 0x31 live)
               ["02:00:0a:18:00:09 dev one-243-0 master onebr8 permanent"])))

(def lynx-lines
  "A host where no guest landed: only remote rows, no tap."
  (vec (concat (remote-pin 4208 0x40 dead)
               (remote-pin 4201 0x41 dead))))

(def incident-paths
  {[emu 4208 dead] 0 [emu 4201 dead] 0
   [cobra 4208 dead] 0 [cobra 4201 dead] 0
   [cobra 4208 live] 12
   [lynx 4208 dead] 0 [lynx 4201 dead] 0})

(def incident
  {:tables {emu emu-lines cobra cobra-lines}
   :paths incident-paths})

;; --- the simulated fabric -------------------------------------------------------------

(defn- unquoted
  "A token as the remote shell reads it: ssh-args single-quotes every token
  holding a space."
  [token]
  (if (re-matches #"'[^']*'" token) (subs token 1 (dec (count token))) token))

(defn- remote
  "[host command] of an ssh argv, as the remote shell splits the command."
  [argv]
  (let [[host & command] (rest (drop-while #(not= "--" %) argv))]
    [host (mapv unquoted command)]))

(def ^:private timed-out
  {:exit 255 :out "" :err "ssh: connect to host secure-emu.srv.g60.bastelgenosse.de port 22: Connection timed out"})

(defn- path-json
  [n]
  (json/generate-string {:vni 4208 :numPrefix 9 :numPaths n :macTable {:numPrefix 14 :numPaths 0}}))

(defn- matches-delete?
  "Whether a table line is the row a `bridge fdb del` argv names."
  [host line args]
  (let [[m _ dev & more] args
        flag (last more)
        kv (apply hash-map (butlast more))
        row (evpn/fdb-row host line)]
    (and row
         (= m (:mac row)) (= dev (:dev row))
         (= (get kv "vlan") (:vlan row))
         (= (get kv "vni") (:entry-vni row))
         (if (= "self" flag)
           (= (get kv "dst") (:dst row))
           (and (nil? (:dst row)) (:master row))))))

(defn- answer-ssh
  [state argv]
  (let [[host command] (remote argv)
        {:keys [tables paths unreachable vtysh]} @state
        command (vec (remove #{"sudo" "-n"} command))]
    (swap! state update :commands conj [host command])
    (cond
      (contains? unreachable host) timed-out

      (= ["bridge" "fdb" "show"] command)
      {:exit 0 :out (str/join "\n" (get tables host)) :err ""}

      (= ["true"] command) {:exit 0 :out "" :err ""}

      (= "vtysh" (first command))
      (let [[_ vni vtep] (re-find #"^show bgp vni (\d+) vtep (\S+) json$" (nth command 2 ""))
            n (get paths [host (some-> vni parse-long) vtep])]
        (cond
          (= :old-frr vtysh) {:exit 1 :out "% Unknown command: show bgp vni" :err ""}
          (= :unread n) {:exit 1 :out "" :err "vtysh: failed to connect to any daemons."}
          (integer? n) {:exit 0 :out (path-json n) :err ""}
          :else {:exit 0 :out "{\"macTable\":{}}" :err ""}))

      (= ["bridge" "fdb" "del"] (subvec command 0 (min 3 (count command))))
      (let [args (subvec command 3)
            table (get tables host)
            hit (some #(when (matches-delete? host % args) %) table)]
        (swap! state update :deleted conj [host command])
        (if hit
          (do (swap! state update-in [:tables host] (fn [t] (vec (remove #{hit} t))))
              {:exit 0 :out "" :err ""})
          {:exit 2 :out "" :err "RTNETLINK answers: No such file or directory"}))

      :else {:exit 0 :out "" :err ""})))

(defn- load-config
  []
  (binding [runbook/*loading* true]
    (load-file book-path)))

(defn run-book
  "Runs the book's whole :action over the simulated fabric. `on-confirm`
  changes the fabric (a fn of the state map) while the operator is asked, who
  answers yes. Returns the result, what was printed, the commands each host
  received, the deletes and the tables afterwards."
  [{:keys [fabric opts on-confirm dry-run? env-hosts picked]}]
  (let [state (atom (merge {:commands [] :deleted [] :unreachable #{}} fabric))
        asked (atom nil)
        record (outcome/fresh)
        eventually verify/eventually
        config (load-config)]
    (binding [ssh/*user-config* nil
              outcome/*record* record
              flow/*dry-run* (boolean dry-run?)]
      (with-redefs [shell/execute (fn [& argv]
                                    (if (= "ssh" (first argv))
                                      (answer-ssh state (vec argv))
                                      {:exit 1 :out "" :err "not simulated"}))
                    kubectl/current-cluster (constantly nil)
                    kubectl/current-context (constantly nil)
                    config/env-value (fn [_ path & _] (when (= [:evpn :hosts] path) env-hosts))
                    fuzzy/select-single (constantly picked)
                    interact/confirm! (fn [request]
                                        (reset! asked request)
                                        (when on-confirm (swap! state on-confirm))
                                        true)
                    announce/announce! (fn [_ _ message] (swap! state assoc :announced message) nil)
                    verify/eventually (fn [check] (eventually (assoc check :timeout 0)))]
        (let [{:keys [result err]} (streams/separated
                                    #(try ((:action config) (or opts {}) {:announcement-data nil})
                                          (catch Exception e (str "threw: " (ex-message e)))))
              {:keys [text]} (outcome/banner {:changes? true :outcome (if (true? result) :ok :failed)} @record)]
          {:result result
           :out (str/split-lines (stdout/strip-colors (str/trim err)))
           :banner text
           :asked (when @asked (select-keys @asked [:action :target :items :destructive?]))
           :announced (:announced @state)
           :deleted (:deleted @state)
           :commands (:commands @state)
           :tables (:tables @state)})))))

(defn- deleted-on
  [run host]
  (vec (for [[h command] (:deleted run) :when (= h host)] (str/join " " command))))

(defn- printed?
  [run re]
  (boolean (some #(re-find re %) (:out run))))

(defn- remaining-naming
  [run host vtep]
  (filterv #(str/includes? % (str "dst " vtep " ")) (get-in run [:tables host])))

;; --- the incident --------------------------------------------------------------------

(def emu-deletes
  "The exact commands that clear secure-emu: per MAC the dst row, then its
  bridge rows on the overlay device, the vlan-tagged one first. The collision
  MAC 0b has no plain bridge row; its tap row stays."
  ["bridge fdb del 02:00:0a:15:09:01 dev lo.4201 dst 10.21.2.70 self"
   "bridge fdb del 02:00:0a:15:09:01 dev lo.4201 vlan 1 master"
   "bridge fdb del 02:00:0a:15:09:01 dev lo.4201 master"
   "bridge fdb del 02:00:0a:18:00:01 dev lo.4208 dst 10.21.2.70 self"
   "bridge fdb del 02:00:0a:18:00:01 dev lo.4208 vlan 1 master"
   "bridge fdb del 02:00:0a:18:00:01 dev lo.4208 master"
   "bridge fdb del 02:00:0a:18:00:02 dev lo.4208 dst 10.21.2.70 self"
   "bridge fdb del 02:00:0a:18:00:02 dev lo.4208 vlan 1 master"
   "bridge fdb del 02:00:0a:18:00:02 dev lo.4208 master"
   "bridge fdb del 02:00:0a:18:00:0b dev lo.4208 dst 10.21.2.70 self"
   "bridge fdb del 02:00:0a:18:00:0b dev lo.4208 vlan 1 master"])

(def cobra-deletes
  ["bridge fdb del 02:00:0a:15:09:10 dev lo.4201 dst 10.21.2.70 self"
   "bridge fdb del 02:00:0a:15:09:10 dev lo.4201 vlan 1 master"
   "bridge fdb del 02:00:0a:15:09:10 dev lo.4201 master"
   "bridge fdb del 02:00:0a:18:00:09 dev lo.4208 dst 10.21.2.70 self"
   "bridge fdb del 02:00:0a:18:00:09 dev lo.4208 vlan 1 master"
   "bridge fdb del 02:00:0a:18:00:20 dev lo.4208 dst 10.21.2.70 self"
   "bridge fdb del 02:00:0a:18:00:20 dev lo.4208 vlan 1 master"
   "bridge fdb del 02:00:0a:18:00:20 dev lo.4208 master"])

(def both {:hosts (str emu "," cobra)})

(deftest the-incident-clears-exactly-the-dead-vteps-rows-test
  (let [run (run-book {:fabric incident :opts (assoc both :vtep dead)})]
    (testing "the change ran and its post-check held"
      (is (true? (:result run)) (pr-str (:out run)))
      (is (str/starts-with? (:banner run) "DONE - applied and verified") (:banner run)))
    (testing "the exact delete commands, run with sudo -n, one per row"
      (is (= emu-deletes (deleted-on run emu)))
      (is (= cobra-deletes (deleted-on run cobra))))
    (testing "no row naming the dead VTEP is left except the static pin"
      (is (= ["02:00:0a:18:00:7f dev lo.4208 dst 10.21.2.70 self static"] (remaining-naming run emu dead)))
      (is (= [] (remaining-naming run cobra dead))))
    (testing "the live VTEP's rows survive"
      (is (= (vec (concat (remote-pin 4208 0x30 live) (remote-pin 4208 0x31 live)))
             (filterv #(or (str/includes? % ":30 ") (str/includes? % ":31 ")) (get-in run [:tables cobra])))))
    (testing "tap rows, static pins and permanent rows stay"
      (doseq [line (concat static-pin
                           ["02:00:0a:18:00:0b dev one-247-0 master onebr8"
                            "02:00:0a:18:00:13 dev one-278-0 master onebr8"
                            "33:33:00:00:00:01 dev onebr8 self permanent"
                            "00:00:00:00:00:00 dev lo.4208 dst 10.21.2.73 self permanent"])]
        (is (some #{line} (get-in run [:tables emu])) line))
      (doseq [line ["02:00:0a:18:00:09 dev one-243-0 master onebr8"
                    "02:00:0a:18:00:09 dev one-243-0 master onebr8 permanent"]]
        (is (some #{line} (get-in run [:tables cobra])) line)))
    (testing "every delete names an overlay device, never a tap"
      (is (every? #(re-find #" dev lo\.\d+ " (str/join " " (second %))) (:deleted run))))))

(deftest the-confirmation-lists-hosts-vtep-counts-and-macs-once-test
  (let [{:keys [asked]} (run-book {:fabric incident :opts (assoc both :vtep dead)})
        [head emu-line cobra-line] (:items asked)]
    (is (true? (:destructive? asked)) "a typed yes is required")
    (is (= "fabric (secure-emu.srv.g60.bastelgenosse.de, upward-cobra.srv.g60.bastelgenosse.de)" (:target asked)))
    (is (= "VTEP 10.21.2.70 - no BGP path advertises it on 2 hosts, 19 rows to delete" head))
    (is (str/starts-with? emu-line (str emu ": delete 11 rows (4 dst rows, 7 bridge rows) for 4 MACs in VNI 4208, 4201")) emu-line)
    (is (str/includes? emu-line "lo.4208 master onebr8") emu-line)
    (is (str/includes? emu-line "lo.4201 master onebr.4201") "non-uniform bridge names are read from the rows")
    (is (str/includes? emu-line "02:00:0a:18:00:0b") emu-line)
    (is (str/includes? emu-line "keeps 1 tap row; keeps 1 static or permanent pin") emu-line)
    (is (str/starts-with? cobra-line (str cobra ": delete 8 rows")) cobra-line)))

(deftest the-post-check-reports-the-detectives-over-the-touched-hosts-test
  (let [run (run-book {:fabric incident :opts (assoc both :vtep dead)})]
    (is (printed? run #"no learned fdb row on secure-emu\.srv\.g60\.bastelgenosse\.de names 10\.21\.2\.70, and evpn-orphan-fdb and evpn-fdb-conflict report none for it"))
    (let [after-deletes (reverse (take-while #(not= "del" (get-in % [1 2])) (reverse (:commands run))))]
      (is (= #{[emu ["bridge" "fdb" "show"]] [cobra ["bridge" "fdb" "show"]]} (set after-deletes))
          "the post-check re-reads each touched host's table once"))
    (is (= 3 (count (filter #(= [emu ["bridge" "fdb" "show"]] %) (:commands run))))
        "the table is read before the confirmation, again before the effect, and by the post-check")))

(deftest a-single-host-run-clears-that-host-alone-test
  (let [run (run-book {:fabric incident :opts {:hosts emu :vtep dead}})]
    (is (true? (:result run)))
    (is (= emu-deletes (deleted-on run emu)))
    (is (= [] (deleted-on run cobra)))
    (is (every? #(= emu (first %)) (:commands run)) "no other host is contacted")))

(deftest whole-fabric-mode-reaches-a-host-where-no-guest-landed-test
  (let [fabric (assoc-in incident [:tables lynx] lynx-lines)
        run (run-book {:fabric fabric :env-hosts [emu cobra lynx] :opts {:vtep dead}})]
    (is (true? (:result run)) (pr-str (:out run)))
    (is (= ["bridge fdb del 02:00:0a:15:09:41 dev lo.4201 dst 10.21.2.70 self"
            "bridge fdb del 02:00:0a:15:09:41 dev lo.4201 vlan 1 master"
            "bridge fdb del 02:00:0a:15:09:41 dev lo.4201 master"
            "bridge fdb del 02:00:0a:18:00:40 dev lo.4208 dst 10.21.2.70 self"
            "bridge fdb del 02:00:0a:18:00:40 dev lo.4208 vlan 1 master"
            "bridge fdb del 02:00:0a:18:00:40 dev lo.4208 master"]
           (deleted-on run lynx)))
    (is (= [] (get-in run [:tables lynx])) "every row of the emptied host's peer is gone")))

(deftest the-hosts-come-from-env-edn-when-no-flag-names-them-test
  (let [run (run-book {:fabric incident :env-hosts [cobra] :opts {:vtep dead}})]
    (is (true? (:result run)))
    (is (= cobra-deletes (deleted-on run cobra)))
    (is (= [] (deleted-on run emu)))))

(deftest no-hosts-fail-closed-with-the-detectives-message-test
  (let [run (run-book {:fabric incident :opts {:vtep dead}})]
    (is (false? (:result run)))
    (is (printed? run #"no fabric hosts - pass --hosts or set :evpn :hosts in env\.edn"))
    (is (not (printed? run #"nothing to do")))
    (is (empty? (:commands run)) "no host is contacted")))

;; --- rows already gone, and running again --------------------------------------------

(defn- delete-effect-run
  [answer]
  (with-redefs [shell/execute (fn [& _] answer)]
    (binding [ssh/*user-config* nil]
      (streams/separated #(effect/run! [(clear/fdb-del {:host emu :needs-sudo? true}
                                                       ["bridge" "fdb" "del" "02:00:0a:18:00:01" "dev" "lo.4208" "master"])])))))

(deftest a-row-already-gone-counts-as-deleted-test
  (testing "a delete answering ENOENT is counted as done, its output shown"
    (let [{:keys [result err]} (delete-effect-run {:exit 2 :out "" :err "RTNETLINK answers: No such file or directory"})]
      (is (true? result))
      (is (str/includes? err "RTNETLINK answers: No such file or directory"))
      (is (str/includes? err "already gone"))))
  (testing "any other failure stops the plan"
    (is (false? (:result (delete-effect-run {:exit 1 :out "" :err "sudo: a password is required"}))))
    (is (false? (:result (delete-effect-run {:exit 255 :out "" :err "ssh: Could not resolve hostname: No such file or directory"})))))
  (testing "an ssh failure or a success is never read as a row already gone"
    (is (false? (clear/already-gone? {:exit 255 :err "No such file or directory"})))
    (is (true? (clear/already-gone? {:exit 2 :err "RTNETLINK answers: No such file or directory"})))
    (is (false? (clear/already-gone? {:exit 0 :err "No such file or directory"}))))
  (testing "the effect describes the exact remote command"
    (is (= "ssh secure-emu.srv.g60.bastelgenosse.de -- sudo -n bridge fdb del 02:00:0a:18:00:01 dev lo.4208 master"
           (binding [ssh/*user-config* nil]
             (effect/describe (clear/fdb-del {:host emu :needs-sudo? true}
                                             ["bridge" "fdb" "del" "02:00:0a:18:00:01" "dev" "lo.4208" "master"])))))))

(deftest a-delete-answering-enoent-inside-a-real-run-is-done-test
  (let [;; the table lists the row, but deleting it answers ENOENT: the
        ;; kernel removed it after the read
        fabric (assoc incident :tables {emu emu-lines})
        run (let [orig-answer @#'answer-ssh]
                (with-redefs-fn {#'answer-ssh (fn [state argv]
                                                (let [[_ command] (remote argv)]
                                                  (if (= ["sudo" "-n" "bridge" "fdb" "del" "02:00:0a:18:00:01" "dev" "lo.4208" "master"] command)
                                                    (do (swap! state update-in [:tables emu] (fn [t] (vec (remove #{"02:00:0a:18:00:01 dev lo.4208 extern_learn master onebr8"} t))))
                                                        (swap! state update :deleted conj [emu (vec (remove #{"sudo" "-n"} command))])
                                                        {:exit 2 :out "" :err "RTNETLINK answers: No such file or directory"})
                                                    (orig-answer state argv))))}
                  #(run-book {:fabric fabric :opts {:hosts emu :vtep dead}})))]
    (is (true? (:result run)) (pr-str (:out run)))
    (is (printed? run #"RTNETLINK answers: No such file or directory"))
    (is (printed? run #"already gone"))
    (is (= emu-deletes (deleted-on run emu)) "the deletes after it still run")))

(deftest a-second-run-is-skipped-as-done-test
  (let [first-run (run-book {:fabric incident :opts (assoc both :vtep dead)})
        second-run (run-book {:fabric (assoc incident :tables (:tables first-run)) :opts (assoc both :vtep dead)})]
    (is (true? (:result first-run)))
    (is (true? (:result second-run)))
    (is (nil? (:asked second-run)) "nothing is asked")
    (is (empty? (:deleted second-run)) "nothing is deleted")
    (is (printed? second-run #"skipped, already done") (pr-str (:out second-run)))))

(deftest a-cleared-vtep-on-an-unreadable-host-is-never-done-test
  (let [cleared (:tables (run-book {:fabric incident :opts (assoc both :vtep dead)}))
        run (run-book {:fabric (assoc incident :tables cleared :unreachable #{cobra}) :opts (assoc both :vtep dead)})]
    (is (false? (:result run)))
    (is (not (printed? run #"skipped, already done")))
    (is (printed? run #"refusing: no row naming 10\.21\.2\.70 can be judged") (pr-str (:out run)))))

;; --- unread reads refuse, never delete ---------------------------------------------

(deftest an-unread-path-count-refuses-that-hosts-rows-test
  (let [fabric (assoc-in incident [:paths [cobra 4201 dead]] :unread)
        run (run-book {:fabric fabric :opts (assoc both :vtep dead)})]
    (is (true? (:result run)) "the judged host is still cleared")
    (is (= emu-deletes (deleted-on run emu)))
    (is (= [] (deleted-on run cobra)) "nothing is deleted on the host whose count did not answer")
    (is (some #(str/starts-with? % (str cobra ": unjudged, deletes nothing - BGP's path count for 10.21.2.70 could not be read in VNI 4201"))
              (get-in run [:asked :items]))
        (pr-str (get-in run [:asked :items])))
    (is (some #(str/includes? % "02:00:0a:18:00:20 dev lo.4208 dst 10.21.2.70 self extern_learn") (get-in run [:asked :items]))
        "the unjudged rows are listed")))

(deftest a-count-bgp-did-not-print-is-unread-test
  (let [fabric (update incident :paths dissoc [emu 4201 dead])
        run (run-book {:fabric fabric :opts {:hosts emu :vtep dead}})]
    (is (false? (:result run)))
    (is (empty? (:deleted run)))
    (is (printed? run #"refusing: no row naming 10\.21\.2\.70 can be judged, so none is deleted: secure-emu\S+ - BGP's path count .* could not be read in VNI 4201"))))

(deftest every-host-unread-refuses-test
  (let [run (run-book {:fabric (assoc incident :unreachable #{emu cobra}) :opts (assoc both :vtep dead)})]
    (is (false? (:result run)))
    (is (empty? (:deleted run)))
    (is (nil? (:asked run)) "the operator is not asked to approve a refused change")
    (is (printed? run #"refusing: no row naming 10\.21\.2\.70 can be judged.*its fdb table could not be read: ssh: connect to host"))))

(deftest an-frr-without-show-bgp-vni-falls-back-to-the-single-table-read-test
  (let [run (run-book {:fabric (assoc incident :vtysh :old-frr) :opts {:hosts emu :vtep dead}})]
    (is (false? (:result run)) "the simulated old FRR answers nothing to the fallback, so it stays unjudged")
    (is (some #(= ["vtysh" "-c" "show bgp l2vpn evpn route vni 4208 vtep 10.21.2.70 json"] (second %)) (:commands run)))
    (is (empty? (:deleted run)))))

;; --- the fabric moving under the confirmation --------------------------------------

(deftest the-vtep-advertised-again-after-the-confirmation-refuses-test
  (let [run (run-book {:fabric incident :opts (assoc both :vtep dead)
                       :on-confirm #(assoc-in % [:paths [cobra 4208 dead]] 3)})]
    (is (some? (:asked run)) "the operator was asked")
    (is (false? (:result run)))
    (is (empty? (:deleted run)) "no host is touched")
    (is (printed? run #"refusing: BGP advertises 10\.21\.2\.70 in VNI 4208 on upward-cobra"))))

(deftest a-row-set-changed-after-the-confirmation-refuses-test
  (let [extra (remote-pin 4208 0x50 dead)
        run (run-book {:fabric incident :opts (assoc both :vtep dead)
                       :on-confirm #(update-in % [:tables cobra] into extra)})]
    (is (false? (:result run)))
    (is (empty? (:deleted run)))
    (is (printed? run #"refusing: the rows naming 10\.21\.2\.70 on upward-cobra\S+ are no longer the ones confirmed"))))

(deftest a-row-removed-after-the-confirmation-refuses-test
  (let [run (run-book {:fabric incident :opts (assoc both :vtep dead)
                       :on-confirm #(update-in % [:tables emu] (fn [t] (vec (remove (set (remote-pin 4208 0x02 dead)) t))))})]
    (is (false? (:result run)))
    (is (empty? (:deleted run)))
    (is (printed? run #"refusing: the rows naming 10\.21\.2\.70 on secure-emu"))))

(deftest a-host-unreachable-after-the-confirmation-refuses-test
  (let [run (run-book {:fabric incident :opts (assoc both :vtep dead)
                       :on-confirm #(assoc % :unreachable #{emu})})]
    (is (false? (:result run)))
    (is (empty? (:deleted run)))
    (is (printed? run #"refusing: the rows naming 10\.21\.2\.70 on secure-emu"))))

;; --- the live VTEP, dry runs, picking ---------------------------------------------

(deftest the-live-vtep-is-refused-and-keeps-its-rows-test
  (let [run (run-book {:fabric incident :opts (assoc both :vtep live)})]
    (is (false? (:result run)))
    (is (nil? (:asked run)))
    (is (empty? (:deleted run)))
    (is (printed? run #"refusing: BGP advertises 10\.21\.2\.71 in VNI 4208 on upward-cobra"))))

(deftest a-vtep-live-on-one-host-keeps-its-rows-on-every-host-test
  (let [fabric (assoc-in incident [:paths [emu 4201 dead]] 1)
        run (run-book {:fabric fabric :opts (assoc both :vtep dead)})]
    (is (false? (:result run)))
    (is (empty? (:deleted run)))
    (is (printed? run #"refusing: BGP advertises 10\.21\.2\.70 in VNI 4201 on secure-emu"))))

(deftest a-dry-run-deletes-nothing-and-shows-each-command-test
  (let [run (run-book {:fabric incident :opts (assoc both :vtep dead) :dry-run? true})]
    (is (empty? (:deleted run)))
    (is (nil? (:asked run)))
    (is (printed? run #"would run: ssh secure-emu\.srv\.g60\.bastelgenosse\.de -- sudo -n bridge fdb del 02:00:0a:18:00:0b dev lo\.4208 dst 10\.21\.2\.70 self"))
    (is (printed? run #"would run: ssh upward-cobra\.srv\.g60\.bastelgenosse\.de -- sudo -n bridge fdb del 02:00:0a:15:09:10 dev lo\.4201 vlan 1 master"))
    (is (= (count (concat emu-deletes cobra-deletes)) (count (filter #(re-find #"would run: .* bridge fdb del" %) (:out run)))))))

(deftest a-malformed-vtep-is-refused-before-any-host-is-read-test
  (doseq [vtep ["10.21.2" "10.21.2.300" "secure-emu" "10.21.2.*" "; reboot"]]
    (let [run (run-book {:fabric incident :opts (assoc both :vtep vtep)})]
      (is (false? (:result run)) vtep)
      (is (empty? (:commands run)) vtep)
      (is (printed? run #"is not an IPv4 or IPv6 address") vtep))))

(deftest without-vtep-the-book-offers-the-orphaned-vteps-test
  (testing "the orphaned VTEP is offered and the live one is not"
    (with-redefs [shell/execute (fn [& argv] (answer-ssh (atom (merge {:commands [] :deleted [] :unreachable #{}} incident)) (vec argv)))]
      (binding [ssh/*user-config* nil]
        (is (= [dead] (clear/offered {:connections (evpn/connections [emu cobra] {})}))))))
  (testing "a pick from the offered VTEPs runs the change"
    (let [run (run-book {:fabric incident :opts both :picked dead})]
      (is (true? (:result run)) (pr-str (:out run)))
      (is (= emu-deletes (deleted-on run emu)))))
  (testing "nothing orphaned on a fabric that answered is nothing to do"
    (let [run (run-book {:fabric {:tables {emu (remote-pin 4208 0x30 live)} :paths {[emu 4208 live] 5}} :opts {:hosts emu}})]
      (is (true? (:result run)))
      (is (printed? run #"no scanned host holds a learned fdb row naming a VTEP no BGP path advertises"))))
  (testing "nothing found while a host did not answer fails closed"
    (let [run (run-book {:fabric (assoc incident :unreachable #{emu cobra}) :opts both})]
      (is (false? (:result run)))
      (is (printed? run #"could not list the VTEP candidates"))
      (is (not (printed? run #"no scanned host holds"))))))

;; --- the pure judgement -------------------------------------------------------------

(defn- reading
  [host lines paths]
  {:host host :read? true :output (str/join "\n" lines)
   :rows (evpn/parse-fdb-table host (str/join "\n" lines))
   :paths (into {} (for [[[h vni vtep] n] paths :when (= h host)] [[vni vtep] {:read? true :paths n}]))})

(deftest positive-control-the-judgement-finds-the-orphans-test
  (let [judged (clear/judge-fabric [(reading emu emu-lines incident-paths)
                                    (reading cobra cobra-lines incident-paths)]
                                   dead)]
    (is (= [emu cobra] (map :host (:orphaned judged))))
    (is (= 19 (reduce + (map (comp count clear/deletes) (:orphaned judged)))))
    (is (nil? (clear/refusal judged judged)))
    (is (false? (clear/cleared? judged)))))

(deftest a-mac-also-sent-to-another-vtep-keeps-its-bridge-rows-test
  (let [lines (concat (remote-pin 4208 0x01 dead)
                      ["02:00:0a:18:00:01 dev lo.4208 dst 10.21.2.72 self extern_learn"])
        judged (clear/judge-host (reading emu lines {[emu 4208 dead] 0}) dead)]
    (is (= :orphaned (:status judged)))
    (is (= [["bridge" "fdb" "del" "02:00:0a:18:00:01" "dev" "lo.4208" "dst" dead "self"]]
           (map :argv (clear/deletes judged))))
    (is (= 2 (count (:kept-bridge-rows (first (:pins judged))))))))

(deftest a-row-naming-the-vtep-off-an-overlay-device-leaves-the-host-unjudged-test
  (let [lines ["02:00:0a:18:00:01 dev vxlan0 dst 10.21.2.70 vni 4208 self extern_learn"]
        judged (clear/judge-host (reading emu lines {[emu 4208 dead] 0}) dead)]
    (is (= :unjudged (:status judged)))
    (is (re-find #"vxlan0, which is no lo.<vni> overlay device" (:reason judged)))))

(deftest a-row-carrying-its-vni-is-deleted-by-it-test
  (let [lines ["02:00:0a:18:00:01 dev lo.4208 dst 10.21.2.70 vni 4208 self extern_learn"]
        judged (clear/judge-host (reading emu lines {[emu 4208 dead] 0}) dead)]
    (is (= [["bridge" "fdb" "del" "02:00:0a:18:00:01" "dev" "lo.4208" "dst" dead "vni" "4208" "self"]]
           (map :argv (clear/deletes judged))))))

(deftest the-vtep-must-be-an-address-test
  (is (nil? (clear/vtep-problem "10.21.2.70")))
  (is (nil? (clear/vtep-problem "2001:db8::70")))
  (is (nil? (clear/vtep-problem "fe80:0:0:0:0:0:0:1")))
  (doseq [bad [nil "" "10.21.2" "256.1.1.1" "10.21.2.70/32" "host" "2001:db8::1::2" ":::" "1:2:3:4:5:6:7:8:9"]]
    (is (some? (clear/vtep-problem bad)) (pr-str bad))))

(deftest the-post-check-fails-closed-on-an-unread-table-test
  (is (= [(str emu ": the fdb table could not be read: ssh: timed out")]
         (clear/remaining {:host emu :read? false :error "ssh: timed out"} dead)))
  (testing "a static pin colliding with a tap is still a conflict naming the VTEP"
    (let [lines ["02:00:0a:18:00:0b dev lo.4208 dst 10.21.2.70 self static"
                 "02:00:0a:18:00:0b dev one-247-0 master onebr8"]
          left (clear/remaining (reading emu lines {}) dead)]
      (is (= 1 (count left)))
      (is (re-find #"evpn-fdb-conflict" (first left)))))
  (testing "a cleared host holds"
    (is (= [] (clear/remaining (reading emu ["02:00:0a:18:00:13 dev one-278-0 master onebr8"] {}) dead)))))

(deftest the-orphan-finding-hands-off-to-the-book-test
  (let [[finding] (gumshoe.detectives.evpn/detect-orphan-fdb
                   (clear/evidence (reading emu (remote-pin 4208 0x01 dead) {[emu 4208 dead] 0})))]
    (is (str/includes? (:hint finding)
                       (str "`bb run runbooks/fabric/clear_orphan_fdb.clj --vtep 10.21.2.70 --hosts " emu "`")))))

;; --- golden transcripts ---------------------------------------------------------------

(def scenarios
  {:incident {:fabric incident :opts (assoc both :vtep dead)}
   :single-host {:fabric incident :opts {:hosts emu :vtep dead}}
   :dry-run {:fabric incident :opts (assoc both :vtep dead) :dry-run? true}
   :live-vtep {:fabric incident :opts (assoc both :vtep live)}
   :unread-count {:fabric (assoc-in incident [:paths [cobra 4201 dead]] :unread) :opts (assoc both :vtep dead)}
   :advertised-meanwhile {:fabric incident :opts (assoc both :vtep dead)
                          :on-confirm #(assoc-in % [:paths [cobra 4208 dead]] 3)}
   :rows-changed-meanwhile {:fabric incident :opts (assoc both :vtep dead)
                            :on-confirm #(update-in % [:tables cobra] into (remote-pin 4208 0x50 dead))}
   :no-hosts {:fabric incident :opts {:vtep dead}}})

(defn- transcript
  [scenario]
  (-> (run-book scenario)
      (select-keys [:result :out :banner :asked :announced :deleted])))

(defn transcripts
  []
  (into (sorted-map) (for [[k s] scenarios] [k (transcript s)])))

(defn write-golden!
  []
  (spit golden-path
        ;; split, so a REUSE scan does not read these strings as this file's header
        (str ";; SPDX" "-FileCopyrightText: The gumshoe Authors\n"
             ";; SPDX" "-License-Identifier: 0BSD\n"
             ";; Generated by (gumshoe.clear-orphan-fdb-test/write-golden!); review its diff.\n"
             (with-out-str (pprint/pprint (transcripts))))))

(deftest every-scenario-keeps-its-transcript-test
  (let [golden (edn/read-string (slurp golden-path))
        actual (transcripts)]
    (is (= (set (keys golden)) (set (keys actual))))
    (doseq [[k t] actual]
      (testing (name k)
        (is (= (get golden k) t))))))
