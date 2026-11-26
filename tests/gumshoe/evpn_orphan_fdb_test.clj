;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.evpn-orphan-fdb-test
  "Remote fdb rows naming a VTEP no BGP path advertises.

  The fixtures follow a hypervisor that was emptied: its VTEP 10.21.2.70 left
  both VNIs and withdrew every route, while the two hosts that received its
  guests kept learned rows pointing at it, 26 on one and 13 on the other. Five
  of those MACs also sit on a local tap, the collisions evpn-fdb-conflict
  reports. The rows are the shapes `bridge fdb show` prints, parsed by the
  collection's own parser, and the bridges are named as unevenly as they are
  on the real hosts: onebr8 for VNI 4208, onebr.4201 for 4201."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.evpn :as evpn]
            [gumshoe.evpn :as collect]))

(def ^:private emu "secure-emu.srv.g60.bastelgenosse.de")
(def ^:private cobra "upward-cobra.srv.g60.bastelgenosse.de")
(def ^:private dead "10.21.2.70")
(def ^:private live "10.21.2.71")

(def ^:private bridge {4208 "onebr8" 4201 "onebr.4201"})

(defn- mac
  [vni n]
  (format "02:00:0a:%s:%02x" (if (= 4208 vni) "18:00" "15:09") n))

(defn- remote-pin
  "The three rows FRR installs for a MAC learned from a remote VTEP: two bridge
  rows that name no VTEP, and the device's own row that does."
  [vni n vtep]
  (let [m (mac vni n)]
    [(str m " dev lo." vni " vlan 1 extern_learn master " (bridge vni))
     (str m " dev lo." vni " extern_learn master " (bridge vni))
     (str m " dev lo." vni " dst " vtep " self extern_learn")]))

(defn- collision
  "A guest that migrated here: its tap row, beside the vlan-tagged bridge row
  and the dst row its old VTEP left behind."
  [vni n tap]
  (let [m (mac vni n)]
    [(str m " dev lo." vni " vlan 1 extern_learn master " (bridge vni))
     (str m " dev lo." vni " dst " dead " self extern_learn")
     (str m " dev " tap " master " (bridge vni))]))

(def ^:private emu-lines
  (concat
   (mapcat #(if (= 0x0b %) (collision 4208 % "one-247-0") (remote-pin 4208 % dead))
           (range 0x01 0x13))
   (mapcat #(remote-pin 4201 % dead) (range 0x01 0x09))
   ["02:00:0a:18:00:13 dev one-278-0 master onebr8"
    "33:33:00:00:00:01 dev onebr8 self permanent"
    "00:00:00:00:00:00 dev lo.4208 dst 10.21.2.73 self permanent"]))

(def ^:private cobra-taps
  {[4208 0x09] "one-243-0" [4208 0x0c] "one-249-0" [4208 0x15] "one-283-0"
   [4201 0x04] "one-243-1"})

(def ^:private cobra-lines
  (concat
   (mapcat #(if-let [tap (cobra-taps [4208 %])] (collision 4208 % tap) (remote-pin 4208 % dead))
           [0x09 0x0b 0x0c 0x15 0x20 0x21 0x22 0x23 0x24])
   (mapcat #(if-let [tap (cobra-taps [4201 %])] (collision 4201 % tap) (remote-pin 4201 % dead))
           [0x04 0x10 0x11 0x12])
   (remote-pin 4208 0x30 live)
   (remote-pin 4208 0x31 live)))

(defn- table
  "One host's table as the collection keeps it: read once per VNI scanned."
  [host lines]
  (let [output (str/join "\n" lines)]
    (concat (collect/parse-fdb 4208 host output)
            (collect/parse-fdb 4201 host output))))

(def ^:private incident-fdb
  (vec (concat (table emu emu-lines) (table cobra cobra-lines))))

(def ^:private incident-macs
  (into [{:host emu :host-vtep live :vni 4208 :mac (mac 4208 0x0b) :type :local :intf "one-247-0"}]
        (for [[[vni n] tap] cobra-taps]
          {:host cobra :host-vtep nil :vni vni :mac (mac vni n) :type :local :intf tap})))

(def ^:private none {:read? true :paths 0})

(def ^:private incident-paths
  {[emu 4208 dead] none
   [emu 4201 dead] none
   [cobra 4208 dead] none
   [cobra 4201 dead] none
   [cobra 4208 live] {:read? true :paths 12}})

(def ^:private incident
  {"evpn-fdb" incident-fdb
   "evpn-macs" incident-macs
   "evpn-vtep-paths" incident-paths})

(defn- by-component
  [findings]
  (into {} (map (juxt :component identity)) findings))

(deftest the-incident-reads-as-39-rows-on-2-hosts-beside-the-5-collisions-test
  (let [conflicts (evpn/detect-fdb-conflicts incident)
        orphans (evpn/detect-orphan-fdb incident)
        found (by-component orphans)]
    (testing "the five collisions keep their severity and wording"
      (is (= 5 (count conflicts)))
      (is (every? #(= :critical (:severity %)) conflicts))
      (is (every? #(re-matches #"host holds both a local tap entry \(one-\d+-\d\) and a remote entry \(dst 10\.21\.2\.70\) for one MAC"
                               (:summary %))
                  conflicts)))
    (testing "one finding per host and dead VTEP, 39 rows in all"
      (is (= 2 (count orphans)))
      (is (= #{(str emu "/" dead) (str cobra "/" dead)} (set (keys found))))
      (is (= 26 (:count (found (str emu "/" dead)))))
      (is (= 13 (:count (found (str cobra "/" dead)))))
      (is (= 39 (reduce + (map :count orphans)))))
    (testing "the summary counts rows naming the VTEP and the MACs they name, apart"
      (is (= "26 fdb rows (26 MACs) name 10.21.2.70, which no BGP path advertises in VNIs 4208, 4201"
             (:summary (found (str emu "/" dead)))))
      (is (= "13 fdb rows (13 MACs) name 10.21.2.70, which no BGP path advertises in VNIs 4208, 4201"
             (:summary (found (str cobra "/" dead))))))
    (testing "the MACs on a local tap are named, and left to evpn-fdb-conflict"
      (is (= [(mac 4208 0x0b)] (:local-tap-macs (found (str emu "/" dead)))))
      (is (= #{(mac 4208 0x09) (mac 4208 0x0c) (mac 4208 0x15) (mac 4201 0x04)}
             (set (:local-tap-macs (found (str cobra "/" dead)))))))
    (testing "a guest living on the other host is unreachable from this one, so it is critical"
      (is (= :critical (:severity (found (str cobra "/" dead)))))
      (is (= [{:host emu :intf "one-247-0" :mac (mac 4208 0x0b) :vni 4208}]
             (:owned-elsewhere (found (str cobra "/" dead)))))
      (is (str/includes? (:hint (found (str cobra "/" dead)))
                         (str (mac 4208 0x0b) " on " emu " (one-247-0)"))))
    (testing "rows naming the live VTEP are left alone"
      (is (not-any? #(str/includes? (str (:component %) (:summary %)) live) orphans))
      (is (not-any? #(some #{(mac 4208 0x30) (mac 4208 0x31)} (:macs %)) orphans)))))

(deftest an-orphan-without-a-local-tap-is-reported-test
  (let [lines (remote-pin 4208 0x20 dead)
        findings (evpn/detect-orphan-fdb {"evpn-fdb" (vec (table cobra lines))
                                          "evpn-macs" []
                                          "evpn-vtep-paths" {[cobra 4208 dead] none}})
        finding (first findings)]
    (is (= 1 (count findings)))
    (is (= :warning (:severity finding)))
    (is (= (str cobra "/" dead) (:component finding)))
    (is (= [(mac 4208 0x20)] (:macs finding)))
    (is (= [] (:local-tap-macs finding)))
    (testing "the hint says how to clear the rows without naming a book"
      (is (str/includes? (:hint finding) "leave every tap row in place"))
      (is (not (re-find #"(?i)\bbook\b" (:hint finding)))))))

(deftest a-pin-counts-one-row-naming-the-vtep-and-carries-its-bridge-rows-test
  (let [finding (first (evpn/detect-orphan-fdb {"evpn-fdb" (vec (table emu (remote-pin 4208 0x01 dead)))
                                                "evpn-macs" []
                                                "evpn-vtep-paths" {[emu 4208 dead] none}}))]
    (is (= 1 (:count finding)))
    (is (str/starts-with? (:summary finding) "1 fdb row (1 MAC) name"))
    (testing "the row and its two bridge rows each carry the command that removes it"
      (is (= [{:row "02:00:0a:18:00:01 dev lo.4208 dst 10.21.2.70 self extern_learn" :seen 1
               :delete "bridge fdb del 02:00:0a:18:00:01 dev lo.4208 dst 10.21.2.70 self"}]
             (:rows finding)))
      (is (= #{"bridge fdb del 02:00:0a:18:00:01 dev lo.4208 vlan 1 master"
               "bridge fdb del 02:00:0a:18:00:01 dev lo.4208 master"}
             (set (map :delete (:bridge-rows finding))))))))

(deftest a-row-printed-twice-counts-twice-and-says-it-is-one-test
  (let [line "02:00:0a:18:00:01 dev lo.4208 dst 10.21.2.70 self extern_learn"
        finding (first (evpn/detect-orphan-fdb {"evpn-fdb" (vec (table emu [line line]))
                                                "evpn-macs" []
                                                "evpn-vtep-paths" {[emu 4208 dead] none}}))]
    (is (= 2 (:count finding)))
    (is (str/starts-with? (:summary finding) "2 fdb rows, 1 distinct (1 MAC) name"))))

(deftest a-vtep-bgp-advertises-produces-no-finding-test
  (is (empty? (evpn/detect-orphan-fdb {"evpn-fdb" (vec (table cobra (remote-pin 4208 0x30 live)))
                                       "evpn-macs" []
                                       "evpn-vtep-paths" {[cobra 4208 live] {:read? true :paths 1}}}))))

(deftest the-findings-follow-bgp-not-the-row-shape-test
  (testing "the incident with 10.21.2.70 advertised again: collisions stay, orphans go"
    (let [advertised (assoc incident "evpn-vtep-paths"
                            (update-vals incident-paths (constantly {:read? true :paths 3})))]
      (is (= 5 (count (evpn/detect-fdb-conflicts advertised))))
      (is (empty? (evpn/detect-orphan-fdb advertised)))))
  (testing "the VTEP absent in one VNI and advertised in the other reports only that VNI's rows"
    (let [split (assoc-in incident ["evpn-vtep-paths" [emu 4201 dead]] {:read? true :paths 2})
          finding ((by-component (evpn/detect-orphan-fdb split)) (str emu "/" dead))]
      (is (= [4208] (:vnis finding)))
      (is (= 18 (:count finding))))))

(deftest an-unread-bgp-state-is-reported-and-judges-no-row-an-orphan-test
  (let [unread {:read? false :error "no output"}
        evidence (assoc incident "evpn-vtep-paths"
                        (update-vals incident-paths (constantly unread)))
        findings (evpn/detect-orphan-fdb evidence)]
    (is (= 3 (count findings)) (pr-str (map :summary findings)))
    (is (every? #(= :warning (:severity %)) findings))
    (is (every? #(str/includes? (:summary %) "could not be read") findings))
    (is (not-any? #(str/includes? (:summary %) "which no BGP path advertises") findings))
    (testing "the read's error is named"
      (is (every? #(str/includes? (:hint %) "no output") findings))))
  (testing "a VTEP whose count was never collected reads the same"
    (let [findings (evpn/detect-orphan-fdb (assoc incident "evpn-vtep-paths" {}))]
      (is (seq findings))
      (is (every? #(str/includes? (:summary %) "could not be read") findings)))))

(deftest a-fabric-without-bgp-paths-is-an-unread-check-test
  (let [evidence {"evpn-fdb" incident-fdb "evpn-macs" incident-macs}
        unread (set (map :name (filter :unread (detective/mark-unread evpn/detectives evidence))))]
    (is (contains? unread "evpn-orphan-fdb"))
    (is (empty? (evpn/detect-orphan-fdb evidence)) "no count read is no conclusion either way")
    (is (not (contains? unread "evpn-fdb-conflict")))
    (testing "and the engine keeps none of its findings"
      (is (not-any? #(= "evpn-orphan-fdb" (:detective %))
                    (detective/run-detectives evpn/detectives evidence)))))
  (testing "an empty table with no remote VTEP is a read"
    (is (not (contains? (set (map :name (filter :unread (detective/mark-unread
                                                         evpn/detectives
                                                         {"evpn-fdb" [] "evpn-macs" []
                                                          "evpn-vtep-paths" {}}))))
                        "evpn-orphan-fdb")))))

(deftest a-static-pin-stays-with-evpn-static-fdb-test
  (let [lines ["02:00:0a:18:00:05 dev lo.4208 dst 10.21.2.70 self static"
               "02:00:0a:18:00:05 dev lo.4208 vlan 1 master onebr8 static"
               "02:00:0a:18:00:05 dev lo.4208 master onebr8 static"]
        fdb (vec (table emu lines))]
    (is (= [] (collect/remote-vteps fdb)) "no BGP read is spent on a static pin")
    (is (empty? (evpn/detect-orphan-fdb {"evpn-fdb" fdb "evpn-macs" []
                                         "evpn-vtep-paths" {[emu 4208 dead] none}})))
    (is (= 1 (count (evpn/detect-static-fdb {"evpn-fdb" fdb}))))))

(deftest remote-vteps-name-each-host-vni-and-vtep-once-test
  (is (= [[emu 4208 dead] [emu 4201 dead] [cobra 4208 dead] [cobra 4208 live] [cobra 4201 dead]]
         (collect/remote-vteps incident-fdb)))
  (testing "a permanent flood entry is no learned row"
    (is (not-any? #(= "10.21.2.73" (nth % 2)) (collect/remote-vteps incident-fdb)))))

(def ^:private both-tables-none
  "{\n  \"numPrefix\":4,\n  \"numPaths\":0,\n  \"macTable\":{\n    \"numPrefix\":9,\n    \"numPaths\":0\n  }\n}\n")

(deftest parse-vtep-paths-reads-the-path-counts-test
  (testing "zero paths in both tables is a read with no path, whatever the prefix count"
    (is (= {:read? true :paths 0} (collect/parse-vtep-paths both-tables-none))))
  (testing "a MAC-only route in the MAC table is a path"
    (is (= {:read? true :paths 3}
           (collect/parse-vtep-paths "{\"numPrefix\":2,\"numPaths\":0,\"macTable\":{\"numPrefix\":3,\"numPaths\":3}}"))))
  (testing "the single-table read of an older FRR"
    (is (= {:read? true :paths 1} (collect/parse-vtep-paths "{\"numPrefix\":5,\"numPaths\":1}"))))
  (testing "a VNI BGP does not know prints no count, and is unread"
    (is (false? (:read? (collect/parse-vtep-paths "{\n  \"macTable\":{\n  }\n}\n")))))
  (testing "a MAC table without its count is unread"
    (is (false? (:read? (collect/parse-vtep-paths "{\"numPrefix\":1,\"numPaths\":0,\"macTable\":{}}")))))
  (testing "silence, sudo noise and text output are unread"
    (is (false? (:read? (collect/parse-vtep-paths ""))))
    (is (false? (:read? (collect/parse-vtep-paths "sudo: a password is required\n"))))
    (is (false? (:read? (collect/parse-vtep-paths "No EVPN prefixes exist for this VNI\n")))))
  (testing "an unknown command is unread and says so"
    (let [r (collect/parse-vtep-paths "% Unknown command: show bgp vni 4208 vtep 10.21.2.70 json\n")]
      (is (false? (:read? r)))
      (is (:unknown-command? r)))))

(deftest read-vtep-paths-falls-back-only-on-an-unknown-command-test
  (let [conn {:host emu :needs-sudo? true}
        runner (fn [answers seen]
                 (fn [_ argv]
                   (swap! seen conj argv)
                   (get answers (last argv) "")))]
    (testing "an FRR without the two-table command is read through the single table"
      (let [seen (atom [])
            r (collect/read-vtep-paths conn 4208 dead
                                       (runner {"show bgp vni 4208 vtep 10.21.2.70 json"
                                                "% Unknown command: show bgp vni 4208 vtep 10.21.2.70 json"
                                                "show bgp l2vpn evpn route vni 4208 vtep 10.21.2.70 json"
                                                "{\"numPrefix\":0,\"numPaths\":0}"}
                                               seen))]
        (is (= {:read? true :paths 0} r))
        (is (= 2 (count @seen)))))
    (testing "a failed two-table read never falls back to a read that misses MAC-only routes"
      (let [seen (atom [])
            r (collect/read-vtep-paths conn 4208 dead
                                       (runner {"show bgp l2vpn evpn route vni 4208 vtep 10.21.2.70 json"
                                                "{\"numPrefix\":0,\"numPaths\":0}"}
                                               seen))]
        (is (false? (:read? r)))
        (is (= [["sudo" "vtysh" "-c" "show bgp vni 4208 vtep 10.21.2.70 json"]] @seen))))))

(defn- stub-ssh
  [seen answers]
  (fn [& args]
    (let [argv (vec (flatten args))
          remote (vec (rest (drop (inc (.indexOf argv "--")) argv)))
          line (str/join " " remote)]
      (swap! seen conj line)
      {:exit 0 :err ""
       :out (or (some (fn [[needle out]] (when (str/includes? line needle) out)) answers) "")})))

(deftest collecting-counts-paths-once-per-remote-vtep-test
  (let [seen (atom [])
        fdb (str/join "\n" (concat (remote-pin 4208 0x20 dead)
                                   (remote-pin 4208 0x21 dead)
                                   (remote-pin 4208 0x30 live)
                                   ["02:00:0a:18:00:05 dev lo.4208 dst 10.21.2.99 self static"
                                    "02:00:0a:18:00:13 dev one-278-0 master onebr8"]))
        answers [["'show evpn vni 4208'" " Local VTEP IP: 10.21.2.73\n"]
                 ["'show evpn mac vni 4208'" "02:00:0a:18:00:13 local one-278-0 1/0\n"]
                 ["'show bgp vni 4208 vtep 10.21.2.70 json'" both-tables-none]
                 ["'show bgp vni 4208 vtep 10.21.2.71 json'"
                  "{\"numPrefix\":2,\"numPaths\":1,\"macTable\":{\"numPrefix\":4,\"numPaths\":4}}"]
                 ["bridge fdb show" fdb]]
        evidence (binding [gumshoe.ssh/*user-config* nil]
                   (with-redefs [gumshoe.shell/execute (stub-ssh seen answers)]
                     (collect/collect-evidence! [{:host cobra :needs-sudo? true}] [4208])))]
    (is (= {[cobra 4208 dead] {:read? true :paths 0}
            [cobra 4208 live] {:read? true :paths 5}}
           (get evidence "evpn-vtep-paths")))
    (testing "one read per VTEP, none per MAC, none for a static pin's VTEP"
      (is (= 1 (count (filter #(str/includes? % "vtep 10.21.2.70") @seen))))
      (is (not-any? #(str/includes? % "10.21.2.99") @seen)))
    (let [findings (evpn/detect-orphan-fdb evidence)]
      (is (= 1 (count findings)))
      (is (= "2 fdb rows (2 MACs) name 10.21.2.70, which no BGP path advertises in VNI 4208"
             (:summary (first findings)))))))

(deftest a-host-whose-bgp-does-not-answer-reports-unread-test
  (let [seen (atom [])
        answers [["'show evpn vni 4208'" " Local VTEP IP: 10.21.2.73\n"]
                 ["'show evpn mac vni 4208'" "02:00:0a:18:00:13 local one-278-0 1/0\n"]
                 ["bridge fdb show" (str/join "\n" (remote-pin 4208 0x20 dead))]]
        evidence (binding [gumshoe.ssh/*user-config* nil]
                   (with-redefs [gumshoe.shell/execute (stub-ssh seen answers)]
                     (collect/collect-evidence! [{:host cobra :needs-sudo? true}] [4208])))
        findings (evpn/detect-orphan-fdb evidence)]
    (is (false? (get-in evidence ["evpn-vtep-paths" [cobra 4208 dead] :read?])))
    (is (= 1 (count findings)))
    (is (str/includes? (:summary (first findings)) "could not be read"))))

(deftest the-hint-names-the-collisions-in-agreeing-number-test
  (let [hint-of (fn [component] (:hint ((by-component (evpn/detect-orphan-fdb incident)) component)))]
    (is (str/includes? (hint-of (str emu "/" dead)) "1 MAC here also sits on a local tap"))
    (is (str/includes? (hint-of (str cobra "/" dead)) "4 MACs here also sit on a local tap"))))
