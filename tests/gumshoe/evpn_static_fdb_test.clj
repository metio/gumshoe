;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.evpn-static-fdb-test
  "evpn-static-fdb over `bridge fdb show` text as iproute2 prints it (print_fdb
  in bridge/fdb.c: MAC, dev, dst, vlan, port, vni, src_vni, via, nhid, the flag
  words self/router/extern_learn/offload/master/sticky, master <bridge>, and the
  state word permanent/static/stale). One static pin spans a `dst … self static`
  row and the bridge's `master … static` rows, with and without a vlan, and the
  table is read once per VNI scanned."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.evpn :as evpn]
            [gumshoe.evpn :as collect]))

(def ^:private hosts
  {"hv-70" "10.21.2.70" "hv-71" "10.21.2.71" "hv-73" "10.21.2.73"})

(def ^:private vnis [4201 4208])

(def ^:private bridges {4201 "onebr.4201" 4208 "onebr8"})

(def ^:private gateway-macs ["02:00:0a:15:00:01" "02:00:0a:15:00:02" "02:00:0a:15:00:03"])

(def ^:private gateway-vtep "10.21.0.1")

(defn- pin-rows
  "The three rows `bridge fdb add <mac> dev lo.<vni> master static` plus
  `bridge fdb add <mac> dev lo.<vni> dst <vtep> self static` leave on a bridge
  that keeps a vlan-1 row beside the untagged one."
  [mac vni vtep]
  [(str mac " dev lo." vni " vlan 1 master " (bridges vni) " static")
   (str mac " dev lo." vni " master " (bridges vni) " static")
   (str mac " dev lo." vni " dst " vtep " self static")])

(defn- incident-table
  "One host's whole table: the pins on both VNIs, plus learned rows, a guest tap
  and the interfaces' own permanent multicast rows, none of which is static."
  []
  (str/join "\n"
            (concat (for [vni vnis mac gateway-macs line (pin-rows mac vni gateway-vtep)] line)
                    ["02:00:0a:18:00:0d dev lo.4208 vlan 1 extern_learn master onebr8"
                     "02:00:0a:18:00:0d dev lo.4208 extern_learn master onebr8"
                     "02:00:0a:18:00:0d dev lo.4208 dst 10.21.2.71 self extern_learn"
                     "02:00:0a:18:00:13 dev one-278-0 master onebr8"
                     "33:33:00:00:00:01 dev enp1s0 self permanent"
                     "01:00:5e:00:00:01 dev enp1s0 self permanent"])))

(defn- evidence-from
  "Evidence as collection builds it: each host's table parsed once per VNI."
  [tables]
  (collect/host-evidence
   (for [[host table] tables vni vnis]
     {:host host :vni vni :macs [] :fdb (vec (collect/parse-fdb vni host table))})))

(def ^:private incident
  (evidence-from (for [host (keys hosts)] [host (incident-table)])))

(deftest the-incident-shape-reports-one-finding-per-entry-test
  (let [findings (evpn/detect-static-fdb incident)]
    (testing "the fixture holds the 54 static rows the incident reported one by one"
      (is (= 54 (count (filter :static? (get incident "evpn-fdb"))))))
    (is (= 18 (count findings)) "3 hosts x 2 VNIs x 3 MACs")
    (testing "each finding names host, device, VNI, MAC and destination"
      (is (= (set (for [host (keys hosts) vni vnis mac gateway-macs]
                    [host (str "lo." vni) (str vni) mac gateway-vtep]))
             (set (map (juxt :host :dev :vni :mac :dst) findings))))
      (is (every? #(str/includes? (:summary %) (str "pointing at " gateway-vtep)) findings))
      (is (every? #(str/includes? (:summary %) (:mac %)) findings))
      (is (every? #(str/includes? (:summary %) (str "(VNI " (:vni %) ")")) findings)))
    (testing "no two findings say the same thing"
      (is (= 18 (count (set (map (juxt :component :summary) findings))))))
    (testing "each finding counts and lists the three rows behind it"
      (is (every? #(= 3 (:count %)) findings))
      (is (every? #(str/ends-with? (:summary %) "- 3 fdb rows") findings))
      (is (every? #(= 3 (count (:rows %))) findings)))
    (testing "the hint removes every row of the pin, the bridge rows with master"
      (let [finding (first (filter #(and (= "hv-70" (:host %)) (= "4208" (:vni %))
                                         (= (first gateway-macs) (:mac %)))
                                   findings))]
        (is (str/includes? (:hint finding)
                           (str "`bridge fdb del 02:00:0a:15:00:01 dev lo.4208 dst " gateway-vtep " self`")))
        (is (str/includes? (:hint finding) "`bridge fdb del 02:00:0a:15:00:01 dev lo.4208 vlan 1 master`"))
        (is (str/includes? (:hint finding) "`bridge fdb del 02:00:0a:15:00:01 dev lo.4208 master`"))))))

(deftest two-destinations-for-one-mac-are-two-findings-test
  (let [table (str/join "\n" ["02:00:0a:15:00:01 dev lo.4201 master onebr.4201 static"
                              "02:00:0a:15:00:01 dev lo.4201 dst 10.21.0.1 self static"
                              "02:00:0a:15:00:01 dev lo.4201 dst 10.21.0.2 self static"])
        findings (evpn/detect-static-fdb (evidence-from [["hv-70" table]]))]
    (is (= 2 (count findings)))
    (is (= #{"10.21.0.1" "10.21.0.2"} (set (map :dst findings))))
    (is (= 2 (count (set (map :summary findings)))) "the summaries differ in what they name")
    (testing "the bridge row belongs to both"
      (is (every? #(some #{"02:00:0a:15:00:01 dev lo.4201 master onebr.4201 static"}
                         (map :row (:rows %)))
                  findings)))))

(deftest one-row-printed-twice-is-one-finding-with-a-count-test
  (let [row "02:00:0a:15:00:01 dev lo.4201 dst 10.21.0.1 self static"
        findings (evpn/detect-static-fdb (evidence-from [["hv-70" (str row "\n" row)]]))]
    (is (= 1 (count findings)))
    (is (= 2 (:count (first findings))))
    (is (= [{:row row :seen 2}] (:rows (first findings))))
    (is (str/ends-with? (:summary (first findings)) "- 2 fdb rows, 1 distinct"))))

(deftest a-row-read-once-per-vni-counts-once-test
  (testing "a static tap row comes back from every VNI's read of one table"
    (let [findings (evpn/detect-static-fdb
                    (evidence-from [["hv-70" "02:00:0a:18:00:13 dev one-278-0 master onebr8 static"]]))]
      (is (= 1 (count findings)))
      (is (= 1 (:count (first findings))))
      (is (= "local-port" (:destination (first findings))))
      (is (nil? (:vni (first findings))))
      (is (str/includes? (:summary (first findings)) "pinned to the local port one-278-0, so no remote VTEP")))))

(deftest a-missing-destination-is-stated-test
  (testing "bridge rows with no dst row anywhere"
    (let [table (str/join "\n" ["02:00:0a:15:00:01 dev lo.4201 vlan 1 master onebr.4201 static"
                                "02:00:0a:15:00:01 dev lo.4201 master onebr.4201 static"])
          [finding & more] (evpn/detect-static-fdb (evidence-from [["hv-70" table]]))]
      (is (nil? more))
      (is (nil? (:dst finding)))
      (is (= "unread" (:destination finding)))
      (is (str/includes? (:summary finding) "destination VTEP could not be read"))
      (is (= 2 (:count finding)))))
  (testing "a static bridge row whose device row was learned names the learned VTEP"
    (let [table (str/join "\n" ["02:00:0a:15:00:01 dev lo.4201 master onebr.4201 static"
                                "02:00:0a:15:00:01 dev lo.4201 dst 10.21.2.71 self extern_learn"])
          [finding & more] (evpn/detect-static-fdb (evidence-from [["hv-70" table]]))]
      (is (nil? more))
      (is (= "learned" (:destination finding)))
      (is (= ["10.21.2.71"] (:learned-dsts finding)))
      (is (str/includes? (:summary finding) "learned rows send it to 10.21.2.71"))
      (is (= 1 (:count finding)) "the learned row is not part of the pin"))))

(deftest an-unread-host-stays-unread-test
  (testing "one host failing leaves the others judged and the gap named"
    (let [evidence (collect/host-evidence
                    (concat [{:host "hv-71" :vni 4201 :error "host is not reachable over ssh"}]
                            (for [vni vnis]
                              {:host "hv-70" :vni vni :macs []
                               :fdb (vec (collect/parse-fdb vni "hv-70" (incident-table)))})))
          findings (evpn/detect-static-fdb evidence)]
      (is (= 6 (count findings)))
      (is (every? #(= "hv-70" (:host %)) findings))
      (is (= ["hv-71/vni 4201"] (map :component (evpn/detect-host-problems evidence))))))
  (testing "no host answering leaves the check unread rather than clean"
    (let [evidence (collect/host-evidence [{:host "hv-70" :vni 4201 :error "no output"}])]
      (is (nil? (get evidence "evpn-fdb")))
      (is (contains? (set (map :name (filter :unread (detective/mark-unread evpn/detectives evidence))))
                     "evpn-static-fdb")))))

(deftest positive-control-and-nothing-static-test
  (testing "the same table with every static row learned instead reports nothing"
    (let [learned (str/replace (incident-table) " static" " extern_learn")]
      (is (empty? (evpn/detect-static-fdb (evidence-from [["hv-70" learned]]))))))
  (testing "and one static row in it is reported"
    (let [table (str (str/replace (incident-table) " static" " extern_learn")
                     "\n02:00:0a:15:00:09 dev lo.4208 dst 10.21.0.1 self static")]
      (is (= ["02:00:0a:15:00:09"]
             (map :mac (evpn/detect-static-fdb (evidence-from [["hv-70" table]]))))))))

(deftest parsing-fdb-lines-by-word-test
  (testing "master followed by a bridge is the bridge, followed by a keyword is the flag"
    (is (= {:mac "02:00:0a:15:00:01" :values {"dev" "lo.4208" "vlan" "1" "master" "onebr8"}
            :flags #{"extern_learn"} :state nil}
           (collect/parse-fdb-line "02:00:0a:15:00:01 dev lo.4208 vlan 1 extern_learn master onebr8")))
    (is (= {:mac "02:00:0a:15:00:01" :values {"dev" "swp1"} :flags #{"master"} :state "static"}
           (collect/parse-fdb-line "02:00:0A:15:00:01 dev swp1 master static"))))
  (testing "every valued word iproute2 prints is kept"
    (is (= {"dev" "vxlan0" "dst" "10.0.0.2" "port" "4789" "vni" "4201" "src_vni" "4201"
            "via" "ifindex 7" "nhid" "12"}
           (:values (collect/parse-fdb-line
                     "02:00:0a:15:00:01 dev vxlan0 dst 10.0.0.2 port 4789 vni 4201 src_vni 4201 via ifindex 7 nhid 12 self permanent")))))
  (testing "an unknown state and an unknown word are kept, not dropped"
    (let [row (collect/parse-fdb-line "02:00:0a:15:00:01 dev lo.4208 self offload novel state=0x40")]
      (is (= "state=0x40" (:state row)))
      (is (= #{"self" "offload" "novel"} (:flags row)))))
  (testing "a line that is no row is nil"
    (is (nil? (collect/parse-fdb-line "Deleted 02:00:0a:15:00:01 dev lo.4208")))
    (is (nil? (collect/parse-fdb-line "")))))

(deftest a-per-entry-vni-separates-entries-on-one-device-test
  (testing "a collect-metadata VXLAN device prints vni per row, and the VNI keys the entry"
    (let [rows [{:host "hv-70" :mac "02:00:0a:15:00:01" :dev "vxlan0" :entry-vni "4201"
                 :dst "10.21.0.1" :self? true :static? true}
                {:host "hv-70" :mac "02:00:0a:15:00:01" :dev "vxlan0" :entry-vni "4208"
                 :dst "10.21.0.1" :self? true :static? true}]
          findings (evpn/detect-static-fdb {"evpn-fdb" rows})]
      (is (= #{"4201" "4208"} (set (map :vni findings))))
      (is (= #{"02:00:0a:15:00:01 dev vxlan0 dst 10.21.0.1 vni 4201 self static"
               "02:00:0a:15:00:01 dev vxlan0 dst 10.21.0.1 vni 4208 self static"}
             (set (mapcat #(map :row (:rows %)) findings)))))))
