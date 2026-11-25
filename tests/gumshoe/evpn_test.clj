;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.evpn-test
  "Tests for the EVPN fabric detectives and the parsers that feed them.

  The fixtures reproduce a real partition: a guest MAC whose tap is local on one
  host while every VTEP, that host included, reaches it through a gateway. The
  owner even carries the higher mobility sequence and still loses, because the
  gateway's route is MAC+IP and outranks a MAC-only route."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.evpn :as evpn]
            [gumshoe.evpn :as collect]))

(def ^:private gateway "10.0.0.1")
(def ^:private host-a-vtep "10.0.2.70")
(def ^:private host-b-vtep "10.0.2.73")
(def ^:private hijacked "02:00:0a:18:00:0d")
(def ^:private healthy "02:00:0a:18:00:06")

(def ^:private config
  {"evpn-gateway-vteps" [gateway]
   "evpn-guest-mac-prefixes" ["02:00:"]})

(defn- summaries
  [findings]
  (set (map :summary findings)))

(defn- severity-of
  [findings needle]
  (some #(when (str/includes? (:summary %) needle) (:severity %)) findings))

(deftest a-mac-local-on-one-host-and-remote-elsewhere-is-critical
  (let [evidence (merge config
                        {"evpn-macs"
                         [{:host "host-a" :host-vtep host-a-vtep :vni 4208 :mac hijacked
                           :type :local :intf "one-256-0" :local-seq 2 :remote-seq 0}
                          {:host "host-b" :host-vtep host-b-vtep :vni 4208 :mac hijacked
                           :type :remote :vtep gateway :local-seq 0 :remote-seq 1}]})
        findings (evpn/detect-mac-ownership evidence)]
    (is (= 1 (count findings)))
    (is (= :critical (:severity (first findings))))
    (testing "the summary names the owner, its tap, and who disagrees"
      (is (str/includes? (:summary (first findings)) "host-a"))
      (is (str/includes? (:summary (first findings)) "one-256-0"))
      (is (str/includes? (:summary (first findings)) "host-b")))
    (testing "a gateway claim names the probable cause"
      (is (str/includes? (:hint (first findings)) "proxy-macip-advertisement")))
    (testing "a higher owner sequence is called out as not self-resolving"
      (is (str/includes? (:hint (first findings)) "will not resolve by itself")))))

(deftest a-mac-reported-remote-via-its-own-owner-is-not-a-finding
  (let [evidence (merge config
                        {"evpn-macs"
                         [{:host "host-a" :host-vtep host-a-vtep :vni 4208 :mac healthy
                           :type :local :intf "one-255-0" :local-seq 8 :remote-seq 7}
                          {:host "host-b" :host-vtep host-b-vtep :vni 4208 :mac healthy
                           :type :remote :vtep host-a-vtep :local-seq 0 :remote-seq 8}]})]
    (is (empty? (evpn/detect-mac-ownership evidence)))))

(deftest a-guest-mac-only-reachable-through-a-gateway-is-a-warning
  (let [evidence (merge config
                        {"evpn-macs"
                         [{:host "host-a" :host-vtep host-a-vtep :vni 4208
                           :mac "02:00:0a:18:00:08" :type :remote :vtep gateway}
                          {:host "host-b" :host-vtep host-b-vtep :vni 4208
                           :mac "02:00:0a:18:00:08" :type :remote :vtep gateway}]})
        findings (evpn/detect-mac-ownership evidence)]
    (is (= 1 (count findings)))
    (is (= :warning (:severity (first findings))))
    (is (str/includes? (:summary (first findings)) "only through a gateway"))))

(deftest infrastructure-macs-are-not-mistaken-for-guests
  (testing "a MAC outside the declared guest prefixes may legitimately sit behind a gateway"
    (let [evidence (merge config
                          {"evpn-macs"
                           [{:host "host-a" :host-vtep host-a-vtep :vni 4208
                             :mac "e4:8d:8c:00:13:37" :type :remote :vtep gateway}]})]
      (is (empty? (evpn/detect-mac-ownership evidence))))))

(deftest a-host-holding-both-a-tap-and-a-remote-entry-is-critical
  (let [evidence {"evpn-fdb"
                  [{:host "host-a" :mac hijacked :dev "lo.4208" :vxlan? true
                    :dst gateway :extern-learn? true :static? false}
                   {:host "host-a" :mac hijacked :dev "one-256-0" :vxlan? false
                    :dst nil :static? false}]}
        findings (evpn/detect-fdb-conflicts evidence)]
    (is (= 1 (count findings)))
    (is (= :critical (:severity (first findings))))
    (is (str/includes? (:summary (first findings)) "one-256-0"))
    (is (str/includes? (:hint (first findings)) "black-holed"))))

(deftest a-static-entry-is-reported-because-it-outranks-learning
  (let [findings (evpn/detect-static-fdb
                  {"evpn-fdb" [{:host "host-a" :mac hijacked :dev "lo.4208"
                                :dst host-b-vtep :static? true :vxlan? true}]})]
    (is (= 1 (count findings)))
    (is (= :warning (:severity (first findings))))
    (is (str/includes? (:summary (first findings)) host-b-vtep))))

(deftest every-host-unreadable-is-critical-because-nothing-was-compared
  (testing "a scan that collected nothing must not end by calling the rest clean"
    (let [findings (evpn/detect-host-problems
                    {"evpn-hosts" [{:host "host-a" :vni 4208 :error "host is not reachable over ssh"}
                                   {:host "host-b" :vni 4208 :error "host is not reachable over ssh"}]})]
      (is (= 2 (count findings)))
      (is (every? #(= :critical (:severity %)) findings))
      (is (str/includes? (:hint (first findings)) "unrun rather than clean")))))

(deftest an-unreadable-host-makes-the-scan-incomplete
  (let [findings (evpn/detect-host-problems
                  {"evpn-hosts" [{:host "host-c" :vni 4208 :error "host is not reachable over ssh"}
                                 {:host "host-a" :vni 4208 :macs [] :fdb []}]})]
    (is (= 1 (count findings)))
    (is (= :warning (:severity (first findings))))
    (is (str/includes? (:hint (first findings)) "incomplete"))))

(deftest the-gateway-check-judges-nothing-without-gateway-vteps
  (let [macs {"evpn-macs" [{:host "host-a" :host-vtep host-a-vtep :vni 4208
                            :mac "02:00:0a:18:00:08" :type :remote :vtep gateway}]}]
    (testing "with gateway VTEPs the unclaimed guest is reported"
      (is (= 1 (count (evpn/detect-gateway-only-macs (merge config macs))))))
    (testing "without them nothing is judged, and the check is unread through its :judges"
      (is (empty? (evpn/detect-gateway-only-macs macs)))
      (is (empty? (evpn/detect-gateway-only-macs (assoc macs "evpn-gateway-vteps" []))))
      (let [check (some #(when (= "evpn-gateway-only-mac" (:name %)) %) evpn/detectives)]
        (is (detective/unread? check macs))
        (is (detective/unread? check (assoc macs "evpn-gateway-vteps" [])))
        (is (not (detective/unread? check (merge config macs))))))))

(deftest the-two-ownership-judgements-are-separate-checks
  (let [evidence (merge config
                        {"evpn-macs"
                         [{:host "host-a" :host-vtep host-a-vtep :vni 4208 :mac hijacked
                           :type :local :intf "one-256-0"}
                          {:host "host-b" :host-vtep host-b-vtep :vni 4208 :mac hijacked
                           :type :remote :vtep gateway}
                          {:host "host-b" :host-vtep host-b-vtep :vni 4208
                           :mac "02:00:0a:18:00:08" :type :remote :vtep gateway}]})]
    (is (= [:critical] (map :severity (evpn/detect-ownership-conflicts evidence))))
    (is (= [:warning] (map :severity (evpn/detect-gateway-only-macs evidence))))
    (testing "detect-mac-ownership still returns both"
      (is (= [:critical :warning] (map :severity (evpn/detect-mac-ownership evidence)))))))

(deftest a-missing-judged-input-is-a-finding-naming-what-it-leaves-unjudged
  (let [row (fn [input source]
              {:input input :option (str "--" input) :env-path [:evpn (keyword input)] :source source})
        detect-missing-inputs (fn [evidence] (evpn/detect-missing-inputs (assoc evidence "evpn-macs" [])))]
    (testing "without the MAC tables nothing depends on the inputs, so nothing is reported"
      (is (empty? (evpn/detect-missing-inputs {"evpn-inputs" [(row "gateway-vteps" "missing")]}))))
    (testing "a supplied input is no finding"
      (is (empty? (detect-missing-inputs {"evpn-inputs" [(row "gateway-vteps" "option")
                                                              (row "guest-mac-prefixes" "env.edn")]}))))
    (testing "hosts and VNIs are never judged here"
      (is (empty? (detect-missing-inputs {"evpn-inputs" [(row "hosts" "missing") (row "vnis" "missing")]}))))
    (testing "missing gateway VTEPs leave a check unjudged, a warning"
      (is (= [{:severity :warning :component "--gateway-vteps"}]
             (map #(select-keys % [:severity :component])
                  (detect-missing-inputs {"evpn-inputs" [(row "gateway-vteps" "missing")]})))))
    (testing "missing prefixes widen a check, info"
      (is (= [:info] (map :severity (detect-missing-inputs {"evpn-inputs" [(row "guest-mac-prefixes" "missing")]})))))
    (testing "stderr lines follow the same inputs"
      (is (= ["no gateway VTEPs - pass --gateway-vteps or set :evpn :gateway-vteps in env.edn; evpn-gateway-only-mac cannot judge without them and is reported as unjudged"]
             (evpn/missing-input-lines {"evpn-inputs" [(row "gateway-vteps" "missing")]})))
      (is (empty? (evpn/missing-input-lines {"evpn-inputs" [(row "gateway-vteps" "option")]}))))))

(deftest resolving-the-values-a-scan-judges-against
  (let [env {[:evpn :hosts] ["env-host"] [:evpn :vnis] [4201]
             [:evpn :gateway-vteps] ["10.21.0.1"] [:evpn :guest-mac-prefixes] ["02:00:"]}
        lookup (fn [path] (get env path))
        none (constantly nil)]
    (testing "an option's items win over env.edn"
      (is (= {"hosts" {:value ["h1" "h2"] :source "option"}
              "vnis" {:value [4208] :source "option"}
              "gateway-vteps" {:value ["10.0.0.9"] :source "option"}
              "guest-mac-prefixes" {:value ["aa:"] :source "option"}}
             (collect/resolve-inputs {:hosts "h1, h2" :vni "4208,x" :gateway-vteps "10.0.0.9"
                                      :guest-mac-prefixes "aa:"}
                                     lookup))))
    (testing "env.edn answers what no option names"
      (is (= {:value ["10.21.0.1"] :source "env.edn"}
             (get (collect/resolve-inputs {} lookup) "gateway-vteps"))))
    (testing "a blank option, or one whose items do not parse, falls through"
      (is (= "env.edn" (get-in (collect/resolve-inputs {:gateway-vteps " , " :vni "x"} lookup) ["gateway-vteps" :source])))
      (is (= {:value [4201] :source "env.edn"} (get (collect/resolve-inputs {:vni "x"} lookup) "vnis"))))
    (testing "neither is missing, and so is an empty env.edn list"
      (is (= {:source "missing"} (get (collect/resolve-inputs {} none) "gateway-vteps")))
      (is (= {:source "missing"} (get (collect/resolve-inputs {} (constantly [])) "gateway-vteps"))))
    (testing "the evidence carries each judged value and where every value came from"
      (let [evidence (collect/input-evidence (collect/resolve-inputs {:hosts "h1" :vni "1"} none))]
        (is (nil? (get evidence "evpn-gateway-vteps")))
        (is (contains? evidence "evpn-gateway-vteps"))
        (is (= ["option" "option" "missing" "missing"] (map :source (get evidence "evpn-inputs"))))
        (is (= ["--hosts" "--vni" "--gateway-vteps" "--guest-mac-prefixes"]
               (map :option (get evidence "evpn-inputs")))))
      (is (= ["10.21.0.1"] (get (collect/input-evidence (collect/resolve-inputs {} lookup)) "evpn-gateway-vteps"))))))

(deftest a-clean-fabric-produces-nothing
  (is (empty? (evpn/detect-mac-ownership (merge config {"evpn-macs" []}))))
  (is (empty? (evpn/detect-fdb-conflicts {"evpn-fdb" []})))
  (is (empty? (evpn/detect-static-fdb {"evpn-fdb" []})))
  (is (empty? (evpn/detect-host-problems {"evpn-hosts" []}))))

(deftest missing-evidence-is-silent-rather-than-broken
  (testing "a kubectl-only scan provides none of these keys"
    (is (empty? (evpn/detect-mac-ownership {})))
    (is (empty? (evpn/detect-fdb-conflicts {})))
    (is (empty? (evpn/detect-static-fdb {})))
    (is (empty? (evpn/detect-host-problems {})))
    (is (empty? (evpn/detect-missing-inputs {})))))

;; --- parsers -----------------------------------------------------------------

(def ^:private mac-output
  (str "Number of MACs (local and remote) known for this VNI: 4\n"
       "Flags: N=sync-neighs, I=local-inactive, P=peer-active, X=peer-proxy\n"
       "MAC               Type   Flags Intf/Remote ES/VTEP            VLAN  Seq #'s\n"
       "02:00:0a:18:00:0d local        one-256-0                            2/0\n"
       "02:00:0a:18:00:08 remote       10.0.0.1                             0/0\n"
       "02:00:0a:18:00:06 remote       10.0.2.73                            0/8\n"
       "e4:8d:8c:00:13:37 remote       00:00:21:30:27:00:00:42:08:00        0/0\n"))

(deftest parsing-mac-rows
  (let [rows (vec (collect/parse-macs 4208 "host-a" host-a-vtep mac-output))]
    (is (= 4 (count rows)) "the header lines are not rows")
    (let [local (first (filter #(= :local (:type %)) rows))]
      (is (= hijacked (:mac local)))
      (is (= "one-256-0" (:intf local)))
      (is (= 2 (:local-seq local)) "the trailing pair is read from the end, not by column"))
    (let [remote (first (filter #(= gateway (:vtep %)) rows))]
      (is (= :remote (:type remote)))
      (is (= "02:00:0a:18:00:08" (:mac remote))))
    (testing "every row carries the host that reported it"
      (is (every? #(= "host-a" (:host %)) rows))
      (is (every? #(= host-a-vtep (:host-vtep %)) rows)))))

(deftest parsing-a-flags-column-that-is-present
  (testing "a short all-flag-letters token is skipped rather than read as the VTEP"
    (let [rows (vec (collect/parse-macs 4208 "host-a" host-a-vtep
                                        "02:00:0a:18:00:0d remote P     10.0.0.1     0/1\n"))]
      (is (= 1 (count rows)))
      (is (= gateway (:vtep (first rows)))))))

(deftest parsing-fdb-rows
  (let [out (str "02:00:0a:18:00:0d dev lo.4208 vlan 1 extern_learn master onebr8\n"
                 "02:00:0a:18:00:0d dev lo.4208 dst 10.0.0.1 self extern_learn\n"
                 "02:00:0a:18:00:0d dev one-256-0 master onebr8\n"
                 "02:00:0a:18:00:07 dev lo.4208 dst 10.0.2.73 self static\n"
                 "33:33:00:00:00:01 dev eth0 self permanent\n")
        rows (vec (collect/parse-fdb 4208 "host-a" out))]
    (is (= 5 (count rows)))
    (testing "the remote entry and the tap entry are distinguishable"
      (is (some #(and (:vxlan? %) (= gateway (:dst %))) rows))
      (is (some #(and (not (:vxlan? %)) (nil? (:dst %)) (= "one-256-0" (:dev %))) rows)))
    (testing "static is detected"
      (is (= 1 (count (filter :static? rows)))))
    (testing "the parsed rows feed the conflict detective"
      (is (= :critical (severity-of (evpn/detect-fdb-conflicts {"evpn-fdb" rows})
                                    "one-256-0"))))))

(deftest another-vnis-overlay-is-not-a-tap
  (testing "one fdb table holds every overlay, so a neighbouring VNI must be dropped"
    (let [out (str "02:00:0a:18:00:0d dev lo.4208 dst 10.0.0.1 self extern_learn\n"
                   "02:00:0a:18:00:0d dev one-256-0 master onebr8\n"
                   "02:00:2d:5c:31:d9 dev lo.4200 dst 10.0.2.73 self extern_learn\n"
                   "02:00:0a:15:09:0e dev lo.4201 dst 10.0.2.71 self extern_learn\n")
          rows (vec (collect/parse-fdb 4208 "host-a" out))]
      (is (= 2 (count rows)) "only this VNI's overlay and the tap survive")
      (is (every? #(#{"lo.4208" "one-256-0"} (:dev %)) rows))
      (testing "and the conflict detective then sees one finding, not three"
        (is (= 1 (count (evpn/detect-fdb-conflicts {"evpn-fdb" rows}))))))))

(deftest parsing-the-local-vtep
  (is (= "10.0.2.70" (collect/parse-local-vtep "VNI: 4208\n Local VTEP IP: 10.0.2.70\n")))
  (is (nil? (collect/parse-local-vtep "VNI: 4208\n")) "absent rather than wrong")
  (is (nil? (collect/parse-local-vtep nil))))

(deftest sudo-only-where-it-is-needed
  (let [with-sudo (collect/host-commands {:host "h" :needs-sudo? true} 4208)
        without (collect/host-commands {:host "h" :needs-sudo? false} 4208)]
    (testing "vtysh talks to FRR's socket"
      (is (= ["sudo" "vtysh" "-c" "show evpn mac vni 4208"] (:macs with-sudo)))
      (is (= ["vtysh" "-c" "show evpn mac vni 4208"] (:macs without))))
    (testing "the kernel's fdb is readable unprivileged either way"
      (is (= ["bridge" "fdb" "show"] (:fdb with-sudo)))
      (is (= ["bridge" "fdb" "show"] (:fdb without))))))

(deftest the-transport-receives-a-flat-argv
  (testing "a vector handed over whole becomes one unrunnable remote token"
    (let [seen (atom [])
          run (fn [_conn argv]
                (swap! seen conj argv)
                (when (= "show evpn mac vni 4208" (last argv)) mac-output))
          result (collect/collect-host {:host "host-a" :needs-sudo? true} 4208
                                       run (constantly true))]
      (is (every? (fn [argv] (every? string? argv)) @seen)
          "every token reaching the transport is a string, not a nested collection")
      (is (some #(= ["sudo" "vtysh" "-c" "show evpn mac vni 4208"] %) @seen))
      (is (nil? (:error result)))
      (is (= 4 (count (:macs result)))))))

(deftest an-unreachable-host-short-circuits
  (let [result (collect/collect-host {:host "host-a"} 4208
                                     (fn [_ _] (throw (ex-info "must not run" {})))
                                     (constantly false))]
    (is (= "host is not reachable over ssh" (:error result)))))

(deftest a-silent-command-is-an-error-not-an-empty-fabric
  (let [result (collect/collect-host {:host "host-a" :needs-sudo? true} 4208
                                     (fn [_ _] "") (constantly true))]
    (is (str/includes? (:error result) "no output from 'sudo vtysh"))
    (is (nil? (:macs result)) "an empty parse would read as a clean fabric")))

(deftest building-connections
  (let [conns (vec (collect/connections ["h1" "h2"] {:user "ops"}))]
    (is (= 2 (count conns)))
    (is (= "ops" (:user (first conns))))
    (is (every? :needs-sudo? conns) "vtysh needs sudo on the far end")
    (testing "sudo can be turned off explicitly"
      (is (not (:needs-sudo? (first (collect/connections ["h1"] {:needs-sudo? false}))))))))

(deftest the-full-fixture-reproduces-the-partition
  (testing "two hosts disagreeing about one MAC, as captured during a real outage"
    (let [evidence (merge config
                          {"evpn-macs"
                           (concat (collect/parse-macs 4208 "host-a" host-a-vtep mac-output)
                                   (collect/parse-macs 4208 "host-b" host-b-vtep
                                                       "02:00:0a:18:00:0d remote 10.0.0.1 0/0\n"))})
          findings (evpn/detect-mac-ownership evidence)]
      (is (= #{(str "MAC is local on host-a (one-256-0) but host-b reaches it via " gateway)}
             (summaries (filter #(= :critical (:severity %)) findings)))))))

;; --- per-conflict context: how long the MAC has been local, and BGP behind it -
;;
;; The recorded outputs follow FRR 10.2's text printers: zebra_evpn_print_mac
;; for `show evpn mac vni <vni> mac <mac>` and evpn_show_route_vni_macip with
;; route_vty_out_detail for `show bgp l2vpn evpn route vni <vni> mac <mac>`.

(def ^:private stale-vtep "10.21.2.70")
(def ^:private own-vtep "10.21.2.73")

(def ^:private mac-detail-weeks
  (str "MAC: 02:00:0a:18:00:16\n"
       " Intf: one-284-0(41) VLAN: 1\n"
       " Sync-info: neigh#: 0\n"
       " Local Seq: 1 Remote Seq: 0\n"
       " Uptime: 05w0d22h\n"
       " Neighbors:\n"
       "    No Neighbors\n"))

(def ^:private mac-detail-fresh
  (str "MAC: 02:00:0a:18:00:12\n"
       " Intf: one-270-1(57) VLAN: 1\n"
       " Sync-info: neigh#: 0\n"
       " Local Seq: 2 Remote Seq: 1\n"
       " Uptime: 00:04:27\n"
       " Neighbors:\n"
       "    10.24.0.18 Active\n"))

(def ^:private mac-detail-remote
  (str "MAC: 02:00:0a:18:00:16\n"
       " Remote VTEP: 10.21.2.70\n"
       " Sync-info: neigh#: 0\n"
       " Local Seq: 0 Remote Seq: 3\n"
       " Uptime: 4d09h18m\n"
       " Neighbors:\n"
       "    No Neighbors\n"))

(def ^:private bgp-absent "% Network not in table\n")

(def ^:private bgp-from-stale-vtep
  (str "BGP routing table entry for [2]:[0]:[48]:[02:00:0a:18:00:12]\n"
       "Paths: (1 available, best #1)\n"
       "  Not advertised to any peer\n"
       "  Route [2]:[0]:[48]:[02:00:0a:18:00:12] VNI 4208\n"
       "  65102 65000 65101\n"
       "    10.21.2.70 from 10.21.1.1 (10.21.1.1)\n"
       "      Origin IGP, valid, external, bestpath-from-AS 65102, best (First path received)\n"
       "      Extended Community: RT:65101:4208 ET:8 MM:1\n"
       "      Last update: Tue Nov 24 10:01:12 2026\n"
       "\n"
       "Displayed 1 paths for requested prefix\n"))

(def ^:private bgp-own-route-only
  (str "BGP routing table entry for [2]:[0]:[48]:[02:00:0a:18:00:16]\n"
       "Paths: (1 available, best #1)\n"
       "  Advertised to non peer-group peers:\n"
       "  spine-1(10.21.1.1) spine-2(10.21.1.2)\n"
       "  Route [2]:[0]:[48]:[02:00:0a:18:00:16] VNI 4208\n"
       "  Local\n"
       "    10.21.2.73 from 0.0.0.0 (10.21.2.73)\n"
       "      Origin IGP, weight 32768, valid, sourced, local, bestpath-from-AS Local, best (First path received)\n"
       "      Extended Community: ET:8 RT:65103:4208 MM:2\n"
       "      Last update: Tue Nov 24 10:05:44 2026\n"
       "\n"
       "Displayed 1 paths for requested prefix\n"))

(deftest parsing-frr-uptimes
  (testing "the three shapes frrtime_to_interval prints"
    (is (= 267 (collect/uptime-seconds "00:04:27")))
    (is (= (+ (* 4 86400) (* 9 3600) (* 18 60)) (collect/uptime-seconds "4d09h18m")))
    (is (= (+ (* 5 7 86400) (* 22 3600)) (collect/uptime-seconds "05w0d22h"))))
  (testing "anything else is unknown rather than zero"
    (is (nil? (collect/uptime-seconds "never")))
    (is (nil? (collect/uptime-seconds nil)))))

(deftest parsing-the-mac-detail
  (testing "a local MAC carries its tap, sequences and uptime"
    (is (= {:read? true :state :local :intf "one-284-0" :remote-vtep nil
            :local-seq 1 :remote-seq 0 :uptime "05w0d22h"
            :uptime-seconds (+ (* 35 86400) (* 22 3600))}
           (collect/parse-mac-detail mac-detail-weeks))))
  (testing "a MAC FRR holds as remote names the VTEP instead of a tap"
    (let [parsed (collect/parse-mac-detail mac-detail-remote)]
      (is (= :remote (:state parsed)))
      (is (= stale-vtep (:remote-vtep parsed)))
      (is (nil? (:intf parsed)))))
  (testing "FRR not knowing the MAC is an answer"
    (is (= {:read? true :state :absent}
           (collect/parse-mac-detail "% Requested MAC does not exist in VNI 4208\n"))))
  (testing "silence or unrecognised output is unread, never an answer"
    (is (false? (:read? (collect/parse-mac-detail ""))))
    (is (false? (:read? (collect/parse-mac-detail nil))))
    (is (false? (:read? (collect/parse-mac-detail "% VNI 4208 does not exist\n"))))
    (is (false? (:read? (collect/parse-mac-detail "vtysh: command not found\n"))))))

(deftest parsing-the-bgp-route
  (testing "no route at all"
    (is (= {:read? true :next-hops []} (collect/parse-bgp-mac-route bgp-absent))))
  (testing "each path's next hop is read, not its peer"
    (is (= {:read? true :next-hops [stale-vtep]}
           (collect/parse-bgp-mac-route bgp-from-stale-vtep)))
    (is (= {:read? true :next-hops [own-vtep]}
           (collect/parse-bgp-mac-route bgp-own-route-only))))
  (testing "a next hop printed with its hostname"
    (is (= [stale-vtep]
           (:next-hops (collect/parse-bgp-mac-route
                        (str/replace bgp-from-stale-vtep
                                     "    10.21.2.70 from" "    10.21.2.70(safe-sole) from"))))))
  (testing "silence, an unknown VNI or unrecognised output is unread"
    (is (false? (:read? (collect/parse-bgp-mac-route ""))))
    (is (false? (:read? (collect/parse-bgp-mac-route "VNI not found\n"))))
    (is (false? (:read? (collect/parse-bgp-mac-route "sudo: a password is required\n"))))))

(deftest conflict-commands-name-the-vni-and-mac
  (is (= {:mac-detail ["sudo" "vtysh" "-c" "show evpn mac vni 4208 mac 02:00:0a:18:00:16"]
          :bgp-route ["sudo" "vtysh" "-c" "show bgp l2vpn evpn route vni 4208 mac 02:00:0a:18:00:16"]}
         (collect/conflict-commands {:host "h" :needs-sudo? true} 4208 "02:00:0a:18:00:16")))
  (is (= ["vtysh" "-c" "show evpn mac vni 4201 mac 02:00:0a:15:09:0d"]
         (:mac-detail (collect/conflict-commands {:host "h" :needs-sudo? false}
                                                 4201 "02:00:0a:15:09:0d")))))

(def ^:private conflict-fdb
  [{:host "upward-cobra" :mac "02:00:0a:18:00:16" :dev "lo.4208" :vxlan? true
    :dst stale-vtep :extern-learn? true :static? false}
   {:host "upward-cobra" :mac "02:00:0a:18:00:16" :dev "one-284-0" :vxlan? false
    :dst nil :static? false}])

(deftest a-conflict-names-its-vni-from-the-overlay-device
  (is (= [{:host "upward-cobra" :mac "02:00:0a:18:00:16" :vni 4208
           :local-intf "one-284-0" :stale-remote-vtep stale-vtep}]
         (collect/fdb-conflicts conflict-fdb))))

(defn- conflict-finding
  [context]
  (first (evpn/detect-fdb-conflicts
          (cond-> {"evpn-fdb" conflict-fdb}
            context (assoc "evpn-fdb-context"
                           {["upward-cobra" 4208 "02:00:0a:18:00:16"] context})))))

(deftest an-orphan-is-a-conflict-no-route-stands-behind
  (let [finding (conflict-finding
                 {:mac-detail (collect/parse-mac-detail mac-detail-weeks)
                  :bgp-route (collect/parse-bgp-mac-route bgp-absent)})]
    (testing "the existing fields are unchanged"
      (is (= :critical (:severity finding)))
      (is (= "upward-cobra/02:00:0a:18:00:16" (:component finding)))
      (is (= (str "host holds both a local tap entry (one-284-0) and a remote entry (dst "
                  stale-vtep ") for one MAC")
             (:summary finding))))
    (is (= "05w0d22h" (:local-uptime finding)))
    (is (= (+ (* 35 86400) (* 22 3600)) (:local-uptime-seconds finding)))
    (is (= "absent" (:bgp-route finding)))
    (is (= "orphan" (:classification finding)))
    (is (= 4208 (:vni finding)))
    (is (= stale-vtep (:stale-remote-vtep finding)))
    (testing "the text report carries both values"
      (is (str/includes? (:hint finding) "05w0d22h"))
      (is (str/includes? (:hint finding) "orphan")))))

(deftest a-route-from-another-vtep-does-not-stand-behind-the-stale-entry
  (testing "the host's own advertisement of its local MAC is not the stale VTEP's"
    (let [finding (conflict-finding
                   {:mac-detail (collect/parse-mac-detail mac-detail-weeks)
                    :bgp-route (collect/parse-bgp-mac-route bgp-own-route-only)})]
      (is (= "absent" (:bgp-route finding)))
      (is (= [own-vtep] (:bgp-next-hops finding)))
      (is (= "orphan" (:classification finding))))))

(deftest a-route-from-the-stale-vtep-is-a-live-misadvertisement
  (let [finding (conflict-finding
                 {:mac-detail (collect/parse-mac-detail mac-detail-fresh)
                  :bgp-route (collect/parse-bgp-mac-route bgp-from-stale-vtep)})]
    (is (= "00:04:27" (:local-uptime finding)))
    (is (= 267 (:local-uptime-seconds finding)))
    (is (= "present" (:bgp-route finding)))
    (is (= "live-misadvertisement" (:classification finding)))
    (is (str/includes? (:hint finding) "relearn"))))

(deftest a-failed-read-is-unread-never-no-route
  (let [finding (conflict-finding
                 {:mac-detail (collect/parse-mac-detail "")
                  :bgp-route (collect/parse-bgp-mac-route "")})]
    (is (= "unread" (:bgp-route finding)))
    (is (= "unknown" (:classification finding)))
    (is (nil? (:local-uptime finding)))
    (is (= "unread" (:frr-mac finding)))
    (is (str/includes? (:hint finding) "could not be read")))
  (testing "context that was never collected reads the same"
    (let [finding (conflict-finding nil)]
      (is (= "unread" (:bgp-route finding)))
      (is (= "unknown" (:classification finding)))
      (is (= :critical (:severity finding))))))

(deftest a-mac-frr-holds-as-remote-has-no-local-uptime
  (testing "FRR's uptime of a remote entry is not how long the guest has been here"
    (let [finding (conflict-finding
                   {:mac-detail (collect/parse-mac-detail mac-detail-remote)
                    :bgp-route (collect/parse-bgp-mac-route bgp-from-stale-vtep)})]
      (is (= "remote" (:frr-mac finding)))
      (is (nil? (:local-uptime finding)))
      (is (= "live-misadvertisement" (:classification finding))))))

(defn- stub-ssh
  "A gumshoe.shell/execute stand-in answering per remote command, recording each
  remote argv after the ssh destination."
  [seen answers]
  (fn [& args]
    (let [argv (vec (flatten args))
          remote (vec (drop (inc (.indexOf argv "--")) argv))
          remote (vec (rest remote))]
      (swap! seen conj remote)
      (let [line (str/join " " remote)
            out (some (fn [[needle out]] (when (str/includes? line needle) out)) answers)]
        {:exit 0 :out (or out "") :err ""}))))

(deftest collecting-reads-context-only-for-conflicts
  (let [seen (atom [])
        fdb (str "02:00:0a:18:00:16 dev lo.4208 vlan 1 extern_learn master onebr8\n"
                 "02:00:0a:18:00:16 dev lo.4208 dst 10.21.2.70 self extern_learn\n"
                 "02:00:0a:18:00:16 dev one-284-0 master onebr8\n"
                 "02:00:0a:18:00:13 dev one-278-0 master onebr8\n")
        answers [["'show evpn vni 4208'" " Local VTEP IP: 10.21.2.73\n"]
                 ["'show evpn mac vni 4208 mac 02:00:0a:18:00:16'" mac-detail-weeks]
                 ["'show evpn mac vni 4208'" "02:00:0a:18:00:16 local one-284-0 1/0\n"]
                 ["'show bgp l2vpn evpn route vni 4208 mac 02:00:0a:18:00:16'" bgp-absent]
                 ["bridge fdb show" fdb]]
        evidence (binding [gumshoe.ssh/*user-config* nil]
                   (with-redefs [gumshoe.shell/execute (stub-ssh seen answers)]
                     (collect/collect-evidence! [{:host "upward-cobra" :needs-sudo? true}]
                                                [4208])))
        context (get-in evidence ["evpn-fdb-context" ["upward-cobra" 4208 "02:00:0a:18:00:16"]])]
    (testing "the two reads name the conflict's VNI and MAC"
      (is (some #{["sudo" "vtysh" "-c" "'show evpn mac vni 4208 mac 02:00:0a:18:00:16'"]} @seen))
      (is (some #{["sudo" "vtysh" "-c" "'show bgp l2vpn evpn route vni 4208 mac 02:00:0a:18:00:16'"]}
                @seen)))
    (testing "no MAC without a conflict is read"
      (is (not-any? #(str/includes? (str/join " " %) "02:00:0a:18:00:13") @seen)))
    (is (= "05w0d22h" (get-in context [:mac-detail :uptime])))
    (is (= {:read? true :next-hops []} (:bgp-route context)))
    (is (= "orphan" (:classification (first (evpn/detect-fdb-conflicts evidence)))))))

(deftest a-fabric-without-conflicts-reads-nothing-more
  (let [seen (atom [])
        answers [["'show evpn vni 4208'" " Local VTEP IP: 10.21.2.73\n"]
                 ["'show evpn mac vni 4208'" "02:00:0a:18:00:13 local one-278-0 1/0\n"]
                 ["bridge fdb show" "02:00:0a:18:00:13 dev one-278-0 master onebr8\n"]]
        evidence (binding [gumshoe.ssh/*user-config* nil]
                   (with-redefs [gumshoe.shell/execute (stub-ssh seen answers)]
                     (collect/collect-evidence! [{:host "secure-emu" :needs-sudo? true}]
                                                [4208])))]
    (is (= {} (get evidence "evpn-fdb-context")))
    (is (not-any? #(re-find #"vni \d+ mac " (str/join " " %)) @seen))))
