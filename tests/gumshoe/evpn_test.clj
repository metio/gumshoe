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
            [gumshoe.detectives.evpn :as evpn]
            [gumshoe.evpn :as collect]))

(def ^:private gateway "10.0.0.1")
(def ^:private host-a-vtep "10.0.2.70")
(def ^:private host-b-vtep "10.0.2.73")
(def ^:private hijacked "02:00:0a:18:00:0d")
(def ^:private healthy "02:00:0a:18:00:06")

(def ^:private config
  {"config" {:evpn {:gateway-vteps [gateway]
                    :guest-mac-prefixes ["02:00:"]}}})

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
    (is (empty? (evpn/detect-host-problems {})))))

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
