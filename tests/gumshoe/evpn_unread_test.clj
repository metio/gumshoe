;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.evpn-unread-test
  "The fabric's MAC and fdb tables are nil when no host answered, so the
   checks over them are unread instead of clean."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.evpn :as evpn]
            [gumshoe.evpn :as collect]
            [gumshoe.shell :as shell]))

(defn- unread-names
  [evidence]
  (set (map :name (filter :unread (detective/mark-unread evpn/detectives evidence)))))

(def ^:private table-checks
  #{"evpn-mac-ownership" "evpn-fdb-conflict" "evpn-static-fdb"})

(deftest no-answering-host-leaves-the-tables-unread-test
  (let [evidence (collect/host-evidence [{:host "h1" :vni 1 :error "host is not reachable over ssh"}
                                         {:host "h2" :vni 1 :error "no output"}])]
    (is (nil? (get evidence "evpn-macs")))
    (is (nil? (get evidence "evpn-fdb")))
    (is (every? (unread-names evidence) table-checks) (pr-str (unread-names evidence)))
    (testing "the host check still reads and names the gap"
      (is (not (contains? (unread-names evidence) "evpn-hosts"))))))

(deftest one-answering-host-reads-the-tables-test
  (let [mac {:vni 1 :mac "02:00:00:00:00:01" :type :local :host "h2" :vtep "10.0.0.2" :intf "tap0"}
        evidence (collect/host-evidence [{:host "h1" :vni 1 :error "host is not reachable over ssh"}
                                         {:host "h2" :vni 1 :macs [mac] :fdb []}])]
    (is (= [mac] (get evidence "evpn-macs")))
    (is (= [] (get evidence "evpn-fdb")))
    (is (not-any? (unread-names evidence) table-checks))))

(deftest no-host-at-all-leaves-the-tables-unread-test
  (let [evidence (collect/host-evidence [])]
    (is (nil? (get evidence "evpn-macs")))
    (is (every? (unread-names evidence) table-checks))))

(deftest a-collection-over-unreachable-hosts-is-unread-test
  (let [evidence (with-redefs [shell/execute (fn [& _] {:exit 255 :out "" :err "ssh: connect to host h1 port 22: No route to host"})]
                   (collect/collect-evidence! [{:host "h1" :needs-sudo? true}] [1]))]
    (is (nil? (get evidence "evpn-macs")))
    (is (nil? (get evidence "evpn-fdb")))
    (is (nil? (get evidence "evpn-fdb-context")))
    (is (every? (unread-names evidence) table-checks))
    (is (= :critical (:severity (first (evpn/detect-host-problems evidence)))))))
