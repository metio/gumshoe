;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.opennebula-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.opennebula :as detectives]
            [gumshoe.opennebula :as opennebula]
            [gumshoe.shell :as shell]))

(defn- summaries
  [findings]
  (set (map :summary findings)))

(deftest as-seq-test
  (testing "a single element, a list, and nothing all normalize to a seq"
    (is (= [{:ID "0"}] (opennebula/as-seq {:ID "0"})))
    (is (= [{:ID "0"} {:ID "1"}] (opennebula/as-seq [{:ID "0"} {:ID "1"}])))
    (is (= [] (opennebula/as-seq nil)))))

(deftest host-detective-test
  (let [evidence {"hosts" [{:NAME "kvm-1" :STATE "2"}
                           {:NAME "kvm-2" :STATE "3"}
                           {:NAME "kvm-3" :STATE "8"}
                           {:NAME "kvm-4" :STATE "4"}]}
        findings (detectives/detect-host-problems evidence)]
    (testing "monitored hosts are silent, error is critical, offline warns, disabled informs"
      (is (= #{"host is ERROR" "host is OFFLINE" "host is DISABLED"} (summaries findings)))
      (is (= :critical (:severity (first (filter #(= "kvm-2" (:component %)) findings)))))
      (is (= :warning (:severity (first (filter #(= "kvm-3" (:component %)) findings)))))
      (is (= :info (:severity (first (filter #(= "kvm-4" (:component %)) findings))))))))

(deftest vm-detective-test
  (is (= #{"VM is FAILED" "VM is CLONING_FAILURE"}
         (summaries (detectives/detect-vm-problems
                     {"vms" [{:NAME "web" :ID "10" :STATE "3"}
                             {:NAME "broken" :ID "11" :STATE "7"}
                             {:NAME "clone" :ID "12" :STATE "11"}]})))))

(deftest datastore-usage-test
  (is (= 80.0 (detectives/datastore-usage {:TOTAL_MB "1000" :FREE_MB "200"})))
  (is (nil? (detectives/datastore-usage {:TOTAL_MB "0" :FREE_MB "0"})))
  (is (nil? (detectives/datastore-usage {:TOTAL_MB "bad" :FREE_MB "200"}))))

(deftest datastore-detective-test
  (let [evidence {"datastores" [{:NAME "system" :TOTAL_MB "1000" :FREE_MB "500"}
                                {:NAME "images" :TOTAL_MB "1000" :FREE_MB "200"}
                                {:NAME "backups" :TOTAL_MB "1000" :FREE_MB "50"}]}
        findings (detectives/detect-datastore-problems evidence)]
    (testing "under 75% is silent, over warns, over 85% is critical"
      (is (= #{"datastore is 80% full" "datastore is 95% full"} (summaries findings)))
      (is (= :warning (:severity (first (filter #(= "images" (:component %)) findings)))))
      (is (= :critical (:severity (first (filter #(= "backups" (:component %)) findings))))))))

(deftest an-active-vm-whose-lifecycle-failed-is-reported
  (is (= #{["VM is BOOT_FAILURE" :critical] ["VM is PROLOG_FAILURE" :critical] ["VM is UNKNOWN" :warning]}
         (set (map (juxt :summary :severity)
                   (detectives/detect-vm-problems
                    {"vms" [{:NAME "web" :ID "10" :STATE "3" :LCM_STATE "3"}
                            {:NAME "boot" :ID "13" :STATE "3" :LCM_STATE "36"}
                            {:NAME "prolog" :ID "14" :STATE "3" :LCM_STATE "39"}
                            {:NAME "lost" :ID "15" :STATE "3" :LCM_STATE "16"}
                            {:NAME "off" :ID "16" :STATE "8" :LCM_STATE "36"}]}))))))

(deftest pool-elements-test
  (testing "an answered pool is its elements, an empty one an empty list"
    (is (= [{:ID "0"}] (opennebula/pool-elements {:exit 0 :out "{\"HOST_POOL\":{\"HOST\":{\"ID\":\"0\"}}}"}
                                                 :HOST_POOL :HOST)))
    (is (= [] (opennebula/pool-elements {:exit 0 :out "{\"HOST_POOL\":{}}"} :HOST_POOL :HOST))))
  (testing "a command that did not answer leaves the pool nil"
    (is (nil? (opennebula/pool-elements {:exit 255 :out ""} :HOST_POOL :HOST)) "ssh failed")
    (is (nil? (opennebula/pool-elements {:exit 0 :out ""} :HOST_POOL :HOST)) "printed nothing")
    (is (nil? (opennebula/pool-elements {:exit 0 :out "[ONE] auth failure"} :HOST_POOL :HOST)) "not JSON")
    (is (nil? (opennebula/pool-elements {:exit 0 :out "{\"VM_POOL\":{}}"} :HOST_POOL :HOST)) "another pool")
    (is (nil? (opennebula/pool-elements {:exit 1 :out "{\"HOST_POOL\":{}}"} :HOST_POOL :HOST))
        "a document printed before a failing exit")))

(deftest failed-commands-leave-every-check-unread-test
  (with-redefs [shell/execute (fn [& _] {:exit 255 :out "" :err "oned is not running"})]
    (let [evidence (opennebula/collect-evidence! {:host "frontend.example.org"})]
      (is (nil? (get evidence "hosts")))
      (is (nil? (get evidence "vms")))
      (is (nil? (get evidence "datastores")))
      (is (= #{"opennebula-hosts" "opennebula-vms" "opennebula-datastores"}
             (set (map :name (filter :unread (detective/mark-unread detectives/detectives evidence)))))))))

(deftest answered-commands-are-read-test
  (with-redefs [shell/execute (fn [& args]
                                (let [command (some #{"onehost" "onevm" "onedatastore"} args)]
                                  {:exit 0
                                   :out (case command
                                          "onehost" "{\"HOST_POOL\":{\"HOST\":[{\"NAME\":\"kvm-1\",\"STATE\":\"2\"}]}}"
                                          "onevm" "{\"VM_POOL\":{}}"
                                          "onedatastore" "{\"DATASTORE_POOL\":{\"DATASTORE\":{\"NAME\":\"ds\"}}}")}))]
    (let [evidence (opennebula/collect-evidence! {:host "frontend.example.org"})]
      (is (= 1 (count (get evidence "hosts"))))
      (is (= [] (get evidence "vms")))
      (is (empty? (filter :unread (detective/mark-unread detectives/detectives evidence)))))))
