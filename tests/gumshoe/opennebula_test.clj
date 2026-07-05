;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.opennebula-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.opennebula :as detectives]
            [gumshoe.opennebula :as opennebula]))

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
