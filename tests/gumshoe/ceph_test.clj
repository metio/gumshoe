;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ceph-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.ceph :as ceph]
            [gumshoe.detectives.ceph :as detectives]
            [gumshoe.ssh :as ssh]))

(defn- summaries
  [findings]
  (set (map :summary findings)))

(deftest command-assembly-test
  (testing "ssh runs unattended, and '--' precedes the host so the remote command is clean"
    (is (= ["ssh" "-q" "-o" "BatchMode=yes" "-o" "ConnectTimeout=5"
            "--" "ceph-1.example.org" "exit" "0"]
           (ssh/ssh-args {:host "ceph-1.example.org"} ["exit" "0"]))))
  (testing "the '--' option terminator is never placed after the host"
    (let [args (ssh/ssh-args {:host "h"} ["ceph" "status"])
          host-index (.indexOf args "h")
          terminator-index (.indexOf args "--")]
      (is (< terminator-index host-index))
      (is (= "ceph" (nth args (inc host-index))))))
  (testing "an explicit user wins over the ssh config"
    (is (= "ops@ceph-1" (ssh/target {:host "ceph-1" :user "ops"})))
    (is (= "ceph-1" (ssh/target {:host "ceph-1"}))))
  (testing "ceph runs directly by default (mgr/admin host), no sudo, no container"
    (is (= ["ceph" "status"] (ceph/ceph-args {} ["ceph" "status"]))))
  (testing "cephadm-shell mode wraps the command for hosts with only the cephadm binary"
    (is (= ["sudo" "cephadm" "shell" "--" "ceph" "status"]
           (ceph/ceph-args {:cephadm-shell? true} ["ceph" "status"]))))
  (testing "ceph/effect is an ssh effect carrying the mode-correct ceph command"
    (is (= [:ssh {:host "mgr-1"} "ceph" "osd" "out" "3"]
           (ceph/effect {:host "mgr-1"} "osd" "out" "3")))
    (is (= [:ssh {:host "mgr-1" :cephadm-shell? true} "sudo" "cephadm" "shell" "--" "ceph" "osd" "out" "3"]
           (ceph/effect {:host "mgr-1" :cephadm-shell? true} "osd" "out" "3"))))
  (testing "the connection builder derives the mode and sudo need from the flag"
    (is (= {:host "h" :user nil :cephadm-shell? false :needs-sudo? false}
           (ceph/connection {:host "h"})))
    (is (= {:host "h" :user "ops" :cephadm-shell? true :needs-sudo? true}
           (ceph/connection {:host "h" :user "ops" :cephadm-shell true})))))

(deftest osd-helpers-test
  (testing "OSD ids parse from both notations, garbage stays nil"
    (is (= 3 (ceph/osd-id "3")))
    (is (= 3 (ceph/osd-id "osd.3")))
    (is (nil? (ceph/osd-id "mon.a"))))
  (let [osds [{:osd 0 :up 1 :in 1} {:osd 1 :up 0 :in 0}]]
    (testing "names, lookup, and state predicates"
      (is (= "osd.0" (ceph/osd-name (first osds))))
      (is (= 1 (:osd (ceph/osd-numbered osds 1))))
      (is (ceph/osd-in? (first osds)))
      (is (not (ceph/osd-in? (second osds))))
      (is (ceph/osd-up? (first osds)))
      (is (not (ceph/osd-up? (second osds)))))))

(deftest daemon-helpers-test
  (let [daemons [{:daemon_name "osd.3" :status_desc "running" :started "2026-07-04T10:00:00"}
                 {:daemon_name "mon.ceph-1" :status_desc "error"}]]
    (is (= "running" (:status_desc (ceph/daemon-named daemons "osd.3"))))
    (is (nil? (ceph/daemon-named daemons "osd.9")))
    (is (ceph/daemon-running? (first daemons)))
    (is (not (ceph/daemon-running? (second daemons))))))

(deftest upgrade-helpers-test
  (testing "version-strings pulls the distinct versions from the overall map"
    (is (= ["ceph version 17.2.9 (abc) quincy (stable)"]
           (ceph/version-strings {:overall {(keyword "ceph version 17.2.9 (abc) quincy (stable)") 52}}))))
  (testing "all-on-version? is true only when one version matches the target"
    (is (ceph/all-on-version? {:overall {(keyword "ceph version 17.2.9 (abc) quincy (stable)") 52}} "17.2.9"))
    (is (not (ceph/all-on-version? {:overall {(keyword "ceph version 16.2.13 (x) pacific") 52}} "17.2.9")))
    (testing "a mixed cluster mid-upgrade is not done"
      (is (not (ceph/all-on-version? {:overall {(keyword "ceph version 16.2.13 (x) pacific") 10
                                                (keyword "ceph version 17.2.9 (y) quincy") 42}}
                                     "17.2.9")))))
  (testing "upgrade-in-progress? reads the cephadm flag"
    (is (ceph/upgrade-in-progress? {:in_progress true}))
    (is (not (ceph/upgrade-in-progress? {:in_progress false})))
    (is (not (ceph/upgrade-in-progress? {})))))

(deftest health-checks-test
  (let [evidence {"status" {:health {:status "HEALTH_WARN"
                                     :checks {:OSD_NEARFULL {:severity "HEALTH_WARN"
                                                             :summary {:message "1 nearfull osd(s)"}}
                                              :PG_DAMAGED {:severity "HEALTH_ERR"
                                                           :summary {:message "Possible data damage"}}}}}}
        findings (detectives/detect-health-checks evidence)]
    (is (= #{"1 nearfull osd(s)" "Possible data damage"} (summaries findings)))
    (is (= :critical (:severity (first (filter #(= "PG_DAMAGED" (:component %)) findings)))))))

(deftest osd-problems-test
  (is (= #{"1 of 12 OSDs are down" "2 of 12 OSDs are out"}
         (summaries (detectives/detect-osd-problems
                     {"status" {:osdmap {:num_osds 12 :num_up_osds 11 :num_in_osds 10}}}))))
  (is (empty? (detectives/detect-osd-problems
               {"status" {:osdmap {:num_osds 12 :num_up_osds 12 :num_in_osds 12}}}))))

(deftest pg-problems-test
  (let [findings (detectives/detect-pg-problems
                  {"status" {:pgmap {:pgs_by_state [{:state_name "active+clean" :count 500}
                                                    {:state_name "active+clean+scrubbing" :count 2}
                                                    {:state_name "active+undersized+degraded" :count 7}
                                                    {:state_name "down" :count 1}]}}})]
    (is (= #{"7 PGs are active+undersized+degraded" "1 PGs are down"}
           (summaries findings)))
    (is (= :critical (:severity (first (filter #(= "1 PGs are down" (:summary %)) findings)))))))

(deftest quorum-test
  (is (= #{"only 2 of 3 monitors are in quorum"}
         (summaries (detectives/detect-quorum-problems
                     {"status" {:quorum_names ["a" "b"] :monmap {:num_mons 3}}}))))
  (is (empty? (detectives/detect-quorum-problems
               {"status" {:quorum_names ["a" "b" "c"] :monmap {:num_mons 3}}}))))

(deftest capacity-test
  (let [findings (detectives/detect-capacity-problems
                  {"df" {:stats {:total_used_raw_ratio 0.87}
                         :pools [{:name "rbd" :stats {:percent_used 0.9}}
                                 {:name "cephfs" :stats {:percent_used 0.5}}]}})]
    (is (= #{"raw capacity is 87% used" "pool is 90% used"} (summaries findings)))
    (is (every? #(= :critical (:severity %)) findings))))

(deftest osd-utilization-test
  (is (= #{"OSD is 92% full"}
         (summaries (detectives/detect-osd-utilization
                     {"osd-df" {:nodes [{:name "osd.3" :utilization 91.7}
                                        {:name "osd.4" :utilization 55.0}]}})))))

(deftest services-test
  (is (= #{"2 of 3 daemons are running"}
         (summaries (detectives/detect-service-problems
                     {"orch-ls" [{:service_name "mon" :status {:running 2 :size 3}}
                                 {:service_name "mgr" :status {:running 2 :size 2}}]})))))

(deftest crashes-test
  (is (= #{"2 unarchived daemon crash(es)"}
         (summaries (detectives/detect-new-crashes
                     {"crash-ls-new" [{:crash_id "a"} {:crash_id "b"}]}))))
  (is (empty? (detectives/detect-new-crashes {"crash-ls-new" []}))))
