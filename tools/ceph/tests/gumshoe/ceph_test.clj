;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ceph-test
  (:require [clojure.test :refer [deftest is testing]]
            [babashka.fs]
            [gumshoe.ceph :as ceph]
            [gumshoe.interact]
            [gumshoe.runbook :as runbook]
            [gumshoe.detectives.ceph :as detectives]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]
            [gumshoe.tools.ceph :as tools-ceph]))

(defn- summaries
  [findings]
  (set (map :summary findings)))

(deftest command-assembly-test
  (testing "ssh runs unattended, and '--' precedes the host so the remote command is clean"
    (is (= ["ssh" "-q" "-o" "BatchMode=yes" "-o" "ConnectTimeout=5"
            "-o" "ServerAliveInterval=15" "-o" "ServerAliveCountMax=4"
            "--" "ceph-1.example.org" "exit" "0"]
           (binding [ssh/*user-config* nil]
             (ssh/ssh-args {:host "ceph-1.example.org"} ["exit" "0"])))))
  (testing "a user ssh config is named with -F, so a system config owned by an unmapped uid is never read"
    (is (= ["ssh" "-q" "-F" "/home/op/.ssh/config" "-o" "BatchMode=yes" "-o" "ConnectTimeout=5"
            "-o" "ServerAliveInterval=15" "-o" "ServerAliveCountMax=4"
            "--" "h" "exit" "0"]
           (binding [ssh/*user-config* "/home/op/.ssh/config"]
             (ssh/ssh-args {:host "h"} ["exit" "0"])))))
  (testing "an argument with a space is quoted, or the remote shell splits it"
    (is (= ["ssh" "-q" "-o" "BatchMode=yes" "-o" "ConnectTimeout=5"
            "-o" "ServerAliveInterval=15" "-o" "ServerAliveCountMax=4" "--" "h"
            "vtysh" "-c" "'show evpn mac vni 4208'"]
           (binding [ssh/*user-config* nil]
             (ssh/ssh-args {:host "h"} ["vtysh" "-c" "show evpn mac vni 4208"])))))
  (testing "a token needing no quoting is passed through, so a reproducer stays readable"
    (is (= ["ceph" "status"]
           (binding [ssh/*user-config* nil]
             (subvec (ssh/ssh-args {:host "h"} ["ceph" "status"]) 12)))))
  (testing "keepalives are on by default and a connection overrides them"
    (let [options (fn [connection]
                    (binding [ssh/*user-config* nil]
                      (filterv #(re-find #"^ServerAlive" %) (ssh/ssh-args connection ["true"]))))]
      (is (= ["ServerAliveInterval=15" "ServerAliveCountMax=4"] (options {:host "h"})))
      (is (= ["ServerAliveInterval=30" "ServerAliveCountMax=2"]
             (options {:host "h" :server-alive-interval 30 :server-alive-count-max 2})))
      (is (= ["ServerAliveInterval=0" "ServerAliveCountMax=4"] (options {:host "h" :server-alive-interval 0}))
          "an interval of 0 turns the keepalives off")
      (is (= ["ServerAliveInterval=15" "ServerAliveCountMax=4"]
             (options {:host "h" :server-alive-interval -1 :server-alive-count-max "x"}))
          "a value ssh would refuse falls back to the default")))
  (testing "an embedded single quote survives"
    (is (= "'it'\\''s'" (ssh/shell-quote "it's"))))
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
  (testing "an --osd value becomes the name the candidates carry, garbage stays as given"
    (is (= "osd.3" (ceph/osd-pick "3")))
    (is (= "osd.3" (ceph/osd-pick "osd.3")))
    (is (= "mon.a" (ceph/osd-pick "mon.a")))
    (is (nil? (ceph/osd-pick nil))))
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
    (is (not (ceph/upgrade-in-progress? {}))))
  (testing "upgrade-finished? treats an unreadable (nil) status as not done, so the watch keeps waiting"
    (is (not (ceph/upgrade-finished? nil)) "nil means unreadable, not finished")
    (is (not (ceph/upgrade-finished? {:in_progress true})) "still in progress")
    (is (ceph/upgrade-finished? {:in_progress false}) "present and no longer in progress is done")))

(deftest capacity-preflight-helpers-test
  (testing "growth-bytes sums the logical (target - current) of the plan"
    (is (= (* 20 1024 1024 1024) (tools-ceph/growth-bytes [{:current "10Gi" :target "30Gi"}])))
    (is (= (* 20 1024 1024 1024) (tools-ceph/growth-bytes [{:current "10Gi" :target "20Gi"}
                                                           {:current "5Gi" :target "15Gi"}])))
    (testing "an unmeasurable capacity makes the whole plan not-our-call (nil)"
      (is (nil? (tools-ceph/growth-bytes [{:current "10Gi" :target "500m"}])))))
  (testing "min-pool-max-avail uses the replication-adjusted pool max_avail, not the raw total"
    (is (= 50 (tools-ceph/min-pool-max-avail {:stats {:total_avail_bytes 9999}
                                              :pools [{:stats {:max_avail 100}}
                                                      {:stats {:max_avail 50}}]})))
    (testing "no readable pool capacity is nil (the preflight then fails closed)"
      (is (nil? (tools-ceph/min-pool-max-avail {:stats {:total_avail_bytes 9999} :pools []})))
      (is (nil? (tools-ceph/min-pool-max-avail {:stats {:total_avail_bytes 9999}}))))))

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

(defn- answering
  [result]
  (fn [& _] result))

(deftest unarchived-crashes-tell-an-empty-answer-from-none
  (testing "an answered list is the crashes"
    (with-redefs [shell/execute (answering {:exit 0 :out "[{\"crash_id\":\"c1\",\"entity\":\"osd.3\"}]" :err ""})]
      (is (= {:crashes [{:crash_id "c1" :entity "osd.3"}]} (ceph/unarchived-crashes {:host "h"})))))
  (testing "an answered empty list is no crash"
    (with-redefs [shell/execute (answering {:exit 0 :out "[]" :err ""})]
      (is (= {:crashes []} (ceph/unarchived-crashes {:host "h"})))))
  (testing "a command that failed is unread, with what it said"
    (with-redefs [shell/execute (answering {:exit 1 :out "" :err "Error initializing cluster client\n"})]
      (is (= {:unread "ceph crash ls-new failed: Error initializing cluster client"}
             (ceph/unarchived-crashes {:host "h"})))))
  (testing "a failure that said nothing names its exit code"
    (with-redefs [shell/execute (answering {:exit 255 :out "" :err ""})]
      (is (= {:unread "ceph crash ls-new failed: exit 255"} (ceph/unarchived-crashes {:host "h"})))))
  (testing "a clean exit without output is no answer"
    (with-redefs [shell/execute (answering {:exit 0 :out "" :err ""})]
      (is (= {:unread "ceph crash ls-new answered nothing a crash list parses from"}
             (ceph/unarchived-crashes {:host "h"})))))
  (testing "output that does not parse is no answer"
    (with-redefs [shell/execute (answering {:exit 0 :out "{not json" :err ""})]
      (is (contains? (ceph/unarchived-crashes {:host "h"}) :unread))))
  (testing "a document that is not a list is no answer"
    (with-redefs [shell/execute (answering {:exit 0 :out "{\"crash_id\":\"c1\"}" :err ""})]
      (is (contains? (ceph/unarchived-crashes {:host "h"}) :unread))))
  (testing "the read runs ceph crash ls-new as JSON in the connection's mode"
    (let [seen (atom nil)]
      (with-redefs [shell/execute (fn [& argv] (reset! seen (vec argv)) {:exit 0 :out "[]" :err ""})]
        (binding [ssh/*user-config* nil]
          (ceph/unarchived-crashes {:host "h" :cephadm-shell? true})))
      (is (= ["sudo" "cephadm" "shell" "--" "ceph" "crash" "ls-new" "--format" "json"]
             (subvec @seen 12))))))

(deftest archiving-names-each-confirmed-crash
  (is (= [[:ssh {:host "h"} "ceph" "crash" "archive" "c1"]
          [:ssh {:host "h"} "ceph" "crash" "archive" "c2"]]
         (ceph/archive-effects {:host "h"} [{:crash_id "c1"} {:crash_id "c2"}]))))

(deftest confirmed-crashes-are-archived-once-none-of-them-is-listed-unarchived
  (is (true? (ceph/archived? [{:crash_id "c1"}] {:crashes []})))
  (is (true? (ceph/archived? [{:crash_id "c1"}] {:crashes [{:crash_id "c9"}]}))
      "a crash reported later is not one the operator confirmed")
  (is (false? (ceph/archived? [{:crash_id "c1"}] {:crashes [{:crash_id "c1"}]})))
  (is (false? (ceph/archived? [{:crash_id "c1"}] {:unread "no answer"}))
      "a read that did not answer proves nothing"))

(deftest all-on-version-compares-the-whole-version-number-test
  (let [on (fn [version] {:overall {(keyword (str "ceph version " version " (0123456789abcdef) quincy (stable)")) 52}})]
    (is (= "17.2.10" (ceph/version-number "ceph version 17.2.10 (abc) quincy (stable)")))
    (is (nil? (ceph/version-number "not a ceph version")))
    (is (ceph/all-on-version? (on "17.2.10") "17.2.10"))
    (is (not (ceph/all-on-version? (on "17.2.10") "17.2.1")) "a prefix of the running version")
    (is (not (ceph/all-on-version? (on "17.2.9") "17.2")) "a short target")
    (is (not (ceph/all-on-version? (on "17.2.9") "18")) "a major alone")
    (is (not (ceph/all-on-version? (on "17.2.9") "0123")) "a piece of the commit hash")
    (is (not (ceph/all-on-version? {:overall {}} "17.2.9")) "nothing running")
    (is (not (ceph/all-on-version? nil "17.2.9")) "versions not read")))

(defn- answering
  "A shell/execute stub answering every ssh command with result."
  [result]
  (fn [& _] (merge {:exit 0 :out "" :err ""} result)))

(deftest health-result-test
  (with-redefs [shell/execute (answering {:out "{\"health\":{\"status\":\"HEALTH_WARN\"}}"})]
    (is (= {:ok? true :status "HEALTH_WARN"} (ceph/health-result {:host "ceph-1"}))))
  (testing "a command that did not answer is not ok?"
    (with-redefs [shell/execute (answering {:exit 255 :out ""})]
      (is (not (:ok? (ceph/health-result {:host "ceph-1"})))))
    (with-redefs [shell/execute (answering {:out "mgr restarting"})]
      (is (not (:ok? (ceph/health-result {:host "ceph-1"})))))
    (with-redefs [shell/execute (answering {:out "{\"fsid\":\"x\"}"})]
      (is (not (:ok? (ceph/health-result {:host "ceph-1"}))) "a status without health"))))

(deftest healthy-enough-test
  (is (ceph/healthy-enough? {:ok? true :status "HEALTH_OK"}))
  (is (ceph/healthy-enough? {:ok? true :status "HEALTH_WARN"}))
  (is (not (ceph/healthy-enough? {:ok? true :status "HEALTH_ERR"})))
  (is (not (ceph/healthy-enough? {:ok? false :status nil})) "an unread status is not healthy")
  (is (not (ceph/healthy-enough? {:ok? true :status "HEALTH_UNKNOWN"}))))

(deftest upgrade-refusal-test
  (is (nil? (ceph/upgrade-refusal {:ok? true :status "HEALTH_OK"})))
  (is (nil? (ceph/upgrade-refusal {:ok? true :status "HEALTH_WARN"})))
  (is (re-find #"HEALTH_ERR" (ceph/upgrade-refusal {:ok? true :status "HEALTH_ERR"})))
  (is (re-find #"could not read" (ceph/upgrade-refusal {:ok? false :status nil}))))

(defn- with-out-str-and-err
  "The value of f, with what it prints to stdout and stderr swallowed."
  [f]
  (let [value (atom nil)]
    (binding [*err* (java.io.StringWriter.)]
      (with-out-str (reset! value (f))))
    @value))

(def ^:private upgrade-book
  (binding [runbook/*loading* true]
    (load-file "tools/ceph/playbooks/upgrade.clj")))

(defn- run-upgrade
  "Runs the upgrade playbook's action with the health read (`ceph status
   --format json`) answered by status-result and every other ssh command, the
   backups among them, answered with content. Every confirmation is declined
   and counted. Returns the action's result, the commands it streamed and how
   often it asked. Nothing is written: the backup directory and files are
   stubbed."
  [status-result]
  (let [streamed (atom [])
        asked (atom 0)
        health-read? (fn [args] (and (some #{"status"} args) (some #{"json"} args)))]
    (with-redefs [ssh/check-connection? (constantly true)
                  shell/execute (fn [& args]
                                  (if (health-read? args)
                                    ((answering status-result))
                                    ((answering {:out "dump"}))))
                  shell/run-with-output (fn [& args] (swap! streamed conj (vec args)) 0)
                  gumshoe.interact/confirm! (fn [& _] (swap! asked inc) false)
                  babashka.fs/create-dirs (fn [& _] nil)
                  spit (fn [& _] nil)]
      (let [result (with-out-str-and-err
                     #((:action upgrade-book) {:host "ceph-1.example.org" :target-version "17.2.9"} {}))]
        {:result result :streamed @streamed :asked @asked}))))

(deftest upgrade-refuses-a-cluster-whose-health-was-not-read-test
  (doseq [[label result] [["ssh failed" {:exit 255 :out ""}]
                          ["mgr restarting" {:out "not json"}]
                          ["HEALTH_ERR" {:out "{\"health\":{\"status\":\"HEALTH_ERR\"}}"}]]]
    (let [{:keys [result streamed asked]} (run-upgrade result)]
      (is (false? result) label)
      (is (zero? asked) label)
      (is (not-any? #(some #{"upgrade"} %) streamed) label)))
  (testing "a readable HEALTH_OK reaches the confirmation, so the refusals are the health read's"
    (is (= 1 (:asked (run-upgrade {:out "{\"health\":{\"status\":\"HEALTH_OK\"}}"}))))))

(deftest change-refusal-test
  (is (nil? (ceph/change-refusal {:ok? true :status "HEALTH_OK"} false)))
  (is (re-find #"HEALTH_WARN.*--force" (ceph/change-refusal {:ok? true :status "HEALTH_WARN"} false)))
  (is (nil? (ceph/change-refusal {:ok? true :status "HEALTH_ERR"} true)) "--force accepts a read degraded status")
  (testing "an unread status refuses, even under --force"
    (is (re-find #"could not read" (ceph/change-refusal {:ok? false :status nil} false)))
    (is (re-find #"could not read" (ceph/change-refusal {:ok? false :status nil} true)))))

(defn- run-single-change
  "Runs the :action of the ceph book at path with the health read answered by
   status-result and every other ssh command by `{}` JSON naming one daemon
   and two in-OSDs. Every confirmation is declined and counted."
  [path opts status-result]
  (let [book (binding [runbook/*loading* true] (load-file path))
        asked (atom 0)
        health-read? (fn [args] (and (some #{"status"} args) (some #{"json"} args)))
        other {:out (str "{\"osds\":[{\"osd\":1,\"in\":1,\"up\":1},{\"osd\":2,\"in\":1,\"up\":1}]}")}]
    (with-redefs [ssh/check-connection? (constantly true)
                  ceph/daemons (constantly [{:daemon_name "osd.1" :status_desc "running"}])
                  ceph/daemon-list (constantly {:daemons [{:daemon_name "osd.1" :status_desc "running"}]})
                  shell/execute (fn [& args]
                                  (if (health-read? args) ((answering status-result)) ((answering other))))
                  shell/run-with-output (fn [& _] 0)
                  gumshoe.interact/choose-one (fn [_ _ provided] provided)
                  gumshoe.interact/confirm! (fn [& _] (swap! asked inc) false)]
      {:result (with-out-str-and-err #((:action book) opts {}))
       :asked @asked})))

(deftest single-changes-refuse-an-unread-health-status-test
  (doseq [[path opts] [["tools/ceph/runbooks/restart_daemon.clj" {:host "ceph-1" :daemon "osd.1"}]
                       ["tools/ceph/runbooks/osd_out.clj" {:host "ceph-1" :osd "osd.1"}]]
          forced [false true]
          [label result] [["ssh failed" {:exit 255 :out ""}]
                          ["mgr restarting" {:out "not json"}]
                          ["no health in the document" {:out "{\"fsid\":\"x\"}"}]]]
    (let [{:keys [result asked]} (run-single-change path (assoc opts :force forced) result)]
      (is (false? result) (str path " " label " forced " forced))
      (is (zero? asked) (str path " " label " forced " forced))))
  (testing "a read HEALTH_OK, and a read HEALTH_WARN under --force, reach the confirmation"
    (doseq [[path opts] [["tools/ceph/runbooks/restart_daemon.clj" {:host "ceph-1" :daemon "osd.1"}]
                         ["tools/ceph/runbooks/osd_out.clj" {:host "ceph-1" :osd "osd.1"}]]]
      (is (= 1 (:asked (run-single-change path opts {:out "{\"health\":{\"status\":\"HEALTH_OK\"}}"}))) path)
      (is (= 1 (:asked (run-single-change path (assoc opts :force true) {:out "{\"health\":{\"status\":\"HEALTH_WARN\"}}"}))) path)
      (is (zero? (:asked (run-single-change path opts {:out "{\"health\":{\"status\":\"HEALTH_WARN\"}}"}))) path))))
