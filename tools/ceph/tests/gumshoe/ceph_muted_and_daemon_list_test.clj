;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ceph-muted-and-daemon-list-test
  "A muted health check is acknowledged and left out of ceph's own health
   status, and a daemon list the orchestrator did not answer is a failed
   read, never an empty selection."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.ceph :as ceph]
            [gumshoe.detectives.ceph :as detectives]
            [gumshoe.interact :as interact]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]))

(defn- status
  [checks]
  {"status" {:health {:status "HEALTH_OK" :checks checks}}})

(deftest a-muted-check-is-reported-at-info
  (let [[finding :as all] (detectives/detect-health-checks
                           (status {:OSD_DOWN {:severity "HEALTH_ERR" :summary {:message "1 osds down"} :muted true}}))]
    (is (= 1 (count all)))
    (is (= :info (:severity finding)))
    (is (= "OSD_DOWN" (:component finding)))
    (is (= "muted: 1 osds down" (:summary finding)))
    (is (str/includes? (:hint finding) "ceph health unmute OSD_DOWN"))))

(deftest unmuted-checks-keep-their-severity
  (let [findings (detectives/detect-health-checks
                  (status {:OSD_DOWN {:severity "HEALTH_ERR" :summary {:message "1 osds down"} :muted false}
                           :POOL_FULL {:severity "HEALTH_WARN" :summary {:message "1 pool full"}}}))]
    (is (= {"OSD_DOWN" :critical "POOL_FULL" :warning}
           (into {} (map (juxt :component :severity)) findings)))))

(defn- answering
  [result]
  (fn [& _] (merge {:exit 0 :out "" :err ""} result)))

(deftest the-daemon-list-tells-an-unanswered-command-from-an-empty-list
  (with-redefs [shell/execute (answering {:out "[{\"daemon_name\":\"osd.3\"}]"})]
    (is (= {:daemons [{:daemon_name "osd.3"}]} (ceph/daemon-list {:host "h"}))))
  (with-redefs [shell/execute (answering {:out "[]"})]
    (is (= {:daemons []} (ceph/daemon-list {:host "h"}))))
  (with-redefs [shell/execute (answering {:exit 2 :err "Error ENOENT: No orchestrator configured"})]
    (is (= {:unread "ceph orch ps failed: Error ENOENT: No orchestrator configured"} (ceph/daemon-list {:host "h"}))))
  (with-redefs [shell/execute (answering {:out "not json"})]
    (is (= {:unread "ceph orch ps answered nothing a daemon list parses from"} (ceph/daemon-list {:host "h"}))))
  (with-redefs [shell/execute (answering {:out "{\"x\":1}"})]
    (is (= {:unread "ceph orch ps answered a document that is no list of daemons"} (ceph/daemon-list {:host "h"})))))

(deftest the-crash-read-keeps-its-reasons
  (with-redefs [shell/execute (answering {:out "{\"x\":1}"})]
    (is (= {:unread "ceph crash ls-new answered a document that is no list of crash reports"}
           (ceph/unarchived-crashes {:host "h"})))))

(defn- restart
  "Runs restart_daemon with --daemon osd.3 against an ssh that answers the
   health read with HEALTH_OK and `ceph orch ps` with orch-ps. Returns the
   result, what it printed and whether the picker or a confirmation ran."
  [orch-ps]
  (let [book (binding [runbook/*loading* true] (load-file "tools/ceph/runbooks/restart_daemon.clj"))
        picked (atom 0)
        asked (atom 0)
        err (java.io.StringWriter.)
        out (java.io.StringWriter.)]
    (with-redefs [ssh/check-connection? (constantly true)
                  shell/execute (fn [& argv]
                                  (cond
                                    (some #{"orch"} argv) (merge {:exit 0 :out "" :err ""} orch-ps)
                                    (some #{"status"} argv) {:exit 0 :out "{\"health\":{\"status\":\"HEALTH_OK\"}}" :err ""}
                                    :else {:exit 1 :out "" :err "unexpected"}))
                  shell/run-with-output (fn [& _] 0)
                  interact/choose-one (fn [_ candidates provided] (swap! picked inc) (some #{provided} candidates))
                  interact/confirm! (fn [& _] (swap! asked inc) false)]
      (let [result (binding [*out* out *err* err] ((:action book) {:host "ceph-1" :daemon "osd.3"} {}))]
        {:result result :printed (str out err) :picked @picked :asked @asked}))))

(deftest restart-names-a-failed-daemon-list
  (let [{:keys [result printed picked asked]} (restart {:exit 2 :err "Error EINVAL: mgr failing over"})]
    (is (false? result))
    (is (str/includes? printed "could not list the daemons - ceph orch ps failed: Error EINVAL: mgr failing over") printed)
    (is (not (str/includes? printed "no daemon selected")) printed)
    (is (zero? picked))
    (is (zero? asked))))

(deftest restart-with-an-answered-list-reaches-the-confirmation
  (testing "the daemon is listed"
    (is (= 1 (:asked (restart {:out "[{\"daemon_name\":\"osd.3\",\"status_desc\":\"running\"}]"})))))
  (testing "an answered list without the daemon is no daemon selected"
    (let [{:keys [result printed asked]} (restart {:out "[{\"daemon_name\":\"osd.1\"}]"})]
      (is (false? result))
      (is (str/includes? printed "no daemon selected") printed)
      (is (zero? asked)))))
