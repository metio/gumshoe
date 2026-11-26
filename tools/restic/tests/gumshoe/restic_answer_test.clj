;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.restic-answer-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.restic :as detectives]
            [gumshoe.restic :as restic]))

(def ^:private now (java.time.Instant/parse "2026-07-04T00:00:00Z"))

(defn- findings
  [answer]
  (vec (detectives/detect-backup-problems
        {:now now :warn-days 2 :critical-days 7 "repositories" {"s3:backups" answer}})))

(deftest a-snapshot-list-is-read
  (is (= {:reachable true :snapshots [{:hostname "h" :paths ["/d"] :time "2026-07-03T00:00:00Z"}]}
         (restic/snapshots-answer {:exit 0 :out "[{\"hostname\":\"h\",\"paths\":[\"/d\"],\"time\":\"2026-07-03T00:00:00Z\"}]"}))))

(deftest an-empty-repository-is-empty
  (is (= {:reachable true :snapshots []} (restic/snapshots-answer {:exit 0 :out "[]\n"})))
  (testing "restic before 0.11.0 printed null for no snapshots"
    (is (= {:reachable true :snapshots []} (restic/snapshots-answer {:exit 0 :out "null\n"})))))

(deftest output-that-is-no-snapshot-list-is-unread
  (doseq [out ["repository abc opened (version 2)\n[{\"time\":\"2026-07-03T00:00:00Z\"}]"
               "[{\"time\":\"2026-07-03T00:00:00Z\"}, {\"ti"
               ""
               "{\"message\":\"not a list\"}"
               "[1,2]"]]
    (testing (pr-str out)
      (let [answer (restic/snapshots-answer {:exit 0 :out out})]
        (is (= {:reachable false :error restic/unreadable-output-error} answer))
        (is (= ["repository can not be read"] (mapv :summary (findings answer)))
            "never 'repository has no snapshots'")))))

(deftest a-failed-run-names-restic-s-last-line
  (is (= {:reachable false :error "Fatal: wrong password"}
         (restic/snapshots-answer {:exit 1 :out "" :err "opening\nFatal: wrong password\n"}))))
