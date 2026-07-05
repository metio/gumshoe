;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.backups-test
  (:require [clojure.test :refer [deftest is testing]]
            [infra.detectives.restic :as restic-detectives]
            [infra.restic :as restic]))

(defn- summaries
  [findings]
  (set (map :summary findings)))

(def now (java.time.Instant/parse "2026-07-04T00:00:00Z"))

(deftest snapshot-instant-test
  (is (= (java.time.Instant/parse "2026-07-03T02:00:00Z")
         (restic/snapshot-instant {:time "2026-07-03T02:00:00Z"})))
  (testing "restic's offset timestamps parse to the right instant"
    (is (= (java.time.Instant/parse "2026-07-03T00:00:00.123456Z")
           (restic/snapshot-instant {:time "2026-07-03T02:00:00.123456+02:00"}))))
  (is (nil? (restic/snapshot-instant {:time "garbage"}))))

(deftest latest-per-target-test
  (let [targets (restic/latest-per-target
                 [{:hostname "web-1" :paths ["/data"] :time "2026-07-01T02:00:00Z"}
                  {:hostname "web-1" :paths ["/data"] :time "2026-07-03T02:00:00Z"}
                  {:hostname "db-1" :paths ["/var/lib/pg"] :time "2026-06-01T02:00:00Z"}])]
    (testing "targets are grouped by host and paths, keeping the newest"
      (is (= 2 (count targets)))
      (is (= (java.time.Instant/parse "2026-07-03T02:00:00Z")
             (:latest (first (filter #(= "web-1" (:hostname %)) targets))))))))

(deftest repository-detective-test
  (testing "an unreadable repository is critical"
    (is (= #{"repository can not be read"}
           (summaries (restic-detectives/detect-backup-problems
                       {:now now :warn-days 2 :critical-days 7
                        "repositories" {"s3:backups" {:reachable false :error "wrong password"}}})))))
  (testing "an empty repository is critical"
    (is (= #{"repository has no snapshots"}
           (summaries (restic-detectives/detect-backup-problems
                       {:now now :warn-days 2 :critical-days 7
                        "repositories" {"s3:backups" {:reachable true :snapshots []}}})))))
  (testing "a fresh backup is silent, a slightly-late one warns, a very-late one is critical"
    (let [evidence {:now now :warn-days 2 :critical-days 7
                    "repositories"
                    {"s3:fresh" {:reachable true
                                 :snapshots [{:hostname "web" :paths ["/d"] :time "2026-07-03T12:00:00Z"}]}
                     "s3:late" {:reachable true
                                :snapshots [{:hostname "web" :paths ["/d"] :time "2026-07-01T00:00:00Z"}]}
                     "s3:stale" {:reachable true
                                 :snapshots [{:hostname "web" :paths ["/d"] :time "2026-06-20T00:00:00Z"}]}}}
          findings (restic-detectives/detect-backup-problems evidence)]
      (is (= #{"newest backup is 3 days old" "newest backup is 14 days old"}
             (summaries findings)))
      (is (= :warning (:severity (first (filter #(= "newest backup is 3 days old" (:summary %)) findings)))))
      (is (= :critical (:severity (first (filter #(= "newest backup is 14 days old" (:summary %)) findings))))))))
