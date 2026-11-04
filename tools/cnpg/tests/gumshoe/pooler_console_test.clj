;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pooler-console-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gumshoe.pooler-console :as console]))

(def ^:private health-check
  "C-0x1: pgbouncer/pgbouncer@unix(1):5432 closing because: client close request (age=0s)")

(def ^:private missing-metric
  "{\"msg\":\"Missing metric\",\"query\":\"SHOW LISTS\",\"metric\":\"peers\"}")

(def ^:private auth-failure
  "WARNING: server login failed: FATAL password authentication failed for user \"app\"")

(def ^:private connect-failure
  "C-0x2: postgres/app@10.24.160.213:38018 closing because: server login has been failing, cached error: connect failed (server_login_retry) (age=0s)")

(deftest health-checks-and-absent-metrics-are-noise-test
  (is (console/noise? health-check))
  (is (console/noise? missing-metric))
  (is (not (console/noise? auth-failure)))
  (is (not (console/noise? connect-failure))))

(deftest a-log-of-only-health-checks-reads-quiet-test
  (let [verdict (console/classify (str/join "\n" [health-check missing-metric health-check]))]
    (is (= :quiet (:verdict verdict)))
    (is (zero? (:lines verdict)) "noise is not counted as log content")))

(deftest an-authentication-failure-is-named-test
  (let [verdict (console/classify (str/join "\n" [health-check auth-failure]))]
    (is (= :authentication (:verdict verdict)))
    (is (= 1 (:authentication verdict)))
    (is (zero? (:unreachable verdict)))))

(deftest an-unreachable-backend-is-named-test
  (let [verdict (console/classify (str/join "\n" [connect-failure connect-failure]))]
    (is (= :unreachable (:verdict verdict)))
    (is (= 2 (:unreachable verdict)))
    (is (zero? (:authentication verdict)))))

(deftest both-signatures-together-defer-to-the-backend-test
  (let [verdict (console/classify (str/join "\n" [auth-failure connect-failure]))]
    (is (= :both (:verdict verdict)))
    (is (re-find #"unreachable backend first"
                 (first (console/guidance verdict {:metadata {:namespace "db" :name "db-rw"}}))))))

(deftest the-authentication-verdict-prints-the-rollout-command-test
  (is (re-find #"rollout restart deployment/db-rw"
               (first (console/guidance {:verdict :authentication}
                                        {:metadata {:namespace "db" :name "db-rw"}})))))

(deftest the-unreachable-verdict-does-not-suggest-rolling-test
  (let [advice (first (console/guidance {:verdict :unreachable}
                                        {:metadata {:namespace "db" :name "db-rw"}}))]
    (is (re-find #"not a pooler fault" advice))
    (is (not (re-find #"rollout restart" advice)))))

(deftest a-quiet-verdict-does-not-claim-health-test
  (is (re-find #"not evidence"
               (first (console/guidance {:verdict :quiet}
                                        {:metadata {:namespace "db" :name "db-rw"}})))))
