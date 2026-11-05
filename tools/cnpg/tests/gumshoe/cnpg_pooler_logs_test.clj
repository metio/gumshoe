;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-pooler-logs-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gumshoe.cnpg.pooler-logs :as pooler-logs]
            [gumshoe.detectives.cnpg-pooler-logs :as pooler-logs-detective]))

(defn- stamped [at line] (format "{\"ts\":\"%s\",\"msg\":\"%s\"}" at line))

(def ^:private health-check
  "C-0x1: pgbouncer/pgbouncer@unix(1):5432 closing because: client close request (age=0s)")

(def ^:private server-login-failure
  "WARNING: server login failed: FATAL password authentication failed for user \"app\"")

(def ^:private client-login-failure
  "C-0x3: db/app@10.24.160.9:51468 closing because: password authentication failed for user \"app\" (age=4209s)")

(def ^:private connect-failure
  "C-0x2: postgres/app@10.24.160.213:38018 closing because: server login has been failing, cached error: connect failed (server_login_retry) (age=0s)")

(defn- evidence [& rows] {pooler-logs/evidence-key (vec rows)})

(defn- row [pod log] {:namespace "db" :pod pod :read? true :log log})

(deftest a-quiet-log-yields-no-finding-test
  (is (empty? (pooler-logs-detective/detect-rejected-logins
               (evidence (row "db-rw-transaction-1" (str/join "\n" [health-check health-check])))))))

(deftest a-current-server-leg-rejection-is-critical-and-names-the-leg-test
  (let [findings (pooler-logs-detective/detect-rejected-logins
                  (evidence (row "db-rw-transaction-1"
                                 (str/join "\n" [(stamped "2026-11-04T20:59:00Z" health-check)
                                                 (stamped "2026-11-04T20:59:30Z" server-login-failure)]))))
        finding (first findings)]
    (is (= 1 (count findings)))
    (is (= :critical (:severity finding)))
    (is (= "db/db-rw-transaction-1" (:component finding)))
    (is (re-find #"on the server leg" (:summary finding)))))

(deftest a-client-leg-rejection-names-the-client-leg-test
  (let [finding (first (pooler-logs-detective/detect-rejected-logins
                        (evidence (row "db-rw-transaction-1"
                                       (str/join "\n" [(stamped "2026-11-04T20:59:00Z" health-check)
                                                       (stamped "2026-11-04T20:59:30Z" client-login-failure)])))))]
    (is (= :critical (:severity finding)))
    (is (re-find #"on the client leg" (:summary finding)))))

(deftest a-rejection-left-behind-in-the-window-is-a-warning-test
  (let [finding (first (pooler-logs-detective/detect-rejected-logins
                        (evidence (row "db-rw-transaction-1"
                                       (str/join "\n" [(stamped "2026-10-30T19:56:03Z" server-login-failure)
                                                       (stamped "2026-11-04T20:59:30Z" health-check)])))))]
    (is (= :warning (:severity finding)))
    (is (re-find #"none of them current" (:summary finding)))))

(deftest an-unreachable-backend-alone-is-left-to-the-cluster-detectives-test
  (is (empty? (pooler-logs-detective/detect-rejected-logins
               (evidence (row "db-rw-transaction-1"
                              (str/join "\n" [(stamped "2026-11-04T20:59:30Z" connect-failure)])))))))

(deftest a-rejection-beside-an-unreachable-backend-is-still-reported-test
  (let [findings (pooler-logs-detective/detect-rejected-logins
                  (evidence (row "db-rw-transaction-1"
                                 (str/join "\n" [(stamped "2026-11-04T20:59:00Z" connect-failure)
                                                 (stamped "2026-11-04T20:59:30Z" server-login-failure)]))))]
    (is (= 1 (count findings)) "the credential half of a :both verdict is this detective's own")))

(deftest an-unread-log-is-reported-as-unchecked-test
  (let [finding (first (pooler-logs-detective/detect-rejected-logins
                        (evidence {:namespace "db" :pod "db-rw-transaction-1"
                                   :read? false :log ""})))]
    (is (= :warning (:severity finding)))
    (is (re-find #"unread" (:summary finding)))
    (is (re-find #"unchecked rather than healthy" (:hint finding)))))

(deftest every-pooler-pod-is-judged-on-its-own-log-test
  (let [findings (pooler-logs-detective/detect-rejected-logins
                  (evidence (row "healthy" (str/join "\n" [health-check]))
                            (row "broken" (stamped "2026-11-04T20:59:30Z" server-login-failure))))]
    (is (= ["db/broken"] (map :component findings)))))

(deftest the-detective-asks-for-the-pooler-logs-test
  (let [detective (first pooler-logs-detective/detectives)]
    (is (= "cnpg-pooler-logins" (:name detective)))
    (is (= [pooler-logs/evidence-key] (:requires detective)))))
