;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.db-operator-status-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.detectives.db-operator :as db-operator]))

(defn- summaries [status]
  (map :summary (db-operator/detect-database-problems
                 {db-operator/database-type {:items [(cond-> {:metadata {:namespace "a" :name "db"}}
                                                       status (assoc :status status))]}})))

(deftest a-not-ready-database-names-what-its-status-carries
  (is (= ["database is not ready (engine postgres, database a-db, user a-db, operator 2.1.0)"]
         (summaries {:status false :engine "postgres" :database "a-db" :user "a-db" :operatorVersion "2.1.0"})))
  (is (= ["database is not ready (engine mysql)"] (summaries {:status false :engine "mysql" :database ""}))))

(deftest no-phase-is-ever-invented
  (is (not-any? #(re-find #"phase" %) (summaries {:status false}))))

(deftest a-database-without-status-has-not-been-reconciled
  (is (= ["database has no status - db-operator has not reconciled it"] (summaries nil))))

(deftest a-ready-database-is-silent
  (is (empty? (summaries {:status true :engine "postgres"}))))
