;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.postgres-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.postgres :as postgres]))

(deftest credentials-name-test
  (testing "deduplicates the db-db overlap like db-operator does"
    (is (= "moodle-db-credentials" (postgres/credentials-name "moodle-db")))
    (is (= "keycloak-db-credentials" (postgres/credentials-name "keycloak")))))

(deftest split-host-test
  (is (= {:service "postgres" :namespace "databases"}
         (postgres/split-host "postgres.databases.svc.cluster.local")))
  (is (= {:service "postgres" :namespace "databases"}
         (postgres/split-host "postgres.databases"))))

(deftest decode-test
  (is (= "secret" (postgres/decode "c2VjcmV0")))
  (is (nil? (postgres/decode nil))))

(deftest connection-complete-test
  (is (postgres/connection-complete? {:service "s" :namespace "n" :user "u" :password "p"}))
  (is (not (postgres/connection-complete? {:service "s" :namespace "n" :user "u" :password ""})))
  (is (not (postgres/connection-complete? {:service nil :namespace "n" :user "u" :password "p"}))))

(deftest dump-complete-test
  (fs/with-temp-dir [workdir {}]
    (let [complete (str (fs/path workdir "complete.sql"))
          truncated (str (fs/path workdir "truncated.sql"))]
      (spit complete "SET x;\n-- PostgreSQL database dump complete\n")
      (spit truncated "SET x;\nCREATE TABLE half (")
      (testing "a dump counts as complete only with pg_dump's marker"
        (is (postgres/dump-complete? complete))
        (is (not (postgres/dump-complete? truncated)))
        (is (not (postgres/dump-complete? (str (fs/path workdir "missing.sql")))))))))
