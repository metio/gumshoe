;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.postgres-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
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

;; What pg_dump writes at either end of a plain dump.
(def ^:private dump-head "--\n-- PostgreSQL database dump\n--\n\nSET statement_timeout = 0;\n")
(def ^:private dump-tail "\n--\n-- PostgreSQL database dump complete\n--\n\n")

(deftest dump-complete-test
  (fs/with-temp-dir [workdir {}]
    (let [complete (str (fs/path workdir "complete.sql"))
          truncated (str (fs/path workdir "truncated.sql"))
          headless (str (fs/path workdir "headless.sql"))
          empty-file (str (fs/path workdir "empty.sql"))]
      (spit complete (str dump-head "CREATE TABLE t (id int);" dump-tail))
      (spit truncated (str dump-head "CREATE TABLE half ("))
      (spit headless (str "CREATE TABLE t (id int);" dump-tail))
      (spit empty-file "")
      (testing "a dump counts as complete only with pg_dump's header and marker"
        (is (postgres/dump-complete? complete))
        (is (not (postgres/dump-complete? truncated)))
        (is (not (postgres/dump-complete? empty-file)))
        (is (not (postgres/dump-complete? (str (fs/path workdir "missing.sql")))))
        (is (not (postgres/dump-complete? workdir)) "a directory is no dump")))))

(deftest dump-ends-reads-only-the-ends-test
  (fs/with-temp-dir [workdir {}]
    (let [file (str (fs/path workdir "dump.sql"))
          middle (apply str (repeat 1000 "INSERT INTO t VALUES (1);\n"))]
      (spit file (str dump-head middle "-- PostgreSQL database dump complete\n"))
      (let [{:keys [head tail]} (postgres/dump-ends file 64)]
        (is (= 64 (count head)) "the head is the window, not the file")
        (is (= 64 (count tail)) "the tail is the window, not the file")
        (is (str/starts-with? head "--\n-- PostgreSQL database dump\n"))
        (is (str/ends-with? tail "-- PostgreSQL database dump complete\n"))))))

(deftest dump-ends-complete-is-a-judgement-over-both-ends-test
  (is (postgres/dump-ends-complete? {:head dump-head :tail dump-tail}))
  (is (not (postgres/dump-ends-complete? {:head dump-head :tail "COPY t (id) FROM stdin;\n1\n"}))
      "a tail without the marker is truncated")
  (is (not (postgres/dump-ends-complete? {:head "SELECT 1;" :tail dump-tail}))
      "a head without pg_dump's header is no plain dump")
  (is (not (postgres/dump-ends-complete? {:head nil :tail nil}))))

(deftest a-dump-above-2-gib-is-judged-test
  ;; A sparse file: its length is above what one JVM String can hold, while
  ;; the disk holds only the two ends written here.
  (fs/with-temp-dir [workdir {}]
    (let [file (str (fs/path workdir "big.sql"))
          length (+ (* 2 1024 1024 1024) (* 64 1024 1024))
          tail (.getBytes dump-tail "UTF-8")]
      (with-open [raf (java.io.RandomAccessFile. file "rw")]
        (.write raf (.getBytes dump-head "UTF-8"))
        (.setLength raf length)
        (.seek raf (- length (count tail)))
        (.write raf tail))
      (is (postgres/dump-complete? file) "a complete dump above 2 GiB is restorable")
      (with-open [raf (java.io.RandomAccessFile. file "rw")]
        (.setLength raf (- length (count tail))))
      (is (not (postgres/dump-complete? file)) "a truncated dump above 2 GiB is refused"))))

(deftest dump-complete-counts-an-error-as-incomplete-test
  (with-redefs [postgres/dump-ends (fn [_] (throw (AssertionError. "an Error, as slurping a huge file throws one")))]
    (is (false? (postgres/dump-complete? "any.sql")) "an Error is judged, not thrown")))

(deftest a-dump-runs-without-the-probe-timeout
  (let [bound (atom :unset)]
    (with-redefs [gumshoe.shell/execute-env (fn [& _] (reset! bound gumshoe.shell/*timeout-ms*) {:exit 0})
                  gumshoe.stdout/print-command (fn [& _])]
      (is (true? (postgres/pg-dump! {:user "u" :database "d" :password "p"} 5432 "/dev/null")))
      (is (nil? @bound) "a dump may take as long as the database needs"))))
