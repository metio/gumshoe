;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.postgres-restore-transaction-test
  "The db-operator restore replays a dump in one transaction, so a failure
   rolls it back; a dump holding a statement that cannot run in a
   transaction is refused before anything connects, and a failed replay
   says that nothing was changed."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.outcome :as outcome]
            [gumshoe.postgres :as postgres]
            [gumshoe.postgres-script :as script]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]))

(def ^:private book "tools/db-operator/runbooks/restore.clj")

(defn- load-book!
  []
  (binding [runbook/*loading* true] (load-file book)))

(defn- found [lines] (mapv :statement (script/transaction-breakers lines)))

(deftest the-restore-runs-in-one-transaction-and-stops-at-the-first-error
  (let [args (postgres/restore-args "in.sql")]
    (is (some #{"--single-transaction"} args))
    (is (= "ON_ERROR_STOP=on" (second (drop-while #(not= "--set" %) args))))
    (is (some #{"--no-psqlrc"} args) "a psqlrc could turn autocommit or ON_ERROR_STOP back")
    (is (= "in.sql" (last args)))))

(deftest a-plain-pg-dump-of-one-database-runs-in-one-transaction
  (is (= [] (found ["--"
                    "-- PostgreSQL database dump"
                    "--"
                    "SET statement_timeout = 0;"
                    "SELECT pg_catalog.set_config('search_path', '', false);"
                    "DROP TABLE IF EXISTS public.t;"
                    "CREATE TABLE public.t (id integer, body text);"
                    "COPY public.t (id, body) FROM stdin;"
                    "1\tVACUUM"
                    "2\tCREATE DATABASE x;"
                    "3\tCOMMIT;"
                    "\\."
                    "CREATE INDEX t_idx ON public.t USING btree (id);"
                    "-- PostgreSQL database dump complete"]))
      "COPY data and comments are never read as statements"))

(deftest statements-that-cannot-run-in-a-transaction-are-named-with-their-line
  (is (= [{:line 2 :statement "CREATE DATABASE"} {:line 3 :statement "\\connect"}]
         (script/transaction-breakers ["-- dumped with --create" "CREATE DATABASE app WITH TEMPLATE = template0;" "\\connect app"])))
  (doseq [[sql expected] [["VACUUM ANALYZE t;" "VACUUM"]
                          ["vacuum;" "VACUUM"]
                          ["CREATE INDEX CONCURRENTLY i ON t (a);" "CREATE INDEX CONCURRENTLY"]
                          ["create unique index concurrently i on t (a);" "CREATE INDEX CONCURRENTLY"]
                          ["DROP INDEX CONCURRENTLY i;" "DROP INDEX CONCURRENTLY"]
                          ["REINDEX TABLE CONCURRENTLY t;" "REINDEX CONCURRENTLY"]
                          ["REINDEX DATABASE app;" "REINDEX DATABASE/SYSTEM"]
                          ["DROP DATABASE app;" "DROP DATABASE"]
                          ["CREATE TABLESPACE fast LOCATION '/x';" "CREATE TABLESPACE"]
                          ["ALTER SYSTEM SET work_mem = '1MB';" "ALTER SYSTEM"]
                          ["BEGIN;" "BEGIN"]
                          ["START TRANSACTION;" "BEGIN"]
                          ["COMMIT;" "COMMIT"]
                          ["END;" "COMMIT"]
                          ["ROLLBACK;" "ROLLBACK"]
                          ["COMMIT PREPARED 'x';" "COMMIT/ROLLBACK PREPARED"]
                          ["PREPARE TRANSACTION 'x';" "PREPARE TRANSACTION"]
                          ["CLUSTER;" "CLUSTER without a table"]]]
    (testing sql
      (is (= [expected] (found [sql]))))))

(deftest quoted-and-commented-text-is-not-a-statement
  (is (= [] (found ["SELECT 'a; VACUUM';"
                    "SELECT E'it\\'s; COMMIT';"
                    "SELECT 'it''s; COMMIT';"
                    "SELECT \"odd;\"\"name\" FROM t;"
                    "/* outer /* VACUUM; */ still a comment; COMMIT; */ SELECT 1;"
                    "SELECT 1; -- COMMIT;"]))))

(deftest a-function-body-is-not-split-at-its-semicolons
  (testing "a dollar-quoted plpgsql body with BEGIN, COMMIT and END"
    (is (= [] (found ["CREATE FUNCTION public.f() RETURNS void"
                      "    LANGUAGE plpgsql"
                      "    AS $_$"
                      "BEGIN"
                      "  COMMIT;"
                      "  VACUUM;"
                      "END;"
                      "$_$;"]))))
  (testing "a SQL-standard BEGIN ATOMIC body, whose semicolons are not quoted"
    (is (= ["VACUUM"] (found ["CREATE FUNCTION public.g() RETURNS integer"
                      "    LANGUAGE sql"
                      "    BEGIN ATOMIC"
                      "  SELECT CASE WHEN true THEN 1 ELSE 2 END;"
                      "  SELECT 2;"
                      " END;"
                      "VACUUM;"]))
        "only the VACUUM after the function would be named")
    (is (= ["VACUUM"] (found ["CREATE OR REPLACE PROCEDURE p() LANGUAGE sql BEGIN ATOMIC SELECT 1; END;" "VACUUM;"])))))

(deftest a-statement-left-unterminated-is-judged-too
  (is (= ["VACUUM"] (found ["SELECT 1;" "VACUUM"]))))

(deftest an-unreadable-dump-is-refused
  (let [{:keys [unread]} (script/file-transaction-breakers "/nonexistent/dump.sql")]
    (is (string? unread)))
  (load-book!)
  (let [refusal (requiring-resolve 'runbooks.db-operator.restore/transaction-refusal)]
    (is (str/includes? (refusal "d.sql" {:unread "no such file"}) "could not read d.sql"))
    (is (nil? (refusal "d.sql" [])))))

(deftest the-confirmation-and-the-failure-say-what-the-transaction-means
  (load-book!)
  (let [action @(requiring-resolve 'runbooks.db-operator.restore/confirmation-action)
        guidance ((requiring-resolve 'runbooks.db-operator.restore/failure-guidance) "app/db")]
    (is (str/includes? action "one transaction"))
    (is (str/includes? action "dropped and recreated"))
    (is (str/includes? action "rolls everything back"))
    (is (str/starts-with? guidance "Nothing was changed"))
    (is (str/includes? guidance "app/db"))))

(deftest a-failed-replay-records-that-nothing-was-changed
  (load-book!)
  (let [replay! (requiring-resolve 'runbooks.db-operator.restore/replay!)
        argv (atom nil)
        record (outcome/fresh)]
    (with-redefs [shell/run-with-output-env (fn [_ & args] (reset! argv (vec args)) 3)]
      (binding [*err* (java.io.StringWriter.)
                outcome/*record* record]
        (is (false? (replay! {:user "app" :database "app" :password "pw"} 15432 "in.sql" "app/db")))))
    (is (some #{"--single-transaction"} @argv) "psql was asked for one transaction")
    (is (str/starts-with? (get-in @record [:recovery :text]) "Nothing was changed"))))

(deftest a-dump-that-cannot-run-in-one-transaction-is-refused-before-anything-connects
  (let [dir (fs/create-temp-dir)
        file (str (fs/path dir "create.sql"))
        reads (atom [])]
    (spit file (str "-- PostgreSQL database dump\n"
                    "CREATE DATABASE app;\n"
                    "\\connect app\n"
                    "CREATE TABLE t (a int);\n"
                    "-- PostgreSQL database dump complete\n"))
    (try
      (let [{:keys [action]} (load-book!)
            err (java.io.StringWriter.)
            result (with-redefs [kubectl/current-context (constantly "ctx")
                                 kubectl/current-cluster (constantly "cluster")
                                 shell/execute (fn [& argv] (swap! reads conj (vec argv)) {:exit 1 :out "" :err "stub"})]
                     (binding [*err* err] (action {:dump file} {:announcement-data nil})))]
        (is (false? result))
        (is (= [] @reads) "no Database was looked up and nothing connected")
        (is (str/includes? (str err) "line 2: CREATE DATABASE"))
        (is (str/includes? (str err) "line 3: \\connect")))
      (finally (fs/delete-tree dir)))))

(deftest a-large-dump-is-judged-in-one-pass
  (let [lines (concat ["CREATE TABLE t (a int, b text);" "COPY public.t (a, b) FROM stdin;"]
                      (map #(str % "\tVACUUM; COMMIT; 'unbalanced") (range 200000))
                      ["\\." "VACUUM;"])
        start (System/currentTimeMillis)]
    (is (= ["VACUUM"] (found lines)))
    (is (< (- (System/currentTimeMillis) start) 20000) "COPY data is skipped line by line")))
