;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dry-run-allow-list-test
  "The dry-run harness's allow-list and its view of announcements: every
   command here must be judged the way a review would, or a book that acts
   under --dry-run passes gumshoe.dry-run-books-test."
  (:require [babashka.http-client :as http-client]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.dry-run-books-test :as harness]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]))

(defn- read? [& argv] (harness/read-command? (vec argv)))

(deftest kubectl-auth-reads-only-with-its-read-sub-verbs
  (is (read? "kubectl" "--context=c" "auth" "can-i" "get" "pods"))
  (is (read? "kubectl" "auth" "whoami"))
  (is (not (read? "kubectl" "--context=c" "auth" "reconcile" "-f" "rbac.yaml"))))

(deftest kubectl-config-writes-are-acts
  (is (read? "kubectl" "config" "view" "--minify" "--output" "json"))
  (is (read? "kubectl" "config" "current-context"))
  (is (read? "kubectl" "config" "get-contexts"))
  (doseq [argv [["kubectl" "config" "use-context" "prod"]
                ["kubectl" "config" "set-context" "--current" "--namespace=x"]
                ["kubectl" "config" "delete-context" "prod"]
                ["kubectl" "config" "set-credentials" "u" "--token=t"]
                ["kubectl" "config" "set-cluster" "c" "--server=https://x"]
                ["kubectl" "config" "set" "contexts.c.namespace" "x"]
                ["kubectl" "config" "unset" "contexts.c"]
                ["kubectl" "config" "rename-context" "a" "b"]
                ["kubectl" "config"]]]
    (is (not (harness/read-command? argv)) (pr-str argv))))

(deftest a-dry-run-flag-after-dashes-belongs-to-the-remote-program
  (is (not (read? "kubectl" "exec" "p" "--" "sh" "-c" "rm -rf /data" "--dry-run=client")))
  (is (not (read? "kubectl" "--context=c" "exec" "p" "--dry-run=server" "--" "rm" "-rf" "/data"))
      "an exec is judged by the command it runs")
  (is (not (read? "kubectl" "run" "debug" "--image=busybox" "--" "sh" "-c" "rm -rf /data" "--dry-run=client")))
  (is (not (read? "kubectl" "debug" "node/n" "--image=busybox" "--" "chroot" "/host" "reboot" "--dry-run=server")))
  (is (read? "kubectl" "--context=c" "apply" "-f" "x.yaml" "--dry-run=server"))
  (is (read? "kubectl" "--context=c" "delete" "cm" "x" "--dry-run=client")))

(defn- psql? [& statements]
  (harness/read-command? (into ["kubectl" "exec" "p" "--" "psql" "-At"] (mapcat (fn [s] ["-c" s]) statements))))

(deftest psql-runs-one-read-statement-per-command
  (testing "reads"
    (is (psql? "SELECT 1"))
    (is (psql? "SELECT 1;"))
    (is (psql? "show pools"))
    (is (psql? "EXPLAIN SELECT * FROM t"))
    (is (psql? "select rolname || '|' || md5(rolpassword) from pg_authid\n where rolcanlogin order by rolname")))
  (testing "more than one statement"
    (is (not (psql? "SELECT 1; DROP TABLE users")))
    (is (not (psql? "SELECT 1;DROP TABLE users;"))))
  (testing "statements that are not SELECT, SHOW or plain EXPLAIN"
    (is (not (psql? "DROP TABLE users")))
    (is (not (psql? "WITH x AS (DELETE FROM t RETURNING *) SELECT * FROM x")))
    (is (not (psql? "EXPLAIN ANALYZE DELETE FROM t")))
    (is (not (psql? "explain (analyze, buffers) delete from t")))
    (is (not (psql? "EXPLAIN\nANALYSE DELETE FROM t")))
    (is (not (psql? "SELECT * INTO copy FROM t")))
    (is (not (psql? "SELECT * FROM t FOR UPDATE")))
    (is (not (psql? "SELECT 1" "VACUUM")) "every statement must read"))
  (testing "side-effecting functions inside a SELECT"
    (doseq [call ["pg_terminate_backend(pid)" "pg_cancel_backend(42)" "pg_reload_conf()"
                  "set_config('work_mem', '1GB', false)" "nextval('s')" "setval('s', 1)"
                  "pg_advisory_lock(1)" "lo_unlink(1234)" "dblink_exec('c', 'DROP TABLE t')"
                  "PG_TERMINATE_BACKEND (pid)" "pg_drop_replication_slot('x')"]]
      (is (not (psql? (str "SELECT " call " FROM pg_stat_activity"))) call)))
  (testing "a psql call that reads a script or its input"
    (is (not (read? "kubectl" "exec" "p" "--" "psql" "-f" "/tmp/x.sql" "-c" "SELECT 1")))
    (is (not (read? "kubectl" "exec" "p" "--" "psql" "--file=/tmp/x.sql")))
    (is (not (read? "kubectl" "exec" "p" "--" "psql"))))
  (testing "--command spells -c too"
    (is (read? "kubectl" "exec" "p" "--" "psql" "--command" "SELECT 1"))
    (is (not (read? "kubectl" "exec" "p" "--" "psql" "--command=SELECT 1; DROP TABLE t")))))

(deftest an-announcement-outside-the-change-hook-is-an-act
  (let [run (harness/run-config-dry!
             "announces-outside-the-hook"
             {:action (fn [_ _]
                        (announce/announce! "cluster" {:actor "a" :cluster "c"} "Restart web")
                        true)})]
    (is (= [{:http {:method :post :url "http://127.0.0.1:9/dry-run-announcements"}}]
           (harness/acts run))
        "the harness configures an announcer, so the POST shows")))

(deftest a-direct-http-client-post-is-an-act
  (let [run (harness/run-config-dry!
             "posts-directly"
             {:action (fn [_ _] (http-client/post "https://pad.example.org/new" {:body "x"}) true)})]
    (is (= [{:http {:method :post :url "https://pad.example.org/new"}}] (harness/acts run)))))

(deftest an-announcement-through-the-change-hook-is-not-made-under-dry-run
  (let [run (harness/run-config-dry!
             "announces-through-the-hook"
             {:action (fn [_ _]
                        (flow/change! {:confirmation {:action "restart" :target "c" :items ["web"]}
                                       :effect (effect/kubectl "c" "delete" "pod" "web")
                                       :announce! #(announce/announce! "cluster" {:actor "a" :cluster "c"} "Restart web")}))})]
    (is (= [] (harness/acts run)))))
