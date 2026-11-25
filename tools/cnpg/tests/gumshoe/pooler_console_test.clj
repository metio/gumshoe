;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pooler-console-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.pooler-console :as console]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]))

(def ^:private health-check
  "C-0x1: pgbouncer/pgbouncer@unix(1):5432 closing because: client close request (age=0s)")

(def ^:private missing-metric
  "{\"msg\":\"Missing metric\",\"query\":\"SHOW LISTS\",\"metric\":\"peers\"}")

(def ^:private auth-failure
  "WARNING: server login failed: FATAL password authentication failed for user \"app\"")

(def ^:private connect-failure
  "C-0x2: postgres/app@10.24.160.213:38018 closing because: server login has been failing, cached error: connect failed (server_login_retry) (age=0s)")

(def ^:private client-auth-failure
  "C-0x3: db/app@10.24.160.9:51468 closing because: password authentication failed for user \"app\" (age=4209s)")

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

(deftest a-server-login-failure-is-counted-as-its-own-leg-test
  (let [verdict (console/classify (str/join "\n" [auth-failure client-auth-failure]))]
    (is (= 2 (:authentication verdict)))
    (is (= 1 (:server-login verdict)) "only the server-leg line counts as a server login")))

(deftest a-server-leg-rejection-names-the-version-and-calls-a-roll-relief-test
  (let [verdict (console/classify auth-failure)
        advice (first (console/guidance verdict {:metadata {:namespace "db" :name "db-rw"}}))]
    (is (re-find #"on a client's behalf" advice))
    (is (re-find #"1\.25\.1 and 1\.25\.2" advice))
    (is (re-find #"relief rather than a fix" advice))
    (is (re-find #"rollout restart deployment/db-rw" advice))
    (is (not (re-find #"instance rebuild" advice))
        "a stale pooler is one cause of a rejected credential, not the only one")))

(deftest a-client-leg-rejection-points-at-the-credential-path-test
  (let [verdict (console/classify client-auth-failure)
        advice (first (console/guidance verdict {:metadata {:namespace "db" :name "db-rw"}}))]
    (is (= :authentication (:verdict verdict)))
    (is (zero? (:server-login verdict)))
    (is (re-find #"before reaching the backend" advice))
    (is (re-find #"auth_query" advice))
    (is (not (re-find #"1\.25\.1" advice))
        "the pooler never reached the backend, so the server-leg defect is not implicated")))

(deftest the-unreachable-verdict-does-not-suggest-rolling-test
  (let [advice (first (console/guidance {:verdict :unreachable}
                                        {:metadata {:namespace "db" :name "db-rw"}}))]
    (is (re-find #"not a pooler fault" advice))
    (is (not (re-find #"rollout restart" advice)))))

(def ^:private served-connection
  "S-0x9: db/app@10.24.160.213:5432 closing because: server idle timeout (age=1215s)")

(deftest a-connection-that-lived-counts-as-served-test
  (let [verdict (console/classify (str/join "\n" [served-connection health-check]))]
    (is (= :quiet (:verdict verdict)))
    (is (= 1 (:served verdict)))))

(deftest a-rejected-login-closes-at-age-zero-and-is-not-served-test
  (let [verdict (console/classify
                 "S-0x9: db/app@10.24.160.213:5432 closing because: password authentication failed for user \"app\" (age=0s)")]
    (is (zero? (:served verdict)) "a login that failed never lived")))

(deftest a-quiet-window-with-a-served-connection-reports-it-test
  (let [verdict (console/classify served-connection
                 )
        advice (first (console/guidance verdict {:metadata {:namespace "db" :name "db-rw"}}))]
    (is (re-find #"1 server connection in it" advice))
    (is (re-find #"a claim about the window and not about the present" advice))))

(deftest a-quiet-verdict-does-not-claim-health-test
  (is (re-find #"not evidence"
               (first (console/guidance {:verdict :quiet}
                                        {:metadata {:namespace "db" :name "db-rw"}})))))

(defn- stamped [at line] (format "{\"ts\":\"%s\",\"msg\":\"%s\"}" at line))

(deftest a-failure-at-the-end-of-the-window-describes-the-present-test
  (let [verdict (console/classify
                 (str/join "\n" [(stamped "2026-11-04T20:59:00Z" health-check)
                                 (stamped "2026-11-04T20:59:30Z" connect-failure)]))]
    (is (= :unreachable (:verdict verdict)))
    (is (not (:stale? verdict)))
    (is (re-find #"not a pooler fault"
                 (first (console/guidance verdict {:metadata {:namespace "db" :name "db-rw"}}))))))

(deftest a-failure-left-behind-in-the-window-is-reported-as-historical-test
  (let [verdict (console/classify
                 (str/join "\n" [(stamped "2026-10-30T19:56:03Z" connect-failure)
                                 (stamped "2026-11-04T20:59:30Z" health-check)]))
        advice (first (console/guidance verdict {:metadata {:namespace "db" :name "db-rw"}}))]
    (is (= :unreachable (:verdict verdict)))
    (is (:stale? verdict))
    (is (re-find #"historical" advice))
    (is (re-find #"5d 1h" advice))
    (is (not (re-find #"not a pooler fault" advice)))))

(deftest a-window-without-timestamps-makes-no-claim-about-recency-test
  (let [verdict (console/classify connect-failure)]
    (is (nil? (:window-end verdict)))
    (is (not (:stale? verdict)))))

(def ^:private pooler-book
  (binding [runbook/*loading* true]
    (load-file "tools/cnpg/runbooks/pooler_console.clj")))

(defn- run-pooler-console
  "Runs the pooler console against a cluster holding one Pooler and its pod,
   with every exec answered by exec and the log read answered by logs.
   Returns the action's result and what it printed."
  [exec logs]
  (let [pooler {:metadata {:namespace "db" :name "pg-rw"} :spec {:cluster {:name "pg"}}}
        pod {:metadata {:namespace "db" :name "pg-rw-0"}}
        printed (java.io.StringWriter.)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  shell/execute (fn [& argv]
                                  (cond
                                    (some #{"exec"} argv) exec
                                    (some #{"logs"} argv) logs
                                    (some #(= "poolers.postgresql.cnpg.io" %) argv)
                                    {:exit 0 :out (json/generate-string {:items [pooler]}) :err ""}
                                    (some #{"pods"} argv)
                                    {:exit 0 :out (json/generate-string {:items [pod]}) :err ""}
                                    :else {:exit 1 :out "" :err "unexpected"}))]
      (let [result (binding [*out* printed *err* printed]
                     ((:action pooler-book) {:namespace "db" :pooler "pg-rw" :lines 200} {}))]
        {:result result :out (str printed)}))))

(deftest an-unread-log-gives-no-verdict-test
  (let [{:keys [result out]} (run-pooler-console {:exit 0 :out "database list" :err ""}
                                                 {:exit 1 :out "" :err "container not found"})]
    (is (false? result))
    (is (str/includes? out "could not read the pgbouncer log of pg-rw-0"))
    (is (not (str/includes? out "No login failure")))))

(deftest a-failed-admin-command-is-named-test
  (let [{:keys [result out]} (run-pooler-console {:exit 1 :out "" :err "exec failed"}
                                                 {:exit 0 :out "" :err ""})]
    (is (true? result) "the log answered, so the verdict stands")
    (is (str/includes? out "could not run SHOW SERVERS on the admin console of pg-rw-0"))))

(deftest a-read-empty-log-reads-quiet-test
  (let [{:keys [result out]} (run-pooler-console {:exit 0 :out "x" :err ""} {:exit 0 :out "" :err ""})]
    (is (true? result))
    (is (str/includes? out "No login failure"))))
