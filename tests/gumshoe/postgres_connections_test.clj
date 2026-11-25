;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.postgres-connections-test
  "The in-pod connection reading: one constant read-only statement run with
   psql over kubectl exec, and the pure parser of its CSV. The fixtures under
   tests/fixtures/postgres were recorded from a throwaway
   docker.io/library/postgres:18 container with exactly the command
   connection-command builds, once idle and once with max_connections 12 and
   superuser_reserved_connections 3 held by nine non-superuser sessions, at
   which point a tenth non-superuser login was refused with `remaining
   connection slots are reserved for roles with the SUPERUSER attribute`."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [edamame.core :as edamame]
            [gumshoe.postgres :as postgres]
            [gumshoe.shell :as shell]))

(def ^:private at-ceiling (slurp "tests/fixtures/postgres/pg-connections-at-ceiling.csv"))
(def ^:private idle (slurp "tests/fixtures/postgres/pg-connections-idle.csv"))

(def ^:private header
  "client_addr,usename,application_name,state,backend_type,backends,total,max_connections,superuser_reserved_connections,reserved_connections,read_only,in_recovery")

(defn- csv [& rows] (str/join "\n" (cons header rows)))

;; --- the parser -------------------------------------------------------------------

(deftest the-recorded-ceiling-parses-into-settings-and-rows
  (let [reading (postgres/parse-connections at-ceiling)]
    (is (= {:max-connections 12 :superuser-reserved-connections 3 :reserved-connections 0
            :read-only? true :in-recovery? false}
           (dissoc reading :rows)))
    (is (= 9 (count (:rows reading))))
    (is (= {:client-addr "127.0.0.1" :user "app" :application "moodle, php" :state "idle"
            :backend-type "client backend" :backends 3}
           (nth (:rows reading) 2))
        "a comma inside a quoted CSV field stays in the field")
    (is (= {:client-addr nil :user nil :application nil :state nil :backend-type "io worker" :backends 3}
           (nth (:rows reading) 1))
        "an empty field is no value, so a background process has no address and no user")
    (is (= 10 (postgres/client-backends reading)) "nine sessions plus the reading's own")
    (is (= 9 (postgres/connection-ceiling reading)))))

(deftest the-recorded-idle-server-parses
  (let [reading (postgres/parse-connections idle)]
    (is (= 100 (:max-connections reading)))
    (is (= 1 (postgres/client-backends reading)) "only the reading's own backend")
    (is (= 97 (postgres/connection-ceiling reading)))))

(deftest reserved-connections-lower-the-ceiling
  (is (= 87 (postgres/connection-ceiling {:max-connections 100 :superuser-reserved-connections 3
                                          :reserved-connections 10}))))

(def ^:private refusals
  "label and output that must not parse, each a way an answer can be partial or
   not the answer the statement gives."
  [["blank" ""]
   ["nil" nil]
   ["a header with no rows" header]
   ["a different header" (str/replace at-ceiling "backends," "count,")]
   ["an error message on stdout" "ERROR:  permission denied for view pg_stat_activity"]
   ["a row missing a column" (csv ",,,,walwriter,1,1,100,3,0,on")]
   ["a row with an extra column" (csv ",,,,walwriter,1,1,100,3,0,on,f,x")]
   ["a count that is not a number" (csv ",,,,walwriter,one,1,100,3,0,on,f")]
   ["a negative count" (csv ",,,,walwriter,-1,-1,100,3,0,on,f")]
   ["a total that does not add up, as a truncated answer leaves it"
    (csv "10.0.0.1,app,x,active,client backend,5,9,100,3,0,on,f")]
   ["settings that differ between rows"
    (csv "10.0.0.1,app,x,active,client backend,5,6,100,3,0,on,f"
         ",,,,walwriter,1,6,200,3,0,on,f")]
   ["a reading not taken read-only" (csv ",,,,walwriter,1,1,100,3,0,off,f")]
   ["a recovery flag that is neither t nor f" (csv ",,,,walwriter,1,1,100,3,0,on,maybe")]])

(deftest partial-or-foreign-answers-do-not-parse
  (doseq [[label out] refusals]
    (testing label
      (is (nil? (postgres/parse-connections out))))))

(deftest a-standby-reading-says-so
  (is (true? (:in-recovery? (postgres/parse-connections (csv ",,,,walwriter,1,1,100,3,0,on,t"))))))

(deftest an-absent-reserved-connections-setting-reads-as-zero
  ;; PostgreSQL before 16 has no reserved_connections; the statement
  ;; coalesces it to '0'
  (is (= 0 (:reserved-connections (postgres/parse-connections (csv ",,,,walwriter,1,1,100,3,0,on,f"))))))

;; --- truncation never reads as fewer connections -----------------------------------

(def ^:private seed 20261125)

(def ^:private gen-row
  (gen/let [addr (gen/elements ["" "10.0.0.1" "10.0.0.2" "fd00::1"])
            state (gen/elements ["" "active" "idle"])
            type (gen/elements ["client backend" "walwriter" "autovacuum worker"])
            n (gen/choose 1 600)]
    [addr "app" "pgbouncer" state type n]))

(defn- render
  [rows]
  (let [total (reduce + (map peek rows))]
    (apply csv (for [[addr user app state type n] rows]
                 (str/join "," [addr user app state type n total 1500 3 0 "on" "f"])))))

(def ^:private gen-output-and-cut
  (gen/let [rows (gen/vector gen-row 2 8)
            cut (gen/choose 1 (dec (count rows)))]
    [rows cut]))

(defn- render-truncated
  [rows cut]
  (let [lines (str/split-lines (render rows))]
    (str/join "\n" (take (inc (- (count rows) cut)) lines))))

(defn- truncation-never-parses
  [parse]
  (prop/for-all [[rows cut] gen-output-and-cut]
    (and (some? (parse (render rows)))
         ;; an answer that lost its last rows, as a cut stream would leave it
         (nil? (parse (render-truncated rows cut))))))

(deftest no-truncated-answer-parses-as-fewer-connections
  (let [result (tc/quick-check 300 (truncation-never-parses postgres/parse-connections) :seed seed)]
    (is (:pass? result) (pr-str (select-keys result [:shrunk :fail])))))

(deftest the-property-catches-a-parser-that-ignores-the-total
  ;; the positive control: a parser that keeps the rows it got without checking
  ;; them against the total must be caught
  (let [lenient (fn [out]
                  (with-redefs-fn {#'postgres/complete? (constantly true)}
                    #(postgres/parse-connections out)))]
    (is (not (:pass? (tc/quick-check 300 (truncation-never-parses lenient) :seed seed))))))

;; --- the statement is a constant -----------------------------------------------------

(defn- source-form
  "The top-level form of libraries/gumshoe/postgres.clj that defs name."
  [name]
  (some #(when (and (seq? %) (= 'def (first %)) (= name (second %))) %)
        (edamame/parse-string-all (slurp "libraries/gumshoe/postgres.clj")
                                  {:all true})))

(deftest the-statement-is-a-string-literal-in-the-source
  (let [form (source-form 'connection-sql)]
    (is form "connection-sql is defined at the top level")
    (is (string? (last form)) "a literal, never built with str or format")
    (is (= postgres/connection-sql (last form)))))

(deftest the-statement-holds-no-interpolation
  (doseq [marker ["%s" "%d" "~{" "${" "{{" ":" "?" ";"]]
    (is (not (str/includes? postgres/connection-sql marker)) marker))
  (is (re-find #"(?i)^SELECT " postgres/connection-sql) "a single SELECT")
  (doseq [word ["INSERT" "UPDATE" "DELETE" "DROP" "ALTER" "CREATE" "GRANT" "TRUNCATE" "COPY" "pg_terminate_backend" "pg_cancel_backend" "set_config"]]
    (is (not (str/includes? (str/upper-case postgres/connection-sql) (str/upper-case word))) word)))

(deftest the-command-is-the-same-for-every-target
  (let [seen (atom [])]
    (with-redefs [shell/execute (fn [& argv] (swap! seen conj (vec argv)) {:exit 1 :out "" :err ""})]
      (postgres/connection-reading "ctx" "ns-a" "pod-a" "postgres")
      (postgres/connection-reading "ctx" "x'; DROP TABLE t; --" "pod-b" "postgres"))
    (let [tails (map #(vec (drop-while (complement #{"--"}) %)) @seen)]
      (is (= 2 (count @seen)) (pr-str @seen))
      (is (apply = tails) "what runs in the pod does not depend on the target")
      (is (= (into ["--"] postgres/connection-command) (first tails))))))

(deftest the-command-reads-read-only-without-a-password-or-a-prompt
  (let [command postgres/connection-command]
    (is (= "psql" (first command)))
    (is (some #{"-X"} command) "no psqlrc")
    (is (some #{"-w"} command) "never prompts for a password")
    (is (some #{"--csv"} command))
    (is (some #(str/includes? % "default_transaction_read_only=on") command))
    (is (= postgres/connection-sql (last command)))
    (is (not-any? #(re-find #"(?i)password|PGPASSWORD|--host|-h$" %) command)
        "the local socket with peer authentication, so no secret is read")))

;; --- the exec ----------------------------------------------------------------------

(defn- answering
  [exit out]
  (fn [& argv]
    (is (= ["kubectl" "--context=ctx" "--namespace=db"] (take 3 argv)))
    (is (= ["exec" "db-1" "-c" "postgres" "--"] (take 5 (drop 4 argv))))
    {:exit exit :out out :err ""}))

(deftest the-reading-is-taken-only-on-exit-0
  (testing "an answer"
    (with-redefs [shell/execute (answering 0 at-ceiling)]
      (is (= 10 (postgres/client-backends (postgres/connection-reading "ctx" "db" "db-1" "postgres"))))))
  (testing "exec denied"
    (with-redefs [shell/execute (answering 1 "")]
      (is (nil? (postgres/connection-reading "ctx" "db" "db-1" "postgres")))))
  (testing "psql failed after printing part of its answer"
    (with-redefs [shell/execute (answering 2 at-ceiling)]
      (is (nil? (postgres/connection-reading "ctx" "db" "db-1" "postgres")))))
  (testing "a timeout"
    (with-redefs [shell/execute (answering 124 "")]
      (is (nil? (postgres/connection-reading "ctx" "db" "db-1" "postgres")))))
  (testing "exit 0 with something other than the answer"
    (with-redefs [shell/execute (answering 0 "psql: error: connection to server failed")]
      (is (nil? (postgres/connection-reading "ctx" "db" "db-1" "postgres"))))))
