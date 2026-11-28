;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dry-run-books-test
  "Runs every book gumshoe ships under --dry-run and fails when one of them
   acts. --dry-run promises that nothing is changed, and this holds for every
   book, including those written later, only when it is checked for all of
   them.

   Each book's :action runs in this process with gumshoe.flow/*dry-run*
   bound, as the harness binds it. Every program any book starts, through
   gumshoe.shell, the port-forward or the fuzzy finder, is replaced by one
   fake executable that runs nothing, and the invocation is logged. The fake
   answers a kubectl get with one generic object, so a book gets past its
   selection and as far as its change. An HTTP request and a file write are
   logged instead of sent or written - babashka.http-client's calls too,
   which the announcers and the pad post through - and one webhook announcer
   is configured, so an announcement made outside the change hook shows as a
   POST. Every confirmation is answered yes
   by whatever asks it - the typed word, gum or a UI backend - so a book
   relying on the operator to decline would act here.

   A logged invocation passes only when it is a read the allow-list below
   names; anything else, including a program the list does not know, fails
   the test with the book and the command."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http-client]
            [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.book-surfaces-test :as surfaces]
            [gumshoe.command :as command]
            [gumshoe.flow :as flow]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.http :as http]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.outcome :as outcome]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.ui :as ui]
            [gumshoe.verify :as verify]))

;; --- the fake executable ----------------------------------------------------------------

(def ^:private fake-script
  "Prints the answer the harness chose and exits with its code. It never
   looks at its arguments, so nothing a book passes can make it act."
  "#!/bin/sh\nprintf '%s' \"$GUMSHOE_FAKE_OUT\"\nexit \"${GUMSHOE_FAKE_EXIT:-0}\"\n")

(defn- fake-executable!
  []
  (let [dir (fs/create-temp-dir {:prefix "gumshoe-dry-run-"})
        path (fs/path dir "fake")]
    (spit (str path) fake-script)
    (fs/set-posix-file-permissions path "rwx------")
    (fs/delete-on-exit dir)
    (fs/delete-on-exit path)
    (str path)))

(def ^:private namespace-name "shop")
(def ^:private object-name "web")

(defn- generic-object
  [type]
  {:apiVersion "v1"
   :kind (str/capitalize (first (str/split (str type) #"[./]")))
   :metadata {:name object-name
              :namespace namespace-name
              :uid "00000000-0000-0000-0000-000000000001"
              :resourceVersion "1"
              :generation 1
              :creationTimestamp "2026-01-01T00:00:00Z"
              :labels {}
              :annotations {}}
   :spec {}
   :status {}})

(def ^:private flags-with-values
  #{"-n" "--namespace" "-l" "--selector" "-o" "--output" "--context" "-c" "--container"
    "--field-selector" "--kubeconfig" "--sort-by" "--chunk-size"})

(defn- positionals
  "The arguments after `verb` that are neither flags nor a flag's value."
  [argv verb]
  (loop [[token & more] (rest (drop-while #(not= verb %) argv)) previous nil found []]
    (cond
      (nil? token) found
      (= "--" token) found
      (str/starts-with? token "-") (recur more token found)
      (flags-with-values previous) (recur more nil found)
      :else (recur more nil (conj found token)))))

(defn- output-format
  [argv]
  (some (fn [[a b]]
          (cond
            (#{"-o" "--output"} a) b
            (str/starts-with? a "--output=") (subs a 9)
            (and (str/starts-with? a "-o") (> (count a) 2)) (subs a 2)))
        (partition-all 2 1 argv)))

(defn- kubectl-answer
  [argv]
  (let [[type name] (positionals argv "get")
        [type name] (if (and type (nil? name) (str/includes? type "/"))
                      (str/split type #"/" 2)
                      [type name])]
    (cond
      (and (some #{"get"} argv) (not-any? #{"exec"} argv))
      (case (output-format argv)
        "json" (json/generate-string (if name
                                       (generic-object type)
                                       {:apiVersion "v1" :kind "List" :items [(generic-object type)]}))
        "name" (str type "/" object-name "\n")
        "")

      (and (some #{"config"} argv) (some #{"view"} argv))
      (json/generate-string {:current-context "ctx"
                             :contexts [{:name "ctx" :context {:cluster "cluster" :namespace namespace-name}}]})

      (and (some #{"auth"} argv) (some #{"can-i"} argv)) "yes\n"
      (some #{"current-context"} argv) "ctx\n"
      :else "")))

(defn- answer
  "What the fake prints for a command line."
  [[program & args :as argv]]
  (case program
    "kubectl" (kubectl-answer argv)
    "gum" (if (= "input" (first args)) object-name "")
    ""))

;; --- the log of everything a book tried ---------------------------------------------------

(def ^:private ^:dynamic *log* nil)

(defn- log!
  [entry]
  (when *log* (swap! *log* conj entry)))

(defn- split-call
  "A babashka.process call as its options and its command line. The options
   map may come first or last, and the command line may be strings, a vector
   of them, or one string the library tokenizes."
  [args]
  (let [[opts more] (cond
                      (map? (first args)) [(first args) (rest args)]
                      (map? (last args)) [(last args) (butlast args)]
                      :else [{} args])
        tokens (mapv str (flatten (seq more)))
        argv (if (and (= 1 (count tokens)) (str/includes? (first tokens) " "))
               (str/split (first tokens) #"\s+")
               tokens)]
    [opts argv]))

(defn- faked
  "A replacement for a babashka.process function that logs the command line
   and starts the fake executable in its place. briko.process, which runs
   gumshoe's captured commands, calls it with one map naming the command as
   :cmd and the whole environment as :env; that call keeps its redirections
   and gets the fake as its :cmd."
  [original fake]
  (fn [& args]
    (if-let [argv (and (map? (first args)) (empty? (rest args)) (:cmd (first args)))]
      (let [argv (mapv str argv)]
        (log! {:command argv})
        (original (-> (first args)
                      (assoc :cmd [fake (str "faked:" (first argv))])
                      (update :env merge {"GUMSHOE_FAKE_OUT" (answer argv)
                                          "GUMSHOE_FAKE_EXIT" "0"}))))
      (let [[opts argv] (split-call args)]
        (log! {:command argv})
        ;; the fake reads no input, so a string fed to it would only fail to copy
        (original (update (dissoc opts :in) :extra-env merge {"GUMSHOE_FAKE_OUT" (answer argv)
                                                 "GUMSHOE_FAKE_EXIT" "0"})
                  fake (str "faked:" (first argv)))))))

;; --- what counts as a read ----------------------------------------------------------------

(def ^:private kubectl-reads
  #{"get" "describe" "logs" "top" "version" "api-resources" "api-versions"
    "explain" "cluster-info" "events" "wait" "port-forward"})

(def ^:private kubectl-sub-reads
  "Verbs that read only with some of their sub-verbs: `auth reconcile`
   writes RBAC objects, and every `config` sub-verb that is not listed here
   (use-context, set-context, set-credentials, delete-context, unset,
   rename-context, ...) rewrites the operator's kubeconfig."
  {"auth" #{"can-i" "whoami"}
   "config" #{"view" "current-context" "get-contexts" "get-clusters" "get-users"}})

(defn- verb
  "The first argument that is neither a flag nor a flag's value."
  [args]
  (first (positionals (cons ::start args) ::start)))

(defn- after-dashes
  [argv]
  (next (drop-while #(not= "--" %) argv)))

(defn- words-start-with?
  "Whether `args`, flags left out, begin with one of the word sequences."
  [prefixes args]
  (let [words (remove #(str/starts-with? % "-") args)]
    (boolean (some #(= % (take (count %) words)) prefixes))))

(def ^:private ceph-reads
  [["status"] ["health"] ["df"] ["versions"] ["version"] ["report"]
   ["osd" "df"] ["osd" "dump"] ["osd" "tree"] ["osd" "ls"] ["osd" "stat"] ["osd" "pool" "ls"]
   ["crash" "ls"] ["crash" "ls-new"] ["crash" "info"] ["crash" "stat"]
   ["orch" "ls"] ["orch" "ps"] ["orch" "status"] ["orch" "host" "ls"] ["orch" "upgrade" "status"]
   ["mon" "dump"] ["mon" "stat"] ["pg" "stat"] ["pg" "dump"] ["fs" "status"] ["fs" "ls"]])

(def ^:private side-effecting-functions
  "Functions a SELECT can call that change something: they end other
   sessions, reload or change settings, advance sequences, take advisory
   locks, delete large objects or run a statement elsewhere."
  ["pg_terminate_backend" "pg_cancel_backend" "pg_reload_conf" "set_config" "nextval" "setval"
   "pg_advisory_lock" "pg_advisory_xact_lock" "pg_try_advisory_lock" "pg_try_advisory_xact_lock"
   "lo_unlink" "lo_import" "lo_export" "dblink_exec" "dblink" "pg_drop_replication_slot"
   "pg_create_logical_replication_slot" "pg_create_physical_replication_slot"
   "pg_switch_wal" "pg_rotate_logfile" "pg_promote" "pg_file_write" "pg_stat_reset"])

(def ^:private side-effecting-call
  (re-pattern (str "(?i)\\b(" (str/join "|" side-effecting-functions) ")\\s*\\(")))

(defn sql-read?
  "Whether one psql -c statement only reads. It must be a single statement
   (a semicolon is allowed only at its end, so `SELECT 1; DROP TABLE x`
   fails, and so does a semicolon inside a string, which errs on the side of
   an act), start with SELECT, SHOW or EXPLAIN, and stay a read: no EXPLAIN
   ANALYZE, which runs the statement, no SELECT INTO, which creates a table,
   no row locks, and no call of a function that changes something."
  [statement]
  (let [statement (str/replace (str/trim (str statement)) #";\s*$" "")]
    (boolean
     (and (not (str/includes? statement ";"))
          (re-find #"(?i)^(select|show|explain)\b" statement)
          (not (re-find #"(?is)^explain\b.*\banaly[sz]e\b" statement))
          (not (re-find #"(?i)\binto\b" statement))
          (not (re-find #"(?i)\bfor\s+(update|no\s+key\s+update|share|key\s+share)\b" statement))
          (not (re-find side-effecting-call statement))))))

(defn- sql-statements
  "The statements a psql call passes with -c, --command or --command=."
  [args]
  (concat (keep (fn [[flag statement]] (when (#{"-c" "--command"} flag) statement))
                (partition-all 2 1 args))
          (keep #(when (str/starts-with? % "--command=") (subs % 10)) args)))

(defn- sql-reads?
  "A psql call that runs only -c statements, each a read. One that reads a
   script (-f, --file) or its input runs statements nobody listed here."
  [args]
  (let [statements (sql-statements args)]
    (and (seq statements)
         (not-any? #(or (#{"-f" "--file"} %) (str/starts-with? % "--file=")) args)
         (every? sql-read? statements))))

(def ^:private remote-reads
  "Commands a book may run inside a pod or on a host over SSH during a dry
   run, by their program and, where the program can also write, the words
   that make it a read."
  {"etcdctl" (partial words-start-with? [["get"] ["endpoint"] ["member" "list"] ["version"] ["alarm" "list"]])
   "ceph" (partial words-start-with? ceph-reads)
   "psql" sql-reads?
   "onedatastore" (partial words-start-with? [["list"] ["show"]])
   "onehost" (partial words-start-with? [["list"] ["show"]])
   "onevm" (partial words-start-with? [["list"] ["show"]])
   "exit" #(= ["0"] %)
   "cat" (constantly true)
   "ls" (constantly true)
   "df" (constantly true)})

(defn- remote-read?
  [[program & args]]
  (boolean (when-let [read? (remote-reads program)] (read? args))))

(defn- before-dashes
  "The arguments kubectl reads itself: everything after `--` belongs to the
   program it runs in a pod."
  [args]
  (take-while #(not= "--" %) args))

(defn- server-dry-run?
  "kubectl's own dry run, which the API server evaluates and never persists.
   Only kubectl's own arguments count, so a --dry-run=client handed to the
   remote program after `--` makes nothing a dry run."
  [args]
  (boolean (some #{"--dry-run=server" "--dry-run=client"} (before-dashes args))))

(defn- kubectl-read?
  [args]
  (let [[v sub] (positionals (cons ::start args) ::start)]
    (cond
      (= "exec" v) (remote-read? (after-dashes args))
      (server-dry-run? args) true
      (= "rollout" v) (= "status" sub)
      (kubectl-sub-reads v) (contains? (kubectl-sub-reads v) sub)
      :else (contains? kubectl-reads v))))

(defn- ssh-read?
  "An ssh call is as much a read as the command it runs after the target."
  [args]
  (remote-read? (next (after-dashes args))))

(def ^:private program-reads
  {"kubectl" kubectl-read?
   "ssh" ssh-read?
   "gum" (fn [args] (contains? #{"confirm" "input" "choose" "filter" "style" "format"} (first args)))
   "fzf" (constantly true)
   "dig" (constantly true)
   "whoami" (constantly true)
   "date" (constantly true)
   "gopass" (partial words-start-with? [["show"] ["search"] ["ls"] ["list"]])
   "wg" (partial words-start-with? [["show"]])
   "ip" (fn [args] (some #{"show" "list"} args))
   "mimirtool" (partial words-start-with? [["analyze"]])
   "openssl" (fn [args] (contains? #{"s_client" "x509" "version"} (first args)))
   "helm" (fn [args] (contains? #{"list" "ls" "get" "history" "status" "show" "template" "version" "search"} (verb args)))
   "flux" (fn [args] (contains? #{"get" "tree" "stats" "logs" "events" "version" "check" "diff" "export" "trace"} (verb args)))})

(defn read-command?
  [[program & args]]
  (boolean (when-let [read? (program-reads program)] (read? args))))

(defn- act?
  [{:keys [command http write]}]
  (cond
    command (not (read-command? command))
    http (not (#{:get :head} (:method http)))
    write (not (str/starts-with? (str (fs/absolutize write))
                                 (str (fs/absolutize (System/getProperty "java.io.tmpdir")))))))

;; --- running a book -----------------------------------------------------------------------

(defn- option-value
  [option {:keys [coerce validate]}]
  (cond
    (set? validate) (first (sort (map str validate)))
    (= :namespace option) namespace-name
    (#{:long :int :double :number} coerce) (if (str/includes? (name option) "port") "18080" "1")
    (= :boolean coerce) nil
    (re-find #"url|prometheus|address|endpoint" (name option)) "http://127.0.0.1:9"
    :else object-name))

(defn argv-for
  "Arguments that name the generic object in every flag that takes a value
   and has no default (or a default nothing configured), followed by --dry-run, as a run would pass them. An
   option the harness cannot fill is left out, so a required one shows as a
   refusal rather than a pass."
  [options]
  (let [pairs (for [[option spec] (sort-by key options)
                    :when (nil? (:default spec))
                    :let [value (option-value option spec)]
                    :when value]
                [(str "--" (name option)) value])
        parses? (fn [argv] (try (runbook/arguments options argv) true (catch Exception _ false)))
        all (into [] cat pairs)
        kept (if (parses? (conj all "--dry-run"))
               all
               (reduce (fn [kept pair]
                         (let [candidate (into kept pair)]
                           (if (parses? (conj candidate "--dry-run")) candidate kept)))
                       []
                       pairs))]
    (conj kept "--dry-run")))

(defn- load-config
  [path]
  (binding [runbook/*loading* true]
    (load-file path)))

(def ^:private yes-forever
  (apply str (repeat 200 "yes\n")))

(def ^:private fake-announcer
  "The announcer every dry run is configured with, so an announcement shows
   as a POST to it in the log."
  {:type :webhook :url "http://127.0.0.1:9/dry-run-announcements"})

(defn- logged-http-client
  "A replacement for a babashka.http-client function that logs the request
   instead of sending it. method is the function's own method, or nil for
   request, which carries it in its options."
  [method]
  (fn [& args]
    (let [opts (if (map? (first args)) (first args) (second args))
          url (if (map? (first args)) (or (:uri opts) (:url opts)) (first args))]
      (log! {:http {:method (or method (:method opts) :get) :url (str url)}})
      {:status 599 :body "" :headers {}})))

(defn- with-dry-run-fakes
  "Calls f with every program, HTTP client, file write and announcer of the
   run replaced as run-dry! describes."
  [fake f]
  (let [process-fn process/process
        shell-fn process/shell
        eventually verify/eventually]
    (with-redefs [process/process (faked process-fn fake)
                  process/shell (faked shell-fn fake)
                  command/installed? (constantly true)
                  ui/backend (fn [kind] (when (= :confirm kind) (constantly true)))
                  fuzzy/select-single (fn [_ values & _] (first values))
                  fuzzy/select-multi (fn [_ values] (some-> (first values) vector))
                  http/request (fn [method url _]
                                 (log! {:http {:method method :url url}})
                                 {:reachable false :error "offline: dry-run harness"})
                  http/fetch-lines! (fn [url & _]
                                      (log! {:http {:method :get :url url}})
                                      {:reachable false :error "offline: dry-run harness"})
                  http-client/request (logged-http-client nil)
                  http-client/get (logged-http-client :get)
                  http-client/head (logged-http-client :head)
                  http-client/post (logged-http-client :post)
                  http-client/put (logged-http-client :put)
                  http-client/patch (logged-http-client :patch)
                  http-client/delete (logged-http-client :delete)
                  announce/announcers-of (constantly {:announcers [fake-announcer]})
                  spit (fn [f & _] (log! {:write (str f)}))
                  verify/eventually #(eventually (assoc % :timeout 0))]
      (f))))

(defn run-config-dry!
  "Runs a book's config, as a book file returns it while loading, under
   --dry-run and returns everything it tried, with its result. `book` names
   it in the result."
  [book config]
  (let [fake (fake-executable!)
        options (:options config {})
        argv (argv-for options)
        log (atom [])
        out (java.io.StringWriter.)
        err (java.io.StringWriter.)
        record (outcome/fresh)
        result (binding [*log* log
                         flow/*dry-run* true
                         outcome/*record* record
                         kubectl/*forward-start-ms* 200
                         *in* (java.io.BufferedReader. (java.io.StringReader. yes-forever))
                         *out* out
                         *err* err]
                 (with-dry-run-fakes
                   fake
                   (fn []
                     (try
                       ((:action config) (:opts (runbook/arguments options argv)) {})
                       (catch Throwable e (str "threw: " (ex-message e)))))))]
    {:book book :argv argv :result result :tried @log
     :record @record :output (str out err)}))

(defn run-dry!
  "Runs the book at `path` under --dry-run and returns everything it tried,
   with its result."
  [path]
  (run-config-dry! path (load-config path)))

(defn acts
  "What a run tried that is not a read."
  [run]
  (filterv act? (:tried run)))

;; --- the tests ----------------------------------------------------------------------------

(deftest the-allow-list-tells-reads-from-acts
  (testing "reads"
    (is (read-command? ["kubectl" "--context=c" "get" "pods" "-o" "json"]))
    (is (read-command? ["kubectl" "--context=c" "-n" "x" "logs" "p"]))
    (is (read-command? ["kubectl" "--context=c" "--namespace=kube-system" "exec" "etcd" "--"
                        "etcdctl" "--cacert=/a" "endpoint" "health"]))
    (is (read-command? ["gum" "confirm" "--default=false" "Proceed?"])))
  (testing "acts"
    (is (not (read-command? ["kubectl" "--context=c" "delete" "pod" "p"])))
    (is (not (read-command? ["kubectl" "--context=c" "-n" "get" "patch" "x"]))
        "a namespace called get is still a patch")
    (is (not (read-command? ["kubectl" "--context=c" "--namespace=kube-system" "exec" "etcd" "--"
                             "etcdctl" "--cacert=/a" "check" "perf" "--load=s"])))
    (is (not (read-command? ["kubectl" "exec" "p" "--" "sh" "-c" "rm -rf /"])))
    (is (not (read-command? ["upterm" "host" "--authorized-user" "github:alice"])))
    (is (not (read-command? ["some-tool-nobody-listed"]))
        "a program the list does not know is an act")
    (is (act? {:http {:method :post :url "http://127.0.0.1:9090/api"}}))
    (is (act? {:write "/home/u/.config/gumshoe/env.edn"}))
    (is (not (act? {:write (str (fs/path (System/getProperty "java.io.tmpdir") "x" "http.yaml"))}))
        "a scratch file under the temporary directory changes nothing")
    (is (not (read-command? ["ssh" "-q" "--" "u@h" "ceph" "osd" "out" "3"])))
    (is (read-command? ["ssh" "-q" "--" "u@h" "ceph" "osd" "dump" "--format" "json"]))
    (is (not (read-command? ["kubectl" "exec" "p" "--" "psql" "-c" "SHOW POOLS" "-c" "DROP TABLE x"])))
    (is (read-command? ["kubectl" "--context=c" "delete" "cm" "x" "--dry-run=server"]))))

(deftest the-harness-replaces-every-program
  (let [process-fn process/process
        log (atom [])
        fake (fake-executable!)]
    (binding [*log* log]
      (with-redefs [process/process (faked process-fn fake)]
        (is (= "" (:out @(process/process {:out :string} "upterm" "host"))))
        (is (re-find #"\"name\":\"web\""
                     (:out @(process/process {:out :string} "kubectl" "get" "pods" "-o" "json"))))))
    (is (= [{:command ["upterm" "host"]} {:command ["kubectl" "get" "pods" "-o" "json"]}] @log))))

(deftest the-harness-replaces-every-captured-command
  (let [process-fn process/process
        log (atom [])
        fake (fake-executable!)]
    (binding [*log* log]
      (with-redefs [process/process (faked process-fn fake)]
        (is (re-find #"\"name\":\"web\""
                     (:out (shell/execute "kubectl" "get" "pods" "-o" "json"))))
        (is (= 0 (:exit (shell/execute-with-stdin "{}" "kubectl" "apply" "-f" "-"))))))
    (is (= [{:command ["kubectl" "get" "pods" "-o" "json"]} {:command ["kubectl" "apply" "-f" "-"]}] @log))))

(deftest a-confirmation-alone-never-lets-a-dry-run-act
  (testing "the etcd performance check shows its command and runs nothing"
    (let [run (run-dry! "runbooks/etcd/check_performance.clj")]
      (is (true? (:result run)))
      (is (re-find #"would run: kubectl --context=ctx --namespace=shop exec web -- etcdctl .* check perf --load=s"
                   (:output run)))
      (is (= [] (acts run)))))
  (testing "the share book prints the upterm command and starts nothing"
    (let [run (run-dry! "runbooks/collaborate/share.clj")]
      (is (true? (:result run)))
      (is (re-find #"would run: upterm host --authorized-user github:web" (:output run)))
      (is (not-any? #(= "upterm" (first (:command %))) (:tried run)))))
  (testing "a bespoke change gated only by interact/confirm! is refused"
    (let [config {:action (fn [_ _]
                            (and (interact/confirm! {:action "restart everything" :target "c" :items ["x"]})
                                 (zero? (:exit @(process/process {:out :string} "kubectl" "delete" "pods" "--all")))))}
          log (atom [])
          err (java.io.StringWriter.)
          process-fn process/process
          fake (fake-executable!)
          result (binding [*log* log
                           flow/*dry-run* true
                           outcome/*record* (outcome/fresh)
                           *err* err]
                   (with-redefs [process/process (faked process-fn fake)
                                 ui/backend (fn [kind] (when (= :confirm kind) (constantly true)))]
                     ((:action config) {} {})))]
      (is (false? result))
      (is (= [] @log))
      (is (str/includes? (str err) "this book does not support a dry run - nothing was run")))))

(deftest no-book-acts-under-dry-run
  (doseq [path (surfaces/book-files)]
    (testing path
      (let [run (run-dry! path)]
        (is (= [] (acts run))
            (str path " acted under --dry-run with " (pr-str (:argv run))))))))

(def ^:private legacy-books
  "Every shipped file that hands gumshoe.flow/change! a legacy :execute!
   thunk. A dry run never calls one: flow records the step as not
   rehearsed and the run exits 1 (gumshoe.flow-test,
   gumshoe.runbook-test). A file joining this list declares a change that
   cannot be rehearsed, which its review should see."
  #{"tools/ceph/playbooks/upgrade.clj"
    "tools/db-operator/runbooks/admin_connect.clj"
    "tools/db-operator/runbooks/connect.clj"
    "tools/db-operator/runbooks/restore.clj"
    "tools/prometheus/runbooks/delete_metric.clj"})

(defn- shipped-sources
  []
  (let [roots (concat ["libraries" "runbooks" "playbooks" "firebooks"]
                      (for [tool (fs/list-dir "tools")
                            dir ["libraries" "runbooks" "playbooks" "firebooks"]
                            :let [root (fs/path tool dir)]
                            :when (fs/directory? root)]
                        (str root)))]
    (for [root roots
          :when (fs/directory? root)
          file (fs/glob root "**.clj")]
      (str (fs/relativize (fs/absolutize ".") (fs/absolutize file))))))

(deftest every-legacy-execute-step-is-known
  (let [engine #{"libraries/gumshoe/flow.clj" "libraries/gumshoe/outcome.clj"}
        found (set (for [file (shipped-sources)
                         :when (and (not (engine file))
                                    (re-find #"(?m)^\s*:execute!\s" (slurp file)))]
                     file))]
    (is (contains? found "tools/ceph/playbooks/upgrade.clj") "positive control: the scan finds a known user")
    (is (= legacy-books found))))
