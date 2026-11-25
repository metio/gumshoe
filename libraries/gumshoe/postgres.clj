;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.postgres
  "PostgreSQL access for db-operator managed databases: resolves credentials
   from the cluster, port-forwards the service, and runs psql or pg_dump.
   The password travels via PGPASSWORD, never via process arguments.

   It also reads who holds a server's connections from inside the server's
   own pod: one constant read-only statement, run with psql over kubectl exec
   on the local socket, and a pure parser of its CSV answer."
  (:require [clojure.data.csv :as csv]
            [clojure.string :as str]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(def instance-type "dbinstances.kinda.rocks")
(def database-type "databases.kinda.rocks")

(defn credentials-name
  "db-operator names the credentials after the database, deduplicating the
   'db-db' overlap: 'moodle-db' -> 'moodle-db-credentials'."
  [database]
  (str/replace (str database "-db-credentials") "db-db-credentials" "db-credentials"))

(defn split-host
  "'service.namespace.svc.cluster.local' -> {:service .. :namespace ..}"
  [host]
  (let [[service namespace] (str/split (str host) #"\.")]
    {:service service :namespace namespace}))

(defn decode
  [value]
  (when value
    (String. (.decode (java.util.Base64/getDecoder) ^String value) "UTF-8")))

(defn connection-complete?
  [{:keys [service namespace user password]}]
  (not-any? str/blank? [service namespace user password]))

;; ---------------------------------------------------------------------------
;; resolving connections from the cluster

(defn choose-database
  "Resolves the namespace/database pair from flags, optionally via an ingress
   host lookup, falling back to interactive selection."
  [context {:keys [namespace database ingress]}]
  (let [candidates (kubectl/namespaces-names (kubectl/get-all context database-type))
        namespace (or namespace
                      (when ingress
                        (first (kubectl/namespaces-with-ingress-host
                                (kubectl/get-all context "ingresses") ingress))))]
    (interact/choose-namespaced "Database" candidates namespace database)))

(defn admin-connection
  "Connection data for the admin user of a DbInstance."
  [context instance]
  (let [resource (kubectl/get-cluster-resource context instance-type instance)
        secret-namespace (-> resource :spec :adminSecretRef :Namespace)
        secret-name (-> resource :spec :adminSecretRef :Name)
        secret (kubectl/get-namespaced-resource context secret-namespace "secrets" secret-name)]
    (merge (split-host (-> resource :spec :generic :host))
           {:user (decode (-> secret :data :user))
            :password (decode (-> secret :data :password))})))

(defn database-connection
  "Connection data for a db-operator Database living in an app namespace."
  [context namespace database]
  (let [credentials (credentials-name database)
        configmap (kubectl/get-namespaced-resource context namespace "configmaps" credentials)
        secret (kubectl/get-namespaced-resource context namespace "secrets" credentials)]
    (merge (split-host (-> configmap :data :DB_CONN))
           {:database (decode (-> secret :data :POSTGRES_DB))
            :user (decode (-> secret :data :POSTGRES_USER))
            :password (decode (-> secret :data :POSTGRES_PASSWORD))})))

;; ---------------------------------------------------------------------------
;; running clients against a port-forward

(defn with-postgres
  "Runs f while the connection's postgres service is forwarded to local-port."
  [context connection local-port f]
  (kubectl/with-port-forward {:context context
                              :namespace (:namespace connection)
                              :service (:service connection)
                              :local-port local-port
                              :remote-port 5432}
    f))

(defn- psql-args
  [{:keys [user database]} local-port extra]
  (cond-> ["psql" "--host" "127.0.0.1" "--port" (str local-port) "--username" user]
    database (into ["--dbname" database])
    (seq extra) (into extra)))

(defn psql!
  "Interactive psql session against the forwarded port."
  [connection local-port & extra]
  (let [args (psql-args connection local-port extra)]
    (apply stdout/print-command "PGPASSWORD=***" args)
    (stdout/print-section-marker)
    (zero? (apply shell/run-with-output-env {"PGPASSWORD" (:password connection)} args))))

(defn pg-dump!
  "Writes a plain-format dump (--clean --if-exists) to file. A dump takes as
   long as the database needs, so it runs without the probe timeout."
  [connection local-port file]
  (let [args ["pg_dump" "--host" "127.0.0.1" "--port" (str local-port)
              "--username" (:user connection) "--dbname" (:database connection)
              "--clean" "--if-exists" "--format" "plain" "--file" file]]
    (apply stdout/print-command "PGPASSWORD=***" args)
    (let [result (binding [shell/*timeout-ms* nil]
                   (apply shell/execute-env {"PGPASSWORD" (:password connection)} args))]
      (if (zero? (:exit result))
        true
        (do (stdout/error (format "pg_dump failed:\n%s" (:err result))) false)))))

(def dump-header
  "The comment pg_dump opens a plain dump with."
  "-- PostgreSQL database dump")

(def dump-marker
  "The comment pg_dump closes a successful plain dump with."
  "PostgreSQL database dump complete")

(def ^:private window
  "Bytes read from each end of a dump. pg_dump writes its header on the first
   lines and its completion marker on the last few, so a window of this size
   holds both with room for the few lines a release may add around them."
  65536)

(defn dump-ends
  "{:head <the first bytes> :tail <the last bytes>} of file as text, reading
   at most `window` bytes from each end, so judging a dump costs the same for
   any size and never holds the whole file in memory."
  ([file] (dump-ends file window))
  ([file size]
   (with-open [raf (java.io.RandomAccessFile. (str file) "r")]
     (let [length (.length raf)
           chunk (fn [offset n]
                   (let [buffer (byte-array n)]
                     (.seek raf offset)
                     (.readFully raf buffer)
                     (String. buffer java.nio.charset.StandardCharsets/UTF_8)))
           n (int (min size length))]
       {:head (chunk 0 n)
        :tail (chunk (- length n) n)}))))

(defn dump-ends-complete?
  "Whether the ends of a plain dump show a complete one: pg_dump's header in
   its head and its completion marker in its tail."
  [{:keys [head tail]}]
  (boolean (and (string? head) (string? tail)
                (str/includes? head dump-header)
                (str/includes? tail dump-marker))))

(defn dump-complete?
  "pg_dump ends a successful plain dump with a completion marker - its absence
   means the dump is truncated and must not be trusted. Only the two ends of
   the file are read, so a dump of any size is judged. Any failure to read
   them, an Error included, counts as incomplete."
  [file]
  (try
    (dump-ends-complete? (dump-ends file))
    (catch Throwable _ false)))

;; ---------------------------------------------------------------------------
;; reading who holds the connections, from inside the server's pod

(def connection-sql
  "The one statement the connection reading runs. It is a constant: nothing of
   the cluster, the pod or the operator reaches it. It groups
   pg_stat_activity by client address, role, application, state and backend
   type, and repeats on every row the total over all rows, the settings that
   decide the connection ceiling, whether the transaction was read-only and
   whether the server is a standby. The total lets the parser refuse an answer
   that lost rows. reserved_connections exists from PostgreSQL 16 and reads
   as 0 before."
  "SELECT coalesce(host(client_addr), '') AS client_addr, usename, application_name, state, backend_type, count(*) AS backends, sum(count(*)) OVER () AS total, current_setting('max_connections') AS max_connections, current_setting('superuser_reserved_connections') AS superuser_reserved_connections, coalesce(current_setting('reserved_connections', true), '0') AS reserved_connections, current_setting('transaction_read_only') AS read_only, pg_is_in_recovery() AS in_recovery FROM pg_stat_activity GROUP BY 1, 2, 3, 4, 5 ORDER BY 6 DESC, 1, 2, 3, 4, 5")

(def ^:private read-only-session
  "The connection string: the postgres database, every transaction read-only
   from the moment the session starts, and a statement bounded in time."
  "dbname=postgres options='-c default_transaction_read_only=on -c statement_timeout=10s'")

(def connection-command
  "psql in the pod: no psqlrc (-X), never a password prompt (-w), CSV output,
   the role postgres over the local socket, so peer authentication answers
   and no secret is read. It runs without stdin and without a terminal."
  ["psql" "-X" "-w" "--csv" "-v" "ON_ERROR_STOP=1" "-U" "postgres"
   "--dbname" read-only-session "-c" connection-sql])

(def ^:private connection-columns
  ["client_addr" "usename" "application_name" "state" "backend_type" "backends" "total"
   "max_connections" "superuser_reserved_connections" "reserved_connections" "read_only"
   "in_recovery"])

(defn- count-of
  [s]
  (when (and (string? s) (re-matches #"[0-9]{1,9}" s))
    (parse-long s)))

(defn- value [s] (when-not (str/blank? s) s))

(defn- parse-row
  [fields]
  (when (= (count connection-columns) (count fields))
    (let [[addr user app state type backends total max superuser reserved read-only recovery] fields
          numbers (mapv count-of [backends total max superuser reserved])]
      (when (and (every? some? numbers) (#{"t" "f"} recovery))
        {:row {:client-addr (value addr) :user (value user) :application (value app)
               :state (value state) :backend-type (value type) :backends (numbers 0)}
         :total (numbers 1)
         :settings {:max-connections (numbers 2)
                    :superuser-reserved-connections (numbers 3)
                    :reserved-connections (numbers 4)
                    :read-only? (= "on" read-only)
                    :in-recovery? (= "t" recovery)}}))))

(defn- complete?
  "Whether the rows hold every backend the server counted: each row repeats
   the total, and the rows' own counts add up to it."
  [parsed]
  (and (apply = (map :total parsed))
       (= (:total (first parsed)) (reduce + (map (comp :backends :row) parsed)))))

(defn parse-connections
  "The reading psql printed for connection-sql in CSV, or nil unless it is the
   whole answer: the exact header, at least one row of the right shape, the
   same settings on every row, rows adding up to the total, and a read-only
   transaction. A partial answer is refused rather than read as fewer
   connections.

   {:max-connections 100 :superuser-reserved-connections 3
    :reserved-connections 0 :read-only? true :in-recovery? false
    :rows [{:client-addr \"10.0.0.5\" :user \"app\" :application \"pgbouncer\"
            :state \"active\" :backend-type \"client backend\" :backends 12}]}

   A row without a client address is a backend on the local socket or a
   process of the server's own."
  [out]
  (when (string? out)
    (let [[header & lines] (try (doall (csv/read-csv (str/trim out))) (catch Exception _ nil))
          parsed (map parse-row lines)]
      (when (and (= connection-columns header)
                 (seq parsed)
                 (every? some? parsed)
                 (apply = (map :settings parsed))
                 (complete? parsed)
                 (:read-only? (:settings (first parsed))))
        (assoc (:settings (first parsed)) :rows (mapv :row parsed))))))

(defn client-backends
  "The backends that hold one of max_connections' slots: client backends. The
   server's own processes and WAL senders have slots of their own."
  [reading]
  (reduce + (map :backends (filter #(= "client backend" (:backend-type %)) (:rows reading)))))

(defn connection-ceiling
  "How many client backends a role without superuser or
   pg_use_reserved_connections may reach before the server refuses it with
   `remaining connection slots are reserved`."
  [{:keys [max-connections superuser-reserved-connections reserved-connections]}]
  (- max-connections superuser-reserved-connections (or reserved-connections 0)))

(defn connection-reading
  "Who holds the connections of the PostgreSQL server in a pod's container,
   read by running connection-command there with kubectl exec, or nil unless
   the exec exited 0 with a whole answer. The container must let its OS user
   connect as postgres over the local socket by peer authentication, as
   CloudNativePG's instance pods and the official image do. Denied exec, a
   failed psql and a server refusing even its superuser-reserved slots all
   read as nil, never as zero connections."
  [context namespace pod container]
  (some-> (kubectl/exec-answer context namespace pod container connection-command)
          parse-connections))
