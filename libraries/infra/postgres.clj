;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.postgres
  "PostgreSQL access for db-operator managed databases: resolves credentials
   from the cluster, port-forwards the service, and runs psql or pg_dump.
   The password travels via PGPASSWORD, never via process arguments."
  (:require [clojure.string :as str]
            [infra.interact :as interact]
            [infra.kubectl :as kubectl]
            [infra.shell :as shell]
            [infra.stdout :as stdout]))

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
  "Writes a plain-format dump (--clean --if-exists) to file."
  [connection local-port file]
  (let [args ["pg_dump" "--host" "127.0.0.1" "--port" (str local-port)
              "--username" (:user connection) "--dbname" (:database connection)
              "--clean" "--if-exists" "--format" "plain" "--file" file]]
    (apply stdout/print-command "PGPASSWORD=***" args)
    (let [result (apply shell/execute-env {"PGPASSWORD" (:password connection)} args)]
      (if (zero? (:exit result))
        true
        (do (stdout/error (format "pg_dump failed:\n%s" (:err result))) false)))))

(defn dump-complete?
  "pg_dump ends a successful plain dump with a completion marker - its absence
   means the dump is truncated and must not be trusted."
  [file]
  (try
    (str/includes? (slurp file) "PostgreSQL database dump complete")
    (catch Exception _ false)))
