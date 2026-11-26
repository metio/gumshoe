;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.db-operator.restore
  "Restores a dump into a db-operator managed database. The dump file is
   validated before anything happens: it must exist and carry pg_dump's
   completion marker, because restoring a truncated dump destroys data, and
   every statement in it must be able to run inside one transaction. psql
   replays it in that transaction, so a failure rolls the whole restore back
   and leaves the database as it was."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.announce :as announce]
            [gumshoe.outcome :as outcome]
            [gumshoe.postgres :as postgres]
            [gumshoe.postgres-script :as script]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(def options
  {:namespace {:desc "The namespace of the Database - interactive selection when omitted"
               :alias :n
               :coerce :string}
   :database {:desc "The Database to restore into - interactive selection when omitted"
              :alias :d
              :coerce :string}
   :ingress {:desc "A public ingress host, used to look up the namespace"
             :alias :i
             :coerce :string}
   :dump {:desc "The dump file to restore"
          :alias :r
          :require true
          :coerce :string}
   :port kubectl/local-port-spec})

(def prerequisites
  {:installed-tools ["kubectl" "psql" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get [postgres/database-type "secrets" "configmaps"]})

(defn transaction-refusal
  "Why the dump whose transaction-breakers are `breakers` cannot be restored
   in one transaction, or nil when it can. A dump that could not be read is
   refused too, since nothing then shows it can."
  [file breakers]
  (cond
    (:unread breakers)
    (format "could not read %s to check that it can be restored in one transaction - %s" file (:unread breakers))

    (:unjudged breakers)
    (format "%s: %s - refusing to restore it. A statement this long is usually a COPY or REINDEX missing its terminating semicolon; check the dump around that line."
            file (:unjudged breakers))

    (seq breakers)
    (str (format "%s holds statements that cannot run inside one transaction, so a failed restore could not be rolled back - refusing to restore it:\n" file)
         (str/join "\n" (map (fn [{:keys [line statement]}] (format "  line %d: %s" line statement)) (take 10 breakers)))
         (when (> (count breakers) 10) (format "\n  ... and %d more" (- (count breakers) 10)))
         "\nWrite the dump of a single database with pg_dump --format plain and without --create, as dump.clj does.")

    :else nil))

(def confirmation-action
  "restore a dump - in one transaction, existing objects are dropped and recreated, and a failure rolls everything back")

(defn failure-guidance
  "The recovery printed when the replay failed: psql ran it in one
   transaction with ON_ERROR_STOP, so the failure rolled it back."
  [target]
  (str (format "Nothing was changed: psql replayed the dump in one transaction, so the failure rolled it back and %s is as it was" target)
       " (the one exception is a connection that broke while the final COMMIT was in flight)."
       "\nRead psql's error above, fix its cause and run the restore again."))

(defn replay!
  "Replays file into the database of connection through port and records the
   failure guidance when psql fails. True on success."
  [connection port file target]
  (or (apply postgres/psql! connection port (postgres/restore-args file))
      (do (outcome/recovery! (failure-guidance target) {})
          false)))

(defn- restore
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        file (:dump opts)
        ;; read only once the file exists and is complete
        refusal (delay (transaction-refusal file (script/file-transaction-breakers file)))]
    (cond
      (not (fs/exists? file))
      (do (stdout/error (format "dump file %s does not exist" file)) false)

      (not (postgres/dump-complete? file))
      (do (stdout/error (format "%s is missing pg_dump's completion marker - refusing to restore a truncated dump" file))
          false)

      @refusal
      (do (stdout/error @refusal) false)

      :else
      (let [target (postgres/choose-database context opts)]
        (if (nil? target)
          (do (stdout/error "no Database selected") false)
          (let [{:keys [namespace name]} (kubectl/namespace-name-map target)
                connection (postgres/database-connection context namespace name)]
            (if-not (postgres/connection-complete? connection)
              (do (stdout/error (format "could not resolve the credentials of %s" target)) false)
              (do
                (stdout/print-data-table {:cluster cluster
                                          :database target
                                          :dump file})
                (flow/change!
                 {:confirmation {:action confirmation-action
                                 :target (format "%s (%s)" target cluster)
                                 :items [file]
                                 :destructive? true}
                  :announce! #(announce/announce! cluster announcement-data
                                                               (format "Restore dump into database %s" target))
                  :execute! #(postgres/with-postgres context connection (:port opts)
                               (fn [port] (replay! connection port file target)))})))))))))

(runbook/execute!
 {:description "Restores a dump into a db-operator managed database"
  :options options
  :prerequisites prerequisites
  :action restore})
