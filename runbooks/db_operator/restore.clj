;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.db-operator.restore
  "Restores a dump into a db-operator managed database. The dump file is
   validated before anything happens: it must exist and carry pg_dump's
   completion marker, because restoring a truncated dump destroys data."
  (:require [babashka.fs :as fs]
            [infra.flow :as flow]
            [infra.kubectl :as kubectl]
            [infra.announce :as announce]
            [infra.postgres :as postgres]
            [infra.runbook :as runbook]
            [infra.stdout :as stdout]))

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
   :port {:desc "The local port used for the port-forward"
          :alias :p
          :default 42538
          :coerce :long}})

(def prerequisites
  {:installed-tools ["kubectl" "psql" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get [postgres/database-type "secrets" "configmaps"]})

(defn- restore
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        file (:dump opts)]
    (cond
      (not (fs/exists? file))
      (do (stdout/error (format "dump file %s does not exist" file)) false)

      (not (postgres/dump-complete? file))
      (do (stdout/error (format "%s is missing pg_dump's completion marker - refusing to restore a truncated dump" file))
          false)

      :else
      (let [target (postgres/choose-database context opts)]
        (if (nil? target)
          (do (stdout/error "no Database selected") false)
          (let [{:keys [namespace name]} (kubectl/split-namespace-name target)
                connection (postgres/database-connection context namespace name)]
            (if-not (postgres/connection-complete? connection)
              (do (stdout/error (format "could not resolve the credentials of %s" target)) false)
              (do
                (stdout/print-data-table {:cluster cluster
                                          :database target
                                          :dump file})
                (flow/change!
                 {:confirmation {:action "restore a dump - existing objects are dropped and replaced"
                                 :target (format "%s (%s)" target cluster)
                                 :items [file]
                                 :destructive? true}
                  :announce! #(announce/announce! cluster announcement-data
                                                               (format "Restore dump into database %s" target))
                  :execute! #(postgres/with-postgres context connection (:port opts)
                               (fn [] (postgres/psql! connection (:port opts)
                                                      "--set" "ON_ERROR_STOP=on"
                                                      "--file" file)))})))))))))

(runbook/execute!
 {:description "Restores a dump into a db-operator managed database"
  :options options
  :prerequisites prerequisites
  :action restore})
