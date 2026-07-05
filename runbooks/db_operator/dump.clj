;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.db-operator.dump
  "Dumps a db-operator managed database to a local file. The dump is verified
   to be complete before the runbook calls it a success, and an existing file
   is never overwritten."
  (:require [babashka.fs :as fs]
            [infra.kubectl :as kubectl]
            [infra.postgres :as postgres]
            [infra.runbook :as runbook]
            [infra.stdout :as stdout]
            [infra.verify :as verify]))

(def options
  {:namespace {:desc "The namespace of the Database - interactive selection when omitted"
               :alias :n
               :coerce :string}
   :database {:desc "The Database to dump - interactive selection when omitted"
              :alias :d
              :coerce :string}
   :ingress {:desc "A public ingress host, used to look up the namespace"
             :alias :i
             :coerce :string}
   :dump {:desc "The file to write the dump to"
          :alias :r
          :require true
          :coerce :string}
   :port {:desc "The local port used for the port-forward"
          :alias :p
          :default 42538
          :coerce :long}})

(def prerequisites
  {:installed-tools ["kubectl" "pg_dump" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get [postgres/database-type "secrets" "configmaps"]})

(defn- dump
  [opts _ctx]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        file (:dump opts)
        target (postgres/choose-database context opts)]
    (cond
      (fs/exists? file)
      (do (stdout/error (format "%s already exists - refusing to overwrite a dump" file)) false)

      (nil? target)
      (do (stdout/error "no Database selected") false)

      :else
      (let [{:keys [namespace name]} (kubectl/split-namespace-name target)
            connection (postgres/database-connection context namespace name)]
        (if-not (postgres/connection-complete? connection)
          (do (stdout/error (format "could not resolve the credentials of %s" target)) false)
          (do
            (stdout/print-data-table {:cluster cluster
                                      :database target
                                      :dump file})
            (stdout/print-section "⚡ Action")
            (and (postgres/with-postgres context connection (:port opts)
                   (fn [] (postgres/pg-dump! connection (:port opts) file)))
                 (do (stdout/print-section "🔎 Post-check")
                     (verify/all
                      [{:description (format "%s ends with pg_dump's completion marker" file)
                        :timeout 0
                        :check #(postgres/dump-complete? file)}])))))))))

(runbook/execute!
 {:description "Dumps a db-operator managed database to a local file"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action dump})
