;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.db-operator.connect
  "Opens an interactive psql session on a db-operator managed database."
  (:require [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.announce :as announce]
            [gumshoe.postgres :as postgres]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(def options
  {:namespace {:desc "The namespace of the Database - interactive selection when omitted"
               :alias :n
               :coerce :string}
   :database {:desc "The Database to connect to - interactive selection when omitted"
              :alias :d
              :coerce :string}
   :ingress {:desc "A public ingress host, used to look up the namespace"
             :alias :i
             :coerce :string}
   :port {:desc "The local port used for the port-forward"
          :alias :p
          :default 42538
          :coerce :long}})

(def prerequisites
  {:installed-tools ["kubectl" "psql" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get [postgres/database-type "secrets" "configmaps"]})

(defn- connect
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        target (postgres/choose-database context opts)]
    (if (nil? target)
      (do (stdout/error "no Database selected") false)
      (let [{:keys [namespace name]} (kubectl/split-namespace-name target)
            connection (postgres/database-connection context namespace name)]
        (if-not (postgres/connection-complete? connection)
          (do (stdout/error (format "could not resolve the credentials of %s" target)) false)
          (do
            (stdout/print-data-table {:cluster cluster
                                      :database target
                                      :service (format "%s/%s" (:namespace connection) (:service connection))
                                      :user (:user connection)})
            (flow/change!
             {:confirmation {:action "open a psql session with the application's credentials"
                             :target (format "%s (%s)" target cluster)
                             :items [(:database connection)]}
              :announce! #(announce/announce! cluster announcement-data
                                                           (format "psql session on database %s" target))
              :execute! #(postgres/with-postgres context connection (:port opts)
                           (fn [] (postgres/psql! connection (:port opts))))})))))))

(runbook/execute!
 {:description "Opens an interactive psql session on a db-operator managed database"
  :options options
  :prerequisites prerequisites
  :action connect})
