;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.db-operator.admin-connect
  "Opens an interactive psql session as the admin user of a DbInstance."
  (:require [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.announce :as announce]
            [gumshoe.postgres :as postgres]
            [gumshoe.runbook :as runbook]
            [gumshoe.select :as select]
            [gumshoe.stdout :as stdout]))

(def instance-select
  {:mode :one :label "DbInstance" :flag :instance
   :candidates #(kubectl/names-of (kubectl/get-all % postgres/instance-type))
   :empty-message "no DbInstance exists"
   :on-empty :fail})

(def options
  (select/options instance-select
                  {:instance {:desc "The DbInstance to connect to - interactive selection when omitted"}
                   :port kubectl/local-port-spec}))

(def prerequisites
  {:installed-tools ["kubectl" "psql" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get [postgres/instance-type "secrets"]})

(defn- admin-connect
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        instance (select/pick instance-select opts context)]
    (if (nil? instance)
      false
      (let [connection (postgres/admin-connection context instance)]
        (if-not (postgres/connection-complete? connection)
          (do (stdout/error (format "could not resolve the admin credentials of %s" instance)) false)
          (do
            (stdout/print-data-table {:cluster cluster
                                      :instance instance
                                      :service (format "%s/%s" (:namespace connection) (:service connection))
                                      :user (:user connection)})
            (flow/change!
             {:confirmation {:action "open an ADMIN psql session - every statement runs with full privileges"
                             :target (format "%s (%s)" instance cluster)
                             :items [(format "%s/%s" (:namespace connection) (:service connection))]}
              :announce! #(announce/announce! cluster announcement-data
                                                           (format "Admin psql session on DbInstance %s" instance))
              :execute! #(postgres/with-postgres context connection (:port opts)
                           (fn [port] (postgres/psql! connection port)))})))))))

(runbook/execute!
 {:description "Opens an interactive psql session as the admin user of a DbInstance"
  :options options
  :prerequisites prerequisites
  :action admin-connect})
