;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.db-operator.admin-connect
  "Opens an interactive psql session as the admin user of a DbInstance."
  (:require [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.announce :as announce]
            [gumshoe.postgres :as postgres]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(def options
  {:instance {:desc "The DbInstance to connect to - interactive selection when omitted"
              :alias :i
              :coerce :string}
   :port {:desc "The local port used for the port-forward"
          :alias :p
          :default 42538
          :coerce :long}})

(def prerequisites
  {:installed-tools ["kubectl" "psql" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get [postgres/instance-type "secrets"]})

(defn- admin-connect
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        instances (kubectl/names-of (kubectl/get-all context postgres/instance-type))
        instance (interact/choose-one "DbInstance" instances (:instance opts))]
    (if (nil? instance)
      (do (stdout/error "no DbInstance selected") false)
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
                           (fn [] (postgres/psql! connection (:port opts))))})))))))

(runbook/execute!
 {:description "Opens an interactive psql session as the admin user of a DbInstance"
  :options options
  :prerequisites prerequisites
  :action admin-connect})
