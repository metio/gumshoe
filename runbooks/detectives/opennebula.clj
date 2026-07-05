;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.opennebula
  "Investigates OpenNebula over SSH to the frontend: hosts in error or offline
   states, failed VMs, and datastores running out of space."
  (:require [clojure.string :as str]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.opennebula :as opennebula-detectives]
            [gumshoe.inputs :as inputs]
            [gumshoe.opennebula :as opennebula]
            [gumshoe.runbook :as runbook]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]))

(def options
  ;; The frontend is not a hard-required flag: it resolves from --frontend, then
  ;; env.edn (:opennebula :frontend), so a configured host needs no flag.
  (merge {:frontend {:desc "The OpenNebula frontend host to SSH into - overrides env.edn"
                     :alias :f
                     :coerce :string}
          :user {:desc "The SSH user - your ssh config decides when omitted"
                 :alias :u
                 :coerce :string}}
         detective/output-option))

(def prerequisites
  {:installed-tools ["ssh"]})

(defn- investigate
  [opts _ctx]
  (detective/when-to-run! "Reach for this when a VM won't start or a datastore is filling up - OpenNebula hosts in error/offline states, failed VMs, and datastore capacity.")
  (let [frontend (inputs/value :opennebula-frontend opts)
        connection {:host frontend :user (:user opts)}]
    (stdout/print-section "🔌 Connection")
    (cond
      (str/blank? (str frontend))
      (do (stdout/error "no OpenNebula frontend - pass --frontend or set [:opennebula :frontend] in env.edn")
          false)

      (not (ssh/check-connection? connection))
      false

      :else
      (detective/report!
       opennebula-detectives/detectives
       (detective/run-detectives opennebula-detectives/detectives
                                 (opennebula/collect-evidence! connection))
       (:output opts "text")))))

(runbook/execute!
 {:description "Investigates OpenNebula over SSH: hosts, VMs, datastores"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action investigate})
