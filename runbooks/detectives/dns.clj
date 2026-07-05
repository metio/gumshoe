;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.dns
  "Investigates the DNS setup: dual-stack reachability, nameserver coherence,
   SOA serial replication, and answer consistency across nameservers."
  (:require [gumshoe.config :as config]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.dns :as dns-detectives]
            [gumshoe.dns :as dns]
            [gumshoe.runbook :as runbook]))

(def options
  (merge {:zone {:desc "The zone to investigate"
                 :alias :z
                 :default (config/env-value {} [:dns :zone])
                 :coerce :string}
          :server {:desc "The DNS server used as the entry point"
                   :alias :s
                   :default (config/env-value {} [:dns :server])
                   :coerce :string}
          :probes {:desc "Names that must resolve identically everywhere, repeatable"
                   :alias :p
                   :default (config/env-value {} [:dns :probes])
                   :coerce [:string]}}
         detective/output-option))

(def prerequisites
  {:installed-tools ["dig"]
   :can-ping-using-ipv4 [:server]})

(runbook/execute!
 {:description "Investigates the DNS setup: transports, nameservers, replication, answer consistency"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action (fn [opts _ctx]
            (detective/when-to-run! "Reach for this when names aren't resolving, mail is bouncing, or right after a nameserver change - transports, nameserver redundancy, SOA replication, and answer consistency across servers.")
            (detective/report!
             dns-detectives/detectives
             (detective/run-detectives dns-detectives/detectives
                                       (dns/collect-evidence! {:server (:server opts)
                                                               :zone (:zone opts)
                                                               :probes (:probes opts)}))
             (:output opts "text")))})
