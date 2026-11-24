;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.dns
  "Investigates the DNS setup: dual-stack reachability, nameserver coherence,
   SOA serial replication, and answer consistency across nameservers."
  (:require [gumshoe.config :as config]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.dns :as dns-detectives]
            [gumshoe.dns :as dns]
            [gumshoe.inputs :as inputs]))

(def options
  (merge {:zone {:desc "The zone to investigate"
                 :alias :z
                 :default (inputs/env-default [:dns :zone])
                 :coerce :string}
          :server {:desc "The DNS server used as the entry point"
                   :alias :s
                   :default (inputs/env-default [:dns :server])
                   :coerce :string}
          :probes {:desc "Names that must resolve identically everywhere, repeatable"
                   :alias :p
                   :default (inputs/env-default [:dns :probes])
                   :coerce [:string]}}
         detective/output-option))

(def prerequisites
  {:installed-tools ["dig"]
   :can-ping-using-ipv4 [:server]})

(detective/book
 {:description "Investigates the DNS setup: transports, nameservers, replication, answer consistency"
  :options options
  :prerequisites prerequisites
  :detectives dns-detectives/detectives
  :when-to-run "Reach for this when names aren't resolving, mail is bouncing, or right after a nameserver change - transports, nameserver redundancy, SOA replication, and answer consistency across servers."
  :collect (fn [opts] (dns/collect-evidence! (select-keys opts [:server :zone :probes])))})
