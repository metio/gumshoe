;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.external-dns
  "Investigates external-dns end to end: every hostname the cluster declares
   (Ingresses, HTTPRoutes, Gateway listeners, DNSEndpoints) must actually
   resolve on the DNS server."
  (:require [gumshoe.config :as config]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.external-dns :as external-dns-detectives]
            [gumshoe.external-dns :as external-dns]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]))

(def options
  (merge {:server {:desc "The DNS server the records must exist on"
                   :alias :s
                   :default (config/env-value {} [:dns :server])
                   :coerce :string}}
         detective/output-option))

(def prerequisites
  {:installed-tools ["kubectl" "dig"]
   :cluster-capabilities []
   :kubectl-can-get ["ingresses"]
   :can-ping-using-ipv4 [:server]})

(runbook/execute!
 {:description "Investigates external-dns: cluster-declared hostnames must resolve in DNS"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action (fn [opts _ctx]
            (detective/when-to-run! "Reach for this when a hostname points nowhere - it checks that every hostname your ingresses and routes declare actually resolves in DNS.")
            (detective/report!
             external-dns-detectives/detectives
             (detective/run-detectives external-dns-detectives/detectives
                                       (external-dns/collect-evidence! (kubectl/current-context)
                                                                       (:server opts)))
             (:output opts "text")))})
