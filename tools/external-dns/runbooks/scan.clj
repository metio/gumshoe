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
            [gumshoe.inputs :as inputs]
            [gumshoe.kubectl :as kubectl]))

(def options
  (merge {:server {:desc "The DNS server the records must exist on"
                   :alias :s
                   :default (inputs/env-default [:dns :server])
                   :coerce :string}}
         detective/output-option))

(def prerequisites
  {:installed-tools ["kubectl" "dig"]
   :cluster-capabilities []
   :kubectl-can-get ["ingresses"]
   :can-ping-using-ipv4 [:server]})

(detective/book
 {:description "Investigates external-dns: cluster-declared hostnames must resolve in DNS"
  :options options
  :prerequisites prerequisites
  :detectives external-dns-detectives/detectives
  :when-to-run "Reach for this when a hostname points nowhere - it checks that every hostname your ingresses and routes declare actually resolves in DNS."
  :collect (fn [opts] (external-dns/collect-evidence! (kubectl/current-context) (:server opts)))})
