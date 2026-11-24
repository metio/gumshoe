;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.mail
  "Investigates mail infrastructure end to end: deliverability records
   (MX, SPF, DMARC, DKIM, reverse DNS) and the live SMTP/POP3/IMAP services
   (reachable ports, STARTTLS on cleartext ports, TLS certificate validity)."
  (:require [gumshoe.config :as config]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.mail :as mail-detectives]
            [gumshoe.inputs :as inputs]
            [gumshoe.mail :as mail]))

(def options
  (merge {:domain {:desc "The mail domain (used for MX, SPF, DMARC, DKIM lookups)"
                   :alias :d
                   :default (inputs/env-default [:mail :domain])
                   :coerce :string}
          :host {:desc "The mail server host to probe"
                 :alias :m
                 :default (inputs/env-default [:mail :host])
                 :coerce :string}
          :server {:desc "The DNS server to resolve records against"
                   :alias :s
                   :default (inputs/env-default [:dns :server])
                   :coerce :string}
          :dkim-selectors {:desc "DKIM selectors to check, repeatable"
                           :alias :k
                           :default ["default" "mail"]
                           :coerce [:string]}}
         detective/output-option))

(def prerequisites
  {:installed-tools ["dig" "openssl"]
   :can-ping-using-ipv4 [:host]})

(detective/book
 {:description "Investigates mail infrastructure: deliverability records and live SMTP/POP3/IMAP services"
  :options options
  :prerequisites prerequisites
  :detectives mail-detectives/detectives
  :when-to-run "Reach for this when mail is bouncing or landing in spam - deliverability records (SPF/DKIM/DMARC/reverse DNS) and the live SMTP/POP3/IMAP services."
  :collect (fn [opts] (mail/collect-evidence! (select-keys opts [:domain :host :server :dkim-selectors])))})
