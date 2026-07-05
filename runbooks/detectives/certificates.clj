;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.certificates
  "Investigates cert-manager: certificates that are not ready or expire soon, and sour ACME orders."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.certificates :as certificates]))

(detective/book
 {:description "Investigates cert-manager: certificates not ready or expiring soon, sour ACME orders"
  :when-to-run "Reach for this when TLS is failing or a certificate is close to expiry - cert-manager certificates and stuck ACME orders."
  :detectives certificates/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [certificates/certificate-type certificates/order-type]}})
