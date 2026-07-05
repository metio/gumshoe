;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.gateway-api
  "Investigates the Gateway API layer: Gateways, listeners, and HTTPRoutes."
  (:require [infra.detective :as detective]
            [infra.detectives.registry :as registry]))

(detective/book
 {:description "Investigates the Gateway API layer: Gateways, listeners, HTTPRoutes"
  :when-to-run "Reach for this when traffic isn't reaching a service - Gateway and route wiring, including the ListenerSet indirection routes attach through."
  :scope :traffic
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []}})
