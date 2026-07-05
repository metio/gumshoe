;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.observability
  "Investigates the observability scope: the prometheus-operator managed monitoring stack."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]))

(detective/book
 {:description "Investigates the observability scope: Prometheus, Alertmanager, ThanosRuler"
  :when-to-run "Reach for this when metrics or alerts look wrong - the prometheus-operator stack's Prometheus, Alertmanager, and ThanosRuler health."
  :scope :observability
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []}})
