;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.monitoring
  "Investigates the prometheus-operator monitoring stack: Prometheus, Alertmanager, ThanosRuler."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.monitoring :as monitoring]))

(detective/book
 {:description "Investigates the monitoring stack: Prometheus, Alertmanager, ThanosRuler"
  :detectives monitoring/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [monitoring/prometheus-type monitoring/alertmanager-type monitoring/thanosruler-type]}})
