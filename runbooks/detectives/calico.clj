;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.calico
  "Investigates calico as managed by the tigera-operator."
  (:require [infra.detective :as detective]
            [infra.detectives.calico :as calico]))

(detective/book
 {:description "Investigates calico: unavailable, degraded, or progressing tigera components"
  :detectives calico/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [calico/tigerastatus-type]}})
