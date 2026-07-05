;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.pods
  "Investigates pod health: crash loops, image pull errors, OOM kills, restarts, Pending/Failed, stuck terminating."
  (:require [infra.detective :as detective]
            [infra.detectives.pods :as pods]))

(detective/book
 {:description "Investigates pod health: crash loops, image pull errors, OOM kills, restarts, Pending/Failed"
  :detectives pods/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["pods"]}})
