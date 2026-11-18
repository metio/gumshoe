;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.pods
  "Investigates pod health: restart loops, image pull errors, OOM kills, Pending/Failed, stuck terminating."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.pods :as pods]))

(detective/book
 {:description "Investigates pod health: restart loops, image pull errors, OOM kills, Pending/Failed"
  :detectives pods/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["pods"]}})
