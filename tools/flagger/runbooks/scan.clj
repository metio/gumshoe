;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.flagger
  "Investigates Flagger: Canaries."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.flagger :as flagger]
            [gumshoe.tools.flagger]))

(detective/book
 {:description "Investigates Flagger: Canaries that rolled back or are stuck"
  :when-to-run "Reach for this when a progressive rollout with Flagger does not go through - failed analyses, failing checks, Canaries that never initialize, rollouts waiting at a gate."
  :detectives flagger/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [flagger/canary-type]}})
