;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.stageset
  "Investigates the delivery scope: every StageSet's rollout health."
  (:require [gumshoe.detective :as detective]
            [gumshoe.tools.stageset]))

(detective/book
 {:description "Investigates the delivery scope: StageSets that are not Ready or held"
  :when-to-run "Reach for this when a staged release stalled - StageSets that are not Ready, blocked on a gate or budget, rolled back, or held by an update window."
  :scope :delivery
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities [:stageset]}})
