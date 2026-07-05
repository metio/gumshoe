;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.gitops
  "Investigates the gitops scope: every flux source and reconciliation."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.flux]))

(detective/book
 {:description "Investigates the gitops scope: every flux source and reconciliation"
  :when-to-run "Reach for this when a change didn't land - Flux sources and reconciliations that are failing, suspended, or stuck."
  :scope :gitops
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []}})
