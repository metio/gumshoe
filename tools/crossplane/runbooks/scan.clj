;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.crossplane
  "Investigates Crossplane: Providers, Functions and Configurations."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.crossplane :as crossplane]
            [gumshoe.tools.crossplane]))

(detective/book
 {:description "Investigates Crossplane: packages that do not install or are unhealthy"
  :when-to-run "Reach for this when Crossplane managed resources stop syncing or compositions fail - Providers, Functions and Configurations that are not installed or not healthy."
  :detectives crossplane/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [crossplane/provider-type]}})
