;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.flux
  "Investigates flux: sources and reconciliations that are failing or suspended."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.flux :as flux]))

(detective/book
 {:description "Investigates flux: failing or suspended sources and reconciliations"
  :detectives flux/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [flux/helmrelease-type flux/kustomization-type flux/gitrepository-type]}})
