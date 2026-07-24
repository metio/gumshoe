;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.apiversions
  "Investigates CRD version discovery: resources whose API-server-advertised
   version has drifted from what their CRD actually serves - stale aggregated
   discovery that makes clients 404 healthy objects and Flux health-checks hang."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.apiversions :as detectives]))

(detective/book
 {:description "Investigates CRD discovery drift: served versions vs what the API server advertises"
  :when-to-run (str "Reach for this when a GitOps stage hangs in health-check reporting NotFound, "
                    "or kubectl 404s a resource whose CRD clearly serves it. It catches stale "
                    "aggregated discovery after a controller upgrade changed a CRD's served versions: "
                    "the objects are fine and apply works, but every client that resolves the kind "
                    "through discovery asks for a version the CRD no longer serves.")
  :detectives detectives/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["customresourcedefinitions"]}})
