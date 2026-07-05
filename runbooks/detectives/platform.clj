;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.platform
  "Investigates the platform scope: control plane, nodes, calico, and the CSI storage layer."
  (:require [infra.detective :as detective]
            [infra.detectives.registry :as registry]))

(detective/book
 {:description "Investigates the platform scope: control plane, nodes, calico network, CSI storage"
  :when-to-run "Reach for this when the trouble looks below the apps - node readiness, the control plane, the Calico network, and the CSI storage layer."
  :scope :platform
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["nodes"]}})
