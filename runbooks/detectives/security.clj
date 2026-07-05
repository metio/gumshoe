;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.security
  "Investigates the security posture: RBAC hygiene, pod security, and network segmentation."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]))

(detective/book
 {:description "Investigates the security posture: RBAC, pod security, network segmentation"
  :when-to-run "Reach for this when auditing access or after a permissions change - surprising cluster-admin bindings, pod security gaps, and namespaces without network policies."
  :scope :security
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["clusterroles" "clusterrolebindings" "rolebindings" "serviceaccounts" "pods" "networkpolicies"]}})
