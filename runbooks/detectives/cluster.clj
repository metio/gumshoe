;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.cluster
  "Runs every registered detective against the current cluster - the full investigation."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]))

(detective/book
 {:description "Runs every registered detective against the current cluster"
  :when-to-run "Reach for this first when something is wrong but you don't yet know where - it runs every cluster-wide check at once and points you at the area to dig into."
  :scope :all
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["nodes" "pods" "persistentvolumeclaims" "deployments" "statefulsets" "daemonsets" "jobs" "resourcequotas" "clusterroles" "clusterrolebindings" "rolebindings" "serviceaccounts" "networkpolicies"]}})
