;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.rbac
  "Investigates RBAC hygiene: admins, wildcards, escalation verbs, secret readers, default tokens."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.rbac :as rbac]))

(detective/book
 {:description "Investigates RBAC hygiene: admins, wildcards, escalation verbs, secret readers, default tokens"
  :detectives rbac/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["clusterroles" "clusterrolebindings" "serviceaccounts" "pods"]}})
