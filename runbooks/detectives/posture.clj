;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.posture
  "Investigates the configuration posture: risks that are not failures yet."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]))

(detective/book
 {:description "Investigates the configuration posture: probes, image tags, replicas, disruption budgets, deprecated APIs"
  :when-to-run (str "Reach for this before an upgrade, a node rotation or a production launch - it lists "
                    "workloads that run fine today and break on the next drain, image push or upgrade: "
                    "missing readiness probes, mutable image tags, single replicas, replicated workloads "
                    "without a PodDisruptionBudget, and deprecated APIs still in use.")
  :scope :posture
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["deployments" "statefulsets" "daemonsets" "cronjobs" "poddisruptionbudgets" "horizontalpodautoscalers"]}})
