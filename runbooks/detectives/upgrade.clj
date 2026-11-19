;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.upgrade
  "Investigates whether the cluster can take its next Kubernetes upgrade."
  (:require [gumshoe.detective :as detective]))

(detective/book
 {:description "Investigates upgrade readiness: removed APIs, webhooks, APIServices, version skew, drain blockers"
  :when-to-run (str "Reach for this before a Kubernetes minor upgrade. It gathers what decides whether the upgrade "
                    "goes through: deprecated APIs still requested or stored, unavailable APIServices and CRDs, "
                    "admission webhooks whose backend is missing or not ready (an upgrade recreates every pod), "
                    "kubelets the skew policy forbids or the next minor would leave behind, and "
                    "PodDisruptionBudgets that block the drains.")
  :scope :upgrade
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["nodes" "validatingwebhookconfigurations" "apiservices"]}})
