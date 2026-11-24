;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns playbooks.kubernetes.node-maintenance
  "Playbook: takes a node out of service for maintenance and brings it back.

   1. checks for PodDisruptionBudgets that would block the drain, before the
      confirmation
   2. cordons the node
   3. drains it
      ... waits while you do the actual maintenance
   4. uncordons the node
   5. verifies node health with the detectives

   Cordon, drain and uncordon are the steps of the node runbooks, so each one
   verifies its result exactly as the runbook does."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.disruption :as disruption]
            [gumshoe.detectives.nodes :as node-detectives]
            [gumshoe.playbook :as playbook]
            [gumshoe.steps.nodes :as nodes]))

(def disruption-check
  (detective/check-step
   {:title "🔬 Disruption check"
    :prerequisites {:installed-tools ["kubectl"]
                    :kubectl-can-get ["pods" "poddisruptionbudgets"]}
    :detectives disruption/detectives
    :blocking? false}))

(def health-check
  {:title "🔬 Verification"
   :prerequisites {:installed-tools ["kubectl"]
                   :kubectl-can-get ["nodes" "pods"]}
   :check (fn [{:keys [context]}] (detective/investigate! context node-detectives/detectives))})

(playbook/book
 {:description "Playbook: cordon, drain, maintain, uncordon, and verify a node"
  :options {:node {:desc "The node to maintain - interactive selection when omitted"
                   :alias :n
                   :coerce :string}}
  :select {:mode :one :label "Node" :flag :node :candidates nodes/all-nodes}
  :confirm {:action "take a node out of service for maintenance and bring it back"}
  :steps [disruption-check
          (assoc nodes/cordon :title "🚧 Cordon")
          (assoc nodes/drain :title "💨 Drain")
          {:pause "perform the maintenance on the node now"}
          (assoc nodes/uncordon :title "✅ Uncordon")
          health-check]})
