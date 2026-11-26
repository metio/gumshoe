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
   verifies its result exactly as the runbook does. A failure at any step
   prints what that step's failure means, followed by the node's state read
   at the failure: whether it stays cordoned, the pods a drain still evicts
   from it, and how to uncordon it."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.disruption :as disruption]
            [gumshoe.detectives.nodes :as node-detectives]
            [gumshoe.playbook :as playbook]
            [gumshoe.select :as select]
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
   :check (fn [{:keys [context]}] (detective/investigate! context node-detectives/detectives))
   :recovery nodes/verification-recovery})

(def selection
  {:mode :one :label "Node" :flag :node :candidates nodes/all-nodes})

(playbook/book
 {:description "Playbook: cordon, drain, maintain, uncordon, and verify a node"
  :options (select/options selection {:node {:desc "The node to maintain - interactive selection when omitted"}})
  :select selection
  :confirm {:action "take a node out of service for maintenance and bring it back"}
  :recovery nodes/maintenance-recovery
  :steps [disruption-check
          (assoc nodes/cordon :title "🚧 Cordon" :recovery nodes/cordon-recovery)
          (assoc nodes/drain :title "💨 Drain" :recovery nodes/drain-recovery)
          {:pause "perform the maintenance on the node now"}
          (assoc nodes/uncordon :title "🔓 Uncordon" :recovery nodes/uncordon-recovery)
          health-check]})
