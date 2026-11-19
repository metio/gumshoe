;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.karpenter
  "Investigates Karpenter: NodePools and NodeClaims."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.karpenter :as karpenter]
            [gumshoe.tools.karpenter]))

(detective/book
 {:description "Investigates Karpenter: NodePools that cannot launch, NodeClaims that never become nodes"
  :when-to-run "Reach for this when pods stay Pending on a Karpenter cluster - NodePools that are not Ready or at their limits, and NodeClaims stuck on their way to a node."
  :detectives karpenter/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [karpenter/nodepool-type karpenter/nodeclaim-type]}})
