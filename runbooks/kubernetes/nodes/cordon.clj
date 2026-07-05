;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.nodes.cordon
  "Marks a single node as unschedulable.

   Reference for the declarative mutation style: the whole book is data, the
   select/confirm/announce/verify flow is provided by gumshoe.mutation, and the
   bespoke bits (candidates, effect, check) are small named functions - so it
   composes, supports --dry-run, and is tested without a cluster."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn schedulable-nodes
  [context]
  (kubectl/schedulable-nodes (kubectl/get-all context "nodes")))

(defn cordon-effect
  [context node]
  (effect/plan (effect/kubectl context "cordon" node)))

(defn unschedulable-check
  [context node]
  {:description (format "node %s is unschedulable" node)
   :check (fn [] (true? (-> (kubectl/get-cluster-resource context "nodes" node)
                            :spec :unschedulable)))})

(mutation/book
 {:description "Marks a single node as unschedulable"
  :options {:node {:desc "The node to cordon - interactive selection when omitted"
                   :alias :n
                   :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["nodes"]
                  :kubectl-can-patch ["nodes"]}
  :select {:mode :one :label "Node" :flag :node :candidates schedulable-nodes}
  :empty-message "every node is already cordoned"
  :confirm {:action "cordon (mark unschedulable) a node"}
  :announce (fn [{:keys [target]}] (format "Cordon node %s" target))
  :effect (fn [{:keys [context target]}] (cordon-effect context target))
  :verify (fn [{:keys [context target]}] [(unschedulable-check context target)])})
