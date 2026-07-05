;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.nodes.uncordon
  "Marks a single cordoned node as schedulable again."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn cordoned-nodes
  [context]
  (kubectl/unschedulable-nodes (kubectl/get-all context "nodes")))

(defn schedulable-check
  [context node]
  {:description (format "node %s is schedulable" node)
   :check (fn [] (not (true? (-> (kubectl/get-cluster-resource context "nodes" node)
                                 :spec :unschedulable))))})

(mutation/book
 {:description "Marks a single cordoned node as schedulable again"
  :options {:node {:desc "The node to uncordon - interactive selection when omitted"
                   :alias :n
                   :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["nodes"]
                  :kubectl-can-patch ["nodes"]}
  :select {:mode :one :label "Node" :flag :node :candidates cordoned-nodes}
  :empty-message "no node is cordoned"
  :confirm {:action "uncordon (mark schedulable) a node"}
  :announce (fn [{:keys [target]}] (format "Uncordon node %s" target))
  :effect (fn [{:keys [context target]}] (effect/plan (effect/kubectl context "uncordon" target)))
  :verify (fn [{:keys [context target]}] [(schedulable-check context target)])})
