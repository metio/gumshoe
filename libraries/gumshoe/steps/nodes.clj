;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.steps.nodes
  "The steps that change a node's scheduling: cordon, drain and uncordon.

   Each step is a mutation spec (see gumshoe.mutation): its runbook runs it on
   its own, and a playbook composes it with others (see gumshoe.playbook). A step
   declares the prerequisites and options it needs, so whatever runs it derives
   its own from the steps. The book that runs a step names its :description,
   where the catalog reads it without loading the book. The bespoke parts - candidates, effect, checks - are
   small named functions, tested without a cluster."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]))

(def ^:private node-prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["nodes"]
   :kubectl-can-patch ["nodes"]})

(defn- node-option
  [verb]
  {:node {:desc (format "The node to %s - interactive selection when omitted" verb)
          :alias :n
          :coerce :string}})

(defn schedulable-nodes
  [context]
  (kubectl/schedulable-nodes (kubectl/get-all context "nodes")))

(defn cordoned-nodes
  [context]
  (kubectl/unschedulable-nodes (kubectl/get-all context "nodes")))

(defn all-nodes
  [context]
  (kubectl/names-of (kubectl/get-all context "nodes")))

(defn- unschedulable?
  [context node]
  (true? (-> (kubectl/get-cluster-resource context "nodes" node) :spec :unschedulable)))

(defn cordon-effect
  [context node]
  (effect/plan (effect/kubectl context "cordon" node)))

(defn unschedulable-check
  [context node]
  {:description (format "node %s is unschedulable" node)
   :check (fn [] (unschedulable? context node))})

(defn schedulable-check
  [context node]
  {:description (format "node %s is schedulable" node)
   :check (fn [] (not (unschedulable? context node)))})

(defn drain-effect
  [context node grace-period]
  (effect/plan (effect/kubectl context "drain" node
                               "--ignore-daemonsets" "--delete-emptydir-data"
                               (str "--grace-period=" grace-period))))

(defn drain-checks
  [context node]
  [(unschedulable-check context node)
   {:description (format "no evictable pods remain on node %s" node)
    :timeout 120 :interval 10
    ;; only a read that answered can show the node empty; a failed one
    ;; would read as no pods and verify a drain that did not happen
    :check (fn [] (let [{:keys [ok? doc]} (kubectl/get-json-result context "pods" "--all-namespaces"
                                                                   (str "--field-selector=spec.nodeName=" node))]
                    (and ok? (empty? (kubectl/drainable-pods doc)))))}])

(def cordon
  {:options (node-option "cordon")
   :prerequisites node-prerequisites
   :select {:mode :one :label "Node" :flag :node :candidates schedulable-nodes
            :preview "kubectl describe node {}"}
   :empty-message "every node is already cordoned"
   :confirm {:action "cordon (mark unschedulable) a node"}
   :announce (fn [{:keys [target]}] (format "Cordon node %s" target))
   :effect (fn [{:keys [context target]}] (cordon-effect context target))
   :verify (fn [{:keys [context target]}] [(unschedulable-check context target)])})

(def drain
  {:options (merge (node-option "drain")
                   {:grace-period {:desc "Seconds each pod is given to terminate - the pod's own value when omitted"
                                   :alias :g
                                   :default -1
                                   :coerce :long}})
   :prerequisites (merge node-prerequisites
                         {:kubectl-can-get ["nodes" "pods"]
                          :kubectl-can-delete ["pods"]})
   :select {:mode :one :label "Node" :flag :node :candidates all-nodes}
   :confirm {:action "drain a node - every pod on it is evicted and emptyDir data is deleted"
             :destructive? true}
   :announce (fn [{:keys [target]}] (format "Drain node %s" target))
   ;; -1 lets each pod keep its own terminationGracePeriodSeconds
   :effect (fn [{:keys [context target opts]}] (drain-effect context target (:grace-period opts)))
   :verify (fn [{:keys [context target]}] (drain-checks context target))})

(def uncordon
  {:options (node-option "uncordon")
   :prerequisites node-prerequisites
   :select {:mode :one :label "Node" :flag :node :candidates cordoned-nodes}
   :empty-message "no node is cordoned"
   :confirm {:action "uncordon (mark schedulable) a node"}
   :announce (fn [{:keys [target]}] (format "Uncordon node %s" target))
   :effect (fn [{:keys [context target]}] (effect/plan (effect/kubectl context "uncordon" target)))
   :verify (fn [{:keys [context target]}] [(schedulable-check context target)])})
