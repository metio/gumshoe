;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.nodes.drain
  "Drains a single node: cordons it and evicts every pod running on it."
  (:require [infra.effect :as effect]
            [infra.kubectl :as kubectl]
            [infra.mutation :as mutation]))

(defn all-nodes
  [context]
  (kubectl/names-of (kubectl/get-all context "nodes")))

(defn drain-effect
  [context node grace-period]
  (effect/plan (effect/kubectl context "drain" node
                               "--ignore-daemonsets" "--delete-emptydir-data"
                               (str "--grace-period=" grace-period))))

(defn drain-checks
  [context node]
  [{:description (format "node %s is unschedulable" node)
    :check (fn [] (true? (-> (kubectl/get-cluster-resource context "nodes" node)
                             :spec :unschedulable)))}
   {:description (format "no evictable pods remain on node %s" node)
    :timeout 120 :interval 10
    :check (fn [] (empty? (kubectl/drainable-pods (kubectl/pods-on-node context node))))}])

(mutation/book
 {:description "Drains a single node: cordons it and evicts every pod running on it"
  :options {:node {:desc "The node to drain - interactive selection when omitted"
                   :alias :n
                   :coerce :string}
            :grace-period {:desc "Seconds each pod is given to terminate - the pod's own value when omitted"
                           :alias :g
                           :default -1
                           :coerce :long}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["nodes" "pods"]
                  :kubectl-can-patch ["nodes"]
                  :kubectl-can-delete ["pods"]}
  :select {:mode :one :label "Node" :flag :node :candidates all-nodes}
  :confirm {:action "drain a node - every pod on it is evicted and emptyDir data is deleted"
            :destructive? true}
  :announce (fn [{:keys [target]}] (format "Drain node %s" target))
  :effect (fn [{:keys [context target opts]}]
            ;; the grace period comes from the flag; -1 means "use each pod's own"
            (drain-effect context target (:grace-period opts)))
  :verify (fn [{:keys [context target]}] (drain-checks context target))})
