;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.nodes.remove-taint
  "Removes a single taint from a single node. Two-step selection (node, then
   one of its taints) keeps this hand-written, but the mutation is an :effect
   plan, so it dry-runs and its plan is tested."
  (:require [infra.effect :as effect]
            [infra.flow :as flow]
            [infra.interact :as interact]
            [infra.kubectl :as kubectl]
            [infra.announce :as announce]
            [infra.runbook :as runbook]
            [infra.stdout :as stdout]))

(def options
  {:node {:desc "The node to remove a taint from - interactive selection when omitted"
          :alias :n
          :coerce :string}
   :taint {:desc "The taint to remove (key[=value]:effect) - interactive selection when omitted"
           :alias :t
           :coerce :string}})

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["nodes"]
   :kubectl-can-patch ["nodes"]})

(defn remove-taint-effect
  "The plan that removes a taint - kubectl removes by 'key:effect-'."
  [context node taint]
  (effect/plan (effect/kubectl context "taint" "nodes" node
                               (str (kubectl/taint-removal-spec taint) "-"))))

(defn gone-check
  [context node taint]
  {:description (format "node %s no longer carries taint %s" node taint)
   :check (fn [] (not-any? #(= taint %)
                           (kubectl/taints-of (kubectl/get-cluster-resource context "nodes" node))))})

(defn- remove-taint
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        nodes (kubectl/get-all context "nodes")
        tainted (kubectl/nodes-with-taints nodes)
        node (interact/choose-one "Node" tainted (:node opts))
        taint (when node
                (interact/choose-one "Taint" (kubectl/taints-of (kubectl/find-item nodes node)) (:taint opts)))]
    (cond
      (empty? tainted)
      (do (stdout/ok "no node has any taints") true)

      (nil? node)
      (do (stdout/error "no node selected") false)

      (nil? taint)
      (do (stdout/error "no taint selected") false)

      :else
      (flow/change!
       {:confirmation {:action "remove a taint - pods may start scheduling onto the node"
                       :target (format "%s (%s)" node cluster)
                       :items [taint]}
        :announce! #(announce/announce! cluster announcement-data
                                                     (format "Remove taint %s from node %s" taint node))
        :effect (remove-taint-effect context node taint)
        :post-checks [(gone-check context node taint)]}))))

(runbook/execute!
 {:description "Removes a single taint from a single node"
  :options options
  :prerequisites prerequisites
  :action remove-taint})
