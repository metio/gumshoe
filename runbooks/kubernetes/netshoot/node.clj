;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.netshoot.node
  "Starts an interactive netshoot debug session on a node."
  (:require [infra.effect :as effect]
            [infra.flow :as flow]
            [infra.interact :as interact]
            [infra.kubectl :as kubectl]
            [infra.runbook :as runbook]
            [infra.stdout :as stdout]))

(def options
  {:node {:desc "The node to debug - interactive selection when omitted"
          :alias :n
          :coerce :string}})

(def prerequisites
  {:installed-tools ["kubectl" "kubectl-netshoot" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["nodes"]
   :kubectl-can-create ["pods"]})

(defn- debug-node
  [opts _ctx]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        nodes (kubectl/names-of (kubectl/get-all context "nodes"))
        node (interact/choose-one "Node" nodes (:node opts))]
    (if (nil? node)
      (do (stdout/error "no node selected") false)
      (flow/change!
       {:confirmation {:action "start a privileged netshoot debug pod on a node"
                       :target cluster
                       :items [node]}
        :effect (effect/plan (effect/kubectl context "netshoot" "debug" (str "node/" node)))}))))

(runbook/execute!
 {:description "Starts an interactive netshoot debug session on a node"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action debug-node})
