;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.nodes.list-taints
  "Shows the taints of a single node."
  (:require [infra.interact :as interact]
            [infra.kubectl :as kubectl]
            [infra.runbook :as runbook]
            [infra.stdout :as stdout]))

(def options
  {:node {:desc "The node to inspect - interactive selection when omitted"
          :alias :n
          :coerce :string}})

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["nodes"]})

(defn- list-taints
  [opts _ctx]
  (let [context (kubectl/current-context)
        nodes (kubectl/get-all context "nodes")
        node-name (interact/choose-one "Node" (kubectl/names-of nodes) (:node opts))]
    (if (nil? node-name)
      (do (stdout/error "no node selected") false)
      (let [taints (kubectl/taints-of (kubectl/find-item nodes node-name))]
        (if (empty? taints)
          (stdout/ok (format "node %s has no taints" node-name))
          (doseq [taint taints]
            (println taint)))
        true))))

(runbook/execute!
 {:description "Shows the taints of a single node"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action list-taints})
