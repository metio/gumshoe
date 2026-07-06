;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.nodes
  "Detectives for node health: readiness, every node-problem condition, cordoned
   nodes."
  (:require [gumshoe.kubectl :as kubectl]))

(defn- condition
  [node type]
  (first (filter #(= type (:type %)) (-> node :status :conditions))))

(defn detect-node-problems
  [evidence]
  (let [nodes (kubectl/items-of (get evidence "nodes"))]
    (concat
     (for [node nodes
           :let [ready (condition node "Ready")]
           :when (not= "True" (:status ready))]
       {:severity :critical
        :component (kubectl/name-of node)
        :summary (format "node is not Ready (status %s)" (or (:status ready) "unknown"))
        :hint (:message ready)})
     ;; Every non-Ready node condition follows the same convention: status True
     ;; means the problem is present. Reading them generically catches the built-in
     ;; pressures and NetworkUnavailable, plus every node-problem-detector
     ;; condition (KernelDeadlock, ReadonlyFilesystem, FrequentKubeletRestart, ...),
     ;; without a hardcoded list.
     (for [node nodes
           condition (-> node :status :conditions)
           :let [type (:type condition)]
           :when (not= "Ready" type)
           :when (= "True" (:status condition))]
       {:severity :warning
        :component (kubectl/name-of node)
        :summary (format "%s is active" type)
        :hint (:message condition)})
     (for [node nodes
           :when (true? (-> node :spec :unschedulable))]
       {:severity :info
        :component (kubectl/name-of node)
        :summary "node is cordoned (unschedulable)"
        :hint "uncordon with runbooks/kubernetes/nodes/uncordon.clj when the work on it is done"}))))

(def detectives
  [{:name "nodes"
    :description "Node health: readiness, every active node condition (pressures, NetworkUnavailable, node-problem-detector), cordoned nodes"
    :requires ["nodes"]
    :detect detect-node-problems}])
