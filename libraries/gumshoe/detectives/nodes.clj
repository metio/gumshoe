;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.nodes
  "Detectives for node health: readiness, resource pressure, cordoned nodes."
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
     (for [node nodes
           pressure ["MemoryPressure" "DiskPressure" "PIDPressure"]
           :let [state (condition node pressure)]
           :when (= "True" (:status state))]
       {:severity :warning
        :component (kubectl/name-of node)
        :summary (format "%s is active" pressure)
        :hint (:message state)})
     (for [node nodes
           :when (true? (-> node :spec :unschedulable))]
       {:severity :info
        :component (kubectl/name-of node)
        :summary "node is cordoned (unschedulable)"
        :hint "uncordon with runbooks/kubernetes/nodes/uncordon.clj when the work on it is done"}))))

(def detectives
  [{:name "nodes"
    :description "Node health: readiness, memory/disk/PID pressure, cordoned nodes"
    :requires ["nodes"]
    :detect detect-node-problems}])
