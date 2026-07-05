;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.capacity
  "Detective for cluster resource commitment. Maps to the kube-prometheus
   KubeCPUOvercommit / KubeMemoryOvercommit alerts: if the sum of pod requests
   exceeds what remains after losing the single largest node, the cluster can
   not tolerate a node failure; if it exceeds total capacity, pods can not even
   schedule."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.quantity :as quantity]))

(defn- pod-requests
  [pods resource]
  (quantity/sum (for [pod (kubectl/items-of pods)
                      container (-> pod :spec :containers)]
                  (get-in container [:resources :requests resource]))))

(defn- node-allocatables
  [nodes resource]
  (keep #(quantity/quantity->number (get-in % [:status :allocatable resource]))
        (kubectl/items-of nodes)))

(defn- assess
  [label unit-divisor unit-suffix requested allocatables]
  (let [total (reduce + 0.0 allocatables)
        largest (if (seq allocatables) (apply max allocatables) 0.0)
        headroom (- total largest)
        show (fn [n] (format "%.1f%s" (/ n unit-divisor) unit-suffix))]
    (cond
      (empty? allocatables) []

      (> requested total)
      [{:severity :critical
        :component "cluster"
        :summary (format "%s requests (%s) exceed total allocatable (%s)" label (show requested) (show total))
        :hint "pods can not all be scheduled - add capacity or lower requests"}]

      (> requested headroom)
      [{:severity :warning
        :component "cluster"
        :summary (format "%s requests (%s) exceed capacity without the largest node (%s)"
                         label (show requested) (show headroom))
        :hint "the cluster can not absorb the loss of its largest node"}]

      :else [])))

(defn detect-overcommit
  [evidence]
  (let [pods (get evidence "pods")
        nodes (get evidence "nodes")]
    (concat
     (assess "CPU" 1.0 " cores"
             (pod-requests pods :cpu) (node-allocatables nodes :cpu))
     (assess "memory" 1073741824.0 "Gi"
             (pod-requests pods :memory) (node-allocatables nodes :memory)))))

(def detectives
  [{:name "overcommit"
    :description "Pod requests fit within the cluster's failure-tolerant capacity"
    :requires ["pods" "nodes"]
    :detect detect-overcommit}])
