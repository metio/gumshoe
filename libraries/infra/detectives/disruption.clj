;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.detectives.disruption
  "Detectives for disruption and scaling limits: PodDisruptionBudgets that
   block drains and HorizontalPodAutoscalers pegged at their maximum."
  (:require [infra.kubectl :as kubectl]))

(defn detect-blocking-pdbs
  [evidence]
  (for [pdb (kubectl/items-of (get evidence "poddisruptionbudgets"))
        :let [allowed (-> pdb :status :disruptionsAllowed)
              expected (or (-> pdb :status :expectedPods) 0)]
        :when (and (some? allowed) (zero? allowed) (pos? expected))]
    {:severity :warning
     :component (kubectl/namespace-name-of pdb)
     :summary "PodDisruptionBudget allows zero disruptions"
     :hint "node drains will block on the pods this budget covers"}))

(defn detect-pegged-hpas
  [evidence]
  (for [hpa (kubectl/items-of (get evidence "horizontalpodautoscalers"))
        :let [maximum (-> hpa :spec :maxReplicas)
              current (-> hpa :status :currentReplicas)]
        :when (and maximum current (>= current maximum))]
    {:severity :warning
     :component (kubectl/namespace-name-of hpa)
     :summary (format "HorizontalPodAutoscaler is pegged at its maximum (%d replicas)" maximum)
     :hint "the workload cannot scale further - raise maxReplicas or investigate the load"}))

(def detectives
  [{:name "pdbs"
    :description "PodDisruptionBudgets that currently allow zero disruptions"
    :requires ["poddisruptionbudgets"]
    :detect detect-blocking-pdbs}
   {:name "hpas"
    :description "HorizontalPodAutoscalers pegged at their maximum"
    :requires ["horizontalpodautoscalers"]
    :detect detect-pegged-hpas}])
