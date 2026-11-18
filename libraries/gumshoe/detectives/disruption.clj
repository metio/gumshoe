;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.disruption
  "Detectives for disruption and scaling limits: PodDisruptionBudgets that
   block drains and HorizontalPodAutoscalers pegged at their maximum."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

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

(defn- metrics-cause
  "What a failed metrics read means, from the condition's reason and message."
  [{:keys [reason message]}]
  (let [message (str message)]
    (cond
      (re-find #"missing request for (\S+)" message)
      (format "a container of the target has no %s request, so its utilisation cannot be computed - set the request on every container"
              (second (re-find #"missing request for (\S+)" message)))
      (= "FailedGetResourceMetric" reason) "metrics-server does not answer for the target's pods - check the metrics-server pods and the APIService v1beta1.metrics.k8s.io"
      (contains? #{"FailedGetPodsMetric" "FailedGetObjectMetric"} reason) "the custom metrics adapter does not serve the metric - check the custom.metrics.k8s.io APIService and its adapter"
      (= "FailedGetExternalMetric" reason) "the external metrics adapter does not serve the metric - check the external.metrics.k8s.io APIService and its adapter"
      :else nil)))

(defn detect-failing-hpas
  [evidence]
  (for [hpa (kubectl/items-of (get evidence "horizontalpodautoscalers"))
        :let [conditions (into {} (map (juxt :type identity)) (-> hpa :status :conditions))
              able (get conditions "AbleToScale")
              active (get conditions "ScalingActive")
              component (kubectl/namespace-name-of hpa)]
        finding [(when (= "False" (:status able))
                   (onset/since
                    {:severity :critical
                     :component component
                     :summary (format "HorizontalPodAutoscaler cannot scale its target (%s)" (:reason able))
                     :hint (str (:message able) " - the target's scale subresource cannot be read or written")}
                    (:lastTransitionTime able)))
                 ;; ScalingDisabled is the HPA standing down for a target scaled to 0 by hand
                 (when (and (= "False" (:status active)) (not= "ScalingDisabled" (:reason active)))
                   (onset/since
                    {:severity :critical
                     :component component
                     :summary (format "HorizontalPodAutoscaler cannot read its metrics (%s)" (:reason active))
                     :hint (str/join " - " (remove nil? [(metrics-cause active) (:message active)]))}
                    (:lastTransitionTime active)))]
        :when finding]
    finding))

(def detectives
  [{:name "pdbs"
    :description "PodDisruptionBudgets that currently allow zero disruptions"
    :requires ["poddisruptionbudgets"]
    :detect detect-blocking-pdbs}
   {:name "hpas"
    :description "HorizontalPodAutoscalers pegged at their maximum"
    :requires ["horizontalpodautoscalers"]
    :detect detect-pegged-hpas}
   {:name "hpa-failures"
    :description "HorizontalPodAutoscalers that cannot scale or read their metrics"
    :requires ["horizontalpodautoscalers"]
    :detect detect-failing-hpas}])
