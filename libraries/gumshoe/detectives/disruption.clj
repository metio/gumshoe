;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.disruption
  "Detectives for disruption and scaling limits: PodDisruptionBudgets that
   block drains and HorizontalPodAutoscalers pegged at their maximum."
  (:require [clojure.string :as str]
            [gumshoe.conditions :as conditions]
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

(def default-hpa-grace-minutes
  "How long an HPA condition must hold False before it is critical. The HPA
   controller re-reads the scale subresource and the metrics on every sync,
   15 seconds by default, so a condition set by a metrics adapter that was
   rescheduled or missed one read clears within a few syncs once the adapter
   answers again. Ten minutes is forty syncs: long past such churn, and short
   enough that a broken metrics pipeline still turns critical well before it
   matters for a load spike that lasts."
  10)

(def hpa-grace-option
  "The book option that sets the threshold, in minutes."
  {:hpa-grace-minutes {:desc "Minutes an HPA's AbleToScale or ScalingActive condition must hold False before it is critical; a younger one is a warning"
                       :default default-hpa-grace-minutes
                       :coerce :long
                       :validate {:pred nat-int?
                                  :ex-msg (fn [{:keys [value]}]
                                            (format "--hpa-grace-minutes must be zero or more, got %s" (pr-str value)))}}})

(defn- held-minutes
  "Whole minutes condition has held False at now, nil when either instant is
   missing or unreadable. A transition after now reads as 0, so clock skew
   between the API server and the collector cannot make an age unknown."
  [condition now]
  (let [at (some-> (:lastTransitionTime condition) str (#(try (java.time.Instant/parse %) (catch Exception _ nil))))]
    (when (and at (instance? java.time.Instant now))
      (max 0 (.toMinutes (java.time.Duration/between ^java.time.Instant at ^java.time.Instant now))))))

(defn- failed-condition
  "The finding for a False condition, judged by how long it has held. Below
   grace minutes it is a warning that names its age; at or beyond it, or when
   its age cannot be read, it stays critical, so a missing timestamp never
   hides a lasting fault."
  [component condition summary hint now grace]
  (let [minutes (held-minutes condition now)
        young? (and minutes (< minutes grace))]
    (onset/since
     {:severity (if young? :warning :critical)
      :component component
      :summary (if young? (format "%s, held under %dm" summary grace) summary)
      :hint (str/join " - " (remove str/blank?
                                    [hint
                                     (cond
                                       young? (format "False for %dm, under the %dm threshold; the HPA controller retries every sync, so it may recover on its own - critical once it holds %dm"
                                                      minutes grace grace)
                                       (nil? minutes) "the condition carries no readable lastTransitionTime or the collection time is missing, so how long it has held is unknown")]))}
     (:lastTransitionTime condition))))

(defn detect-failing-hpas
  "AbleToScale or ScalingActive conditions that are False. The age of each is
   read from its lastTransitionTime at the collection time :now, against the
   threshold in the \"options\" evidence (default-hpa-grace-minutes when unset)."
  [evidence]
  (let [now (:now evidence)
        grace (or (get-in evidence ["options" :hpa-grace-minutes]) default-hpa-grace-minutes)]
    (for [hpa (kubectl/items-of (get evidence "horizontalpodautoscalers"))
          :let [conditions (conditions/by-type hpa)
                able (get conditions "AbleToScale")
                active (get conditions "ScalingActive")
                component (kubectl/namespace-name-of hpa)]
          finding [(when (= "False" (:status able))
                     (failed-condition component able
                                       (format "HorizontalPodAutoscaler cannot scale its target (%s)" (:reason able))
                                       (str (:message able) " - the target's scale subresource cannot be read or written")
                                       now grace))
                   ;; ScalingDisabled is the HPA standing down for a target scaled to 0 by hand
                   (when (and (= "False" (:status active)) (not= "ScalingDisabled" (:reason active)))
                     (failed-condition component active
                                       (format "HorizontalPodAutoscaler cannot read its metrics (%s)" (:reason active))
                                       (str/join " - " (remove nil? [(metrics-cause active) (:message active)]))
                                       now grace))]
          :when finding]
      finding)))

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
    :description "HorizontalPodAutoscalers that cannot scale or read their metrics: critical once the condition has held past the threshold, a warning before"
    :requires ["horizontalpodautoscalers"]
    :detect detect-failing-hpas}])
