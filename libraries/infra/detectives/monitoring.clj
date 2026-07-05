;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.detectives.monitoring
  "Detectives for the monitoring stack as managed by the prometheus-operator:
   Prometheus, Alertmanager, and ThanosRuler instances that are not available
   or reconciling."
  (:require [infra.kubectl :as kubectl]))

(def prometheus-type "prometheuses.monitoring.coreos.com")
(def alertmanager-type "alertmanagers.monitoring.coreos.com")
(def thanosruler-type "thanosrulers.monitoring.coreos.com")

(defn- condition
  [resource type]
  (first (filter #(= type (:type %)) (-> resource :status :conditions))))

(defn operator-findings
  [resources kind]
  (concat
   (for [resource resources
         :let [available (condition resource "Available")]
         :when (contains? #{"False" "Degraded"} (:status available))]
     {:severity :critical
      :component (kubectl/namespace-name-of resource)
      :summary (format "%s is not Available (%s)" kind (or (:reason available) (:status available)))
      :hint (:message available)})
   (for [resource resources
         :let [reconciled (condition resource "Reconciled")]
         :when (= "False" (:status reconciled))]
     {:severity :warning
      :component (kubectl/namespace-name-of resource)
      :summary (format "%s is not Reconciled (%s)" kind (or (:reason reconciled) "unknown"))
      :hint (:message reconciled)})
   (for [resource resources
         :let [paused (-> resource :spec :paused)]
         :when (true? paused)]
     {:severity :info
      :component (kubectl/namespace-name-of resource)
      :summary (format "%s is paused - the operator does not reconcile it" kind)})))

(defn detect-prometheus-problems
  [evidence]
  (operator-findings (kubectl/items-of (get evidence prometheus-type)) "Prometheus"))

(defn detect-alertmanager-problems
  [evidence]
  (operator-findings (kubectl/items-of (get evidence alertmanager-type)) "Alertmanager"))

(defn detect-thanosruler-problems
  [evidence]
  (operator-findings (kubectl/items-of (get evidence thanosruler-type)) "ThanosRuler"))

(def detectives
  [{:name "prometheus"
    :description "Prometheus instances that are unavailable, unreconciled, or paused"
    :requires [prometheus-type]
    :detect detect-prometheus-problems}
   {:name "alertmanager"
    :description "Alertmanager instances that are unavailable, unreconciled, or paused"
    :requires [alertmanager-type]
    :detect detect-alertmanager-problems}
   {:name "thanosruler"
    :description "ThanosRuler instances that are unavailable, unreconciled, or paused"
    :requires [thanosruler-type]
    :detect detect-thanosruler-problems}])
