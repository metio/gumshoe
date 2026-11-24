;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.strimzi
  "Detectives for Strimzi (kafka.strimzi.io): resources the operators could
   not reconcile, the Warning and ReconciliationPaused conditions they carry,
   and KafkaConnectors whose connector or tasks failed inside Kafka Connect.

   The cluster and user operators report a failure as a NotReady condition
   that is True, the topic operator as a Ready condition that is False; both
   count here. A NotReady with reason Creating is a new cluster deploying."
  (:require [gumshoe.conditions :as conditions]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def kafka-type "kafkas.kafka.strimzi.io")
(def connect-type "kafkaconnects.kafka.strimzi.io")
(def connector-type "kafkaconnectors.kafka.strimzi.io")
(def mirrormaker2-type "kafkamirrormaker2s.kafka.strimzi.io")
(def bridge-type "kafkabridges.kafka.strimzi.io")
(def nodepool-type "kafkanodepools.kafka.strimzi.io")
(def topic-type "kafkatopics.kafka.strimzi.io")
(def user-type "kafkausers.kafka.strimzi.io")
(def rebalance-type "kafkarebalances.kafka.strimzi.io")

(def resource-types
  "Each Strimzi type with its kind and how bad a failure of it is: a broken
   cluster or Connect stops traffic, a broken topic or user stops one client."
  [[kafka-type "Kafka" :critical]
   [connect-type "KafkaConnect" :critical]
   [mirrormaker2-type "KafkaMirrorMaker2" :critical]
   [bridge-type "KafkaBridge" :critical]
   [connector-type "KafkaConnector" :critical]
   [nodepool-type "KafkaNodePool" :warning]
   [topic-type "KafkaTopic" :warning]
   [user-type "KafkaUser" :warning]
   [rebalance-type "KafkaRebalance" :warning]])

(def ^:private settle-minutes 10)
(def ^:private creating-minutes 30)

(defn failure
  "The condition that says a resource failed, or nil: NotReady True, else
   Ready False."
  [resource]
  (or (when (conditions/holds? resource "NotReady") (conditions/condition resource "NotReady"))
      (when (conditions/status? resource "Ready" "False") (conditions/condition resource "Ready"))))

(defn resource-findings
  [kind severity resource now]
  (let [component (kubectl/namespace-name-of resource)
        failed (failure resource)
        age (conditions/minutes-since (:lastTransitionTime failed) now)
        grace (if (= "Creating" (:reason failed)) creating-minutes settle-minutes)]
    (concat
     (when (and failed (not (-> resource :metadata :deletionTimestamp))
                (or (nil? age) (>= age grace)))
       [(onset/since
         {:severity severity
          :component component
          :summary (format "%s is not ready (%s)" kind (or (:reason failed) "no reason"))
          :hint (str (some-> (conditions/first-line (:message failed)) (str " - "))
                     "the Strimzi operator could not reconcile it - its logs carry the full error")}
         (:lastTransitionTime failed))])
     (for [{:keys [type status reason message]} (-> resource :status :conditions)
           :when (and (= "True" status) (contains? #{"Warning" "ReconciliationPaused"} type))]
       {:severity :info
        :component component
        :summary (if (= "Warning" type)
                   (format "%s reports a warning (%s)" kind (or reason "no reason"))
                   (format "%s reconciliation is paused" kind))
        :hint (if (= "Warning" type)
                (conditions/first-line message)
                "the operator ignores changes to it until the strimzi.io/pause-reconciliation annotation is removed")}))))

(defn connector-findings
  "Findings for a KafkaConnector whose connector or tasks are FAILED in Kafka
   Connect, each with the first line of the failure's trace. The operator also
   sets NotReady on such a connector, with a message that only points at
   connectorStatus."
  [resource]
  (let [{:keys [connector tasks]} (-> resource :status :connectorStatus)
        restarts (-> resource :status :autoRestart :count)
        failed-tasks (filter #(= "FAILED" (:state %)) tasks)
        component (kubectl/namespace-name-of resource)
        restart-note (if (and restarts (pos? restarts))
                       (format " - auto-restarted %d time%s so far" restarts (if (= 1 restarts) "" "s"))
                       "")]
    (remove nil?
            [(when (= "FAILED" (:state connector))
               {:severity :critical
                :component component
                :summary "KafkaConnector's connector is FAILED"
                :hint (str (or (conditions/first-line (:trace connector)) "no trace") restart-note)})
             (when (seq failed-tasks)
               {:severity :critical
                :component component
                :summary (format "KafkaConnector has %d of %d tasks FAILED" (count failed-tasks) (count tasks))
                :hint (str "task " (:id (first failed-tasks)) ": "
                           (or (conditions/first-line (:trace (first failed-tasks))) "no trace")
                           restart-note
                           " - the strimzi.io/restart-task annotation restarts one, spec.autoRestart restarts them on its own")})])))

(defn- failed-in-connect?
  [resource]
  (let [{:keys [connector tasks]} (-> resource :status :connectorStatus)]
    (or (= "FAILED" (:state connector)) (some #(= "FAILED" (:state %)) tasks))))

(defn detect-resources
  "Every Strimzi resource the operators could not reconcile. A KafkaConnector
   that failed inside Kafka Connect is left to strimzi-connectors, which names
   the failure itself."
  [evidence]
  (let [now (:now evidence)]
    (for [[type kind severity] resource-types
          resource (kubectl/items-of (get evidence type))
          finding (resource-findings kind severity resource now)
          :when (not (and (= connector-type type) (failed-in-connect? resource)
                          (not= :info (:severity finding))))]
      finding)))

(defn detect-connectors
  [evidence]
  (mapcat connector-findings (kubectl/items-of (get evidence connector-type))))

(def detectives
  [{:name "strimzi-resources"
    :description "Strimzi resources the operators could not reconcile, and their warnings"
    :requires (mapv first resource-types)
    :detect detect-resources}
   {:name "strimzi-connectors"
    :description "KafkaConnectors whose connector or tasks are FAILED in Kafka Connect"
    :requires [connector-type]
    :detect detect-connectors}])
