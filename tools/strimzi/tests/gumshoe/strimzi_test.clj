;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.strimzi-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.detectives.strimzi :as strimzi]
            [gumshoe.tools.strimzi]))

(def ^:private now (java.time.Instant/parse "2026-11-19T12:00:00Z"))

(defn- condition [type status & {:keys [reason message at] :or {at "2026-11-19T11:00:00Z"}}]
  (cond-> {:type type :status status :lastTransitionTime at}
    reason (assoc :reason reason)
    message (assoc :message message)))

(defn- resource [name & {:keys [conditions connector-status auto-restart deleting]}]
  {:metadata (cond-> {:namespace "kafka" :name name} deleting (assoc :deletionTimestamp "2026-11-19T11:00:00Z"))
   :status (cond-> {:conditions conditions}
             connector-status (assoc :connectorStatus connector-status)
             auto-restart (assoc :autoRestart auto-restart))})

(defn- findings [type & resources]
  (detective/run-detectives strimzi/detectives {:now now type {:items (vec resources)}}))

(deftest both-failure-conventions-count
  (is (= (condition "NotReady" "True" :reason "TimeoutException")
         (strimzi/failure (resource "a" :conditions [(condition "NotReady" "True" :reason "TimeoutException")]))))
  (is (= "KafkaError" (:reason (strimzi/failure (resource "a" :conditions [(condition "Ready" "False" :reason "KafkaError")])))))
  (is (nil? (strimzi/failure (resource "a" :conditions [(condition "Ready" "True")]))))
  (is (nil? (strimzi/failure (resource "a" :conditions [(condition "NotReady" "False")]))))
  (is (nil? (strimzi/failure (resource "a" :conditions [(condition "Warning" "True")])))))

(deftest a-kafka-the-operator-cannot-reconcile
  (let [[finding & more] (findings strimzi/kafka-type
                                   (resource "events" :conditions [(condition "NotReady" "True" :reason "TimeoutException"
                                                                              :message "Exceeded timeout of 300000ms while waiting for Pods resource events-broker-0\nat ...")]))]
    (is (empty? more))
    (is (= :critical (:severity finding)))
    (is (= "kafka/events" (:component finding)))
    (is (= "Kafka is not ready (TimeoutException)" (:summary finding)))
    (is (= "Exceeded timeout of 300000ms while waiting for Pods resource events-broker-0 - the Strimzi operator could not reconcile it - its logs carry the full error"
           (:hint finding)))
    (is (= "2026-11-19T11:00:00Z" (:since finding)))))

(deftest severity-by-kind
  (doseq [[type severity] [[strimzi/kafka-type :critical] [strimzi/connect-type :critical] [strimzi/mirrormaker2-type :critical]
                           [strimzi/bridge-type :critical] [strimzi/nodepool-type :warning] [strimzi/topic-type :warning]
                           [strimzi/user-type :warning] [strimzi/rebalance-type :warning]]]
    (is (= [severity] (map :severity (findings type (resource "x" :conditions [(condition "NotReady" "True" :reason "E")])))) type)))

(deftest a-topic-the-topic-operator-rejected
  (is (= ["KafkaTopic is not ready (KafkaError)"]
         (map :summary (findings strimzi/topic-type
                                 (resource "orders" :conditions [(condition "Ready" "False" :reason "KafkaError" :message "Replication factor: 3 larger than available brokers: 1")]))))))

(deftest grace
  (testing "a fresh failure gets ten minutes"
    (is (empty? (findings strimzi/kafka-type (resource "k" :conditions [(condition "NotReady" "True" :reason "E" :at "2026-11-19T11:55:00Z")]))))
    (is (seq (findings strimzi/kafka-type (resource "k" :conditions [(condition "NotReady" "True" :reason "E" :at "2026-11-19T11:50:00Z")])))))
  (testing "a new cluster gets thirty to deploy"
    (is (empty? (findings strimzi/kafka-type (resource "k" :conditions [(condition "NotReady" "True" :reason "Creating" :at "2026-11-19T11:35:00Z")]))))
    (is (= ["Kafka is not ready (Creating)"]
           (map :summary (findings strimzi/kafka-type (resource "k" :conditions [(condition "NotReady" "True" :reason "Creating" :at "2026-11-19T11:30:00Z")]))))))
  (testing "without a transition time it reports"
    (is (seq (findings strimzi/kafka-type (resource "k" :conditions [(dissoc (condition "NotReady" "True") :lastTransitionTime)])))))
  (testing "a resource being deleted is left alone"
    (is (empty? (findings strimzi/kafka-type (resource "k" :deleting true :conditions [(condition "NotReady" "True" :reason "E")]))))))

(deftest warnings-and-pauses
  (let [result (findings strimzi/kafka-type
                         (resource "k" :conditions [(condition "Ready" "True")
                                                    (condition "Warning" "True" :reason "DeprecatedFields" :message "In API version kafka.strimzi.io/v1beta2 the inter.broker.protocol.version option is deprecated")
                                                    (condition "ReconciliationPaused" "True")]))]
    (is (= [[:info "Kafka reports a warning (DeprecatedFields)"] [:info "Kafka reconciliation is paused"]]
           (map (juxt :severity :summary) result)))
    (is (str/starts-with? (:hint (first result)) "In API version"))
    (is (str/includes? (:hint (second result)) "strimzi.io/pause-reconciliation"))))

(deftest connectors-failed-in-kafka-connect
  (let [not-ready (condition "NotReady" "True" :reason "Throwable" :message "The following tasks have failed: 1, see connectorStatus for more details.")
        connector (resource "sink" :conditions [not-ready]
                            :auto-restart {:count 3 :connectorName "sink"}
                            :connector-status {:name "sink"
                                               :connector {:state "RUNNING" :worker_id "10.0.0.1:8083"}
                                               :tasks [{:id 0 :state "RUNNING" :worker_id "w"}
                                                       {:id 1 :state "FAILED" :worker_id "w"
                                                        :trace "org.apache.kafka.connect.errors.ConnectException: table orders does not exist\n\tat ..."}]})
        result (findings strimzi/connector-type connector)]
    (is (= [[:critical "KafkaConnector has 1 of 2 tasks FAILED"]] (map (juxt :severity :summary) result))
        "the operator's NotReady carries nothing the task finding lacks")
    (is (str/starts-with? (:hint (first result))
                          "task 1: org.apache.kafka.connect.errors.ConnectException: table orders does not exist - auto-restarted 3 times so far"))
    (is (str/includes? (:hint (first result)) "strimzi.io/restart-task")))
  (testing "a failed connector"
    (let [[finding] (strimzi/connector-findings (resource "src" :connector-status {:connector {:state "FAILED" :trace "java.lang.NullPointerException\n\tat x"}
                                                                                  :tasks []}))]
      (is (= "KafkaConnector's connector is FAILED" (:summary finding)))
      (is (= "java.lang.NullPointerException" (:hint finding)))))
  (testing "a failure without a trace"
    (is (= "task 0: no trace" (subs (:hint (second (strimzi/connector-findings
                                                   (resource "x" :connector-status {:connector {:state "FAILED"}
                                                                                    :tasks [{:id 0 :state "FAILED"}]}))))
                                    0 16))))
  (testing "running, paused and unknown connectors say nothing"
    (doseq [state ["RUNNING" "PAUSED" "STOPPED" "UNASSIGNED" "RESTARTING"]]
      (is (empty? (strimzi/connector-findings (resource "x" :connector-status {:connector {:state state} :tasks [{:id 0 :state state}]}))) state))
    (is (empty? (strimzi/connector-findings (resource "x")))))
  (testing "a connector the operator cannot reach is still reported as not ready"
    (is (= ["KafkaConnector is not ready (ConnectRestException)"]
           (map :summary (findings strimzi/connector-type
                                   (resource "x" :conditions [(condition "NotReady" "True" :reason "ConnectRestException")])))))))

(deftest the-package-fills-the-messaging-scope
  (is (= (set (map :name strimzi/detectives)) (set (map :name (registry/for-scope :messaging)))))
  (is (empty? (detective/run-detectives strimzi/detectives {:now now})) "a cluster without Strimzi"))
