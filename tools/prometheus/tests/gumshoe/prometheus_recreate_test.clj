;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.prometheus-recreate-test
  "The post-checks of a Prometheus resize hold only once prometheus-operator
   recreated the StatefulSet the resize deleted with --cascade=orphan: the
   orphaned pods keep running whether or not it does."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.prometheus :as prometheus]
            [gumshoe.shell :as shell]
            [gumshoe.verify :as verify]))

(def ^:private prometheus
  {:metadata {:namespace "monitoring" :name "k8s"}
   :spec {:replicas 2 :paused false}})

(defn- statefulset
  [& {:keys [uid size ready observed generation template]
      :or {uid "uid-new" size "20Gi" ready 2 observed 1 generation 1 template "prometheus-k8s-db"}}]
  {:metadata {:namespace "monitoring" :name "prometheus-k8s" :uid uid :generation generation}
   :spec {:replicas 2
          :volumeClaimTemplates [{:metadata {:name template}
                                  :spec {:resources {:requests {:storage size}}}}]}
   :status (cond-> {:observedGeneration observed :replicas 2}
             ready (assoc :readyReplicas ready))})

(defn- pod
  [phase & [owner-uid]]
  {:metadata (cond-> {:namespace "monitoring" :name "prometheus-k8s-0"}
               owner-uid (assoc :ownerReferences [{:kind "StatefulSet" :name "prometheus-k8s"
                                                   :uid owner-uid :controller true}]))
   :status {:phase phase}})

(defn- cluster
  "A shell/execute that answers the Prometheus, the StatefulSet listing and
   the pods."
  [{:keys [sets pods prom]}]
  (fn [& argv]
    (let [argv (vec (filter string? argv))
          ok (fn [x] {:exit 0 :out (json/generate-string x) :err ""})]
      (cond
        (some #{"statefulsets"} argv) (ok {:items (vec sets)})
        (some #{"pods"} argv) (ok {:items (vec pods)})
        (some #(str/starts-with? % "prometheuses") argv) (ok (or prom prometheus))
        :else {:exit 1 :out "" :err "the test cluster does not answer this"}))))

(defn- verdicts
  "Each non-soft post-check attempted once against the cluster."
  [state]
  (with-redefs [shell/execute (cluster state)]
    (mapv #(verify/attempt (:check %))
          (rest (prometheus/resize-checks "ctx" "cluster" "monitoring" "k8s" "20Gi" [{:pvc "prometheus-k8s-db-prometheus-k8s-0"}])))))

(deftest orphaned-running-pods-without-a-statefulset-are-not-verified
  (is (= [false false] (verdicts {:sets [] :pods [(pod "Running")]}))))

(deftest a-recreated-statefulset-that-adopted-the-pods-is-verified
  (is (= [true true] (verdicts {:sets [(statefulset)] :pods [(pod "Running" "uid-new")]}))))

(deftest pods-still-owned-by-the-deleted-statefulset-are-not-adopted
  (is (false? (prometheus/pods-adopted? {:items [(pod "Running" "uid-old")]} {:items [(statefulset)]}))))

(deftest a-pending-adopted-pod-is-not-running
  (is (false? (prometheus/pods-adopted? {:items [(pod "Pending" "uid-new")]} {:items [(statefulset)]}))))

(deftest no-pods-are-not-running
  (is (false? (prometheus/pods-adopted? {:items []} {:items [(statefulset)]}))))

(deftest the-statefulset-must-carry-the-new-size-and-be-ready-and-current
  (testing "the old template size"
    (is (false? (prometheus/recreated? prometheus {:items [(statefulset :size "10Gi")]} "20Gi"))))
  (testing "a template named otherwise"
    (is (false? (prometheus/recreated? prometheus {:items [(statefulset :template "other")]} "20Gi"))))
  (testing "a replica not ready, readyReplicas omitted as the API server omits zero"
    (is (false? (prometheus/recreated? prometheus {:items [(statefulset :ready 1)]} "20Gi")))
    (is (false? (prometheus/recreated? prometheus {:items [(statefulset :ready nil)]} "20Gi"))))
  (testing "a generation the controller has not observed"
    (is (false? (prometheus/recreated? prometheus {:items [(statefulset :generation 2 :observed 1)]} "20Gi"))))
  (testing "the same size written in another unit"
    (is (true? (prometheus/recreated? prometheus {:items [(statefulset :size "20480Mi")]} "20Gi"))))
  (testing "every shard must be resized"
    (is (false? (prometheus/recreated? prometheus {:items [(statefulset) (statefulset :uid "uid-shard-1" :size "10Gi")]} "20Gi")))))

(deftest a-prometheus-still-paused-or-unread-is-not-recreated
  (is (false? (prometheus/recreated? (assoc-in prometheus [:spec :paused] true) {:items [(statefulset)]} "20Gi")))
  (is (false? (prometheus/recreated? nil {:items [(statefulset)]} "20Gi"))))

(deftest the-claim-template-name-follows-the-prometheus
  (let [named (assoc-in prometheus [:spec :storage :volumeClaimTemplate :metadata :name] "tsdb")]
    (is (true? (prometheus/recreated? named {:items [(statefulset :template "tsdb")]} "20Gi")))
    (is (false? (prometheus/recreated? named {:items [(statefulset)]} "20Gi")))))

(deftest the-statefulsets-are-read-in-the-namespace-by-the-operator-label
  (let [seen (atom [])]
    (with-redefs [shell/execute (fn [& argv] (swap! seen conj (vec argv)) {:exit 0 :out "{\"items\":[]}" :err ""})]
      (prometheus/statefulsets-of "ctx" "monitoring" "k8s"))
    (is (some #{"--namespace=monitoring"} (first @seen)))
    (is (some #{"--selector=operator.prometheus.io/name=k8s"} (first @seen)))))

(deftest the-checks-fail-closed
  (is (= [] (verify/fail-closed-findings
             (prometheus/resize-checks "ctx" "cluster" "monitoring" "k8s" "20Gi" [{:pvc "p"}])))))
