;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.autoscaler-unread-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.autoscaler :as autoscaler]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.kubectl :as kubectl]))

(def ^:private now (java.time.Instant/parse "2026-11-20T12:00:00Z"))

(def ^:private text-status
  "Cluster-autoscaler status at 2026-11-20 11:59:55 +0000 UTC:
Cluster-wide:
  Health:      Unhealthy (ready=1 unready=9 notStarted=0 longNotStarted=0 registered=10 longUnregistered=0)
  ScaleUp:     Backoff (ready=1 registered=10)
")

(def ^:private structured-status
  "autoscalerStatus: Running\nclusterWide:\n  health:\n    status: Unhealthy\n")

(defn- configmap
  [status updated]
  {:metadata {:namespace "kube-system" :name "cluster-autoscaler-status"
              :annotations {(keyword autoscaler/last-updated-annotation) updated}}
   :data {:status status}})

(def ^:private check (first autoscaler/detectives))

(defn- evidence
  [cm]
  {"cluster-autoscaler-status" cm
   autoscaler/parsed-status-key (autoscaler/parse-status cm)
   :now now})

(deftest a-text-status-leaves-the-check-unread
  (let [e (evidence (configmap text-status "2026-11-20 11:59:55 +0000 UTC"))]
    (is (nil? (get e autoscaler/parsed-status-key)))
    (is (true? (detective/unread? check e)) "a status the check cannot read is never a pass")
    (is (= [] (vec (autoscaler/detect-autoscaler-status e))))))

(deftest a-stale-text-status-is-still-reported
  (let [e (evidence (configmap text-status "2026-11-20 09:00:00 +0000 UTC"))]
    (is (true? (detective/unread? check e)))
    (is (= ["the cluster autoscaler has stopped updating its status"]
           (mapv :summary (autoscaler/detect-autoscaler-status e))))))

(deftest a-structured-status-is-read
  (let [e (evidence (configmap structured-status "2026-11-20 11:59:55 +0000 UTC"))]
    (is (false? (detective/unread? check e)))
    (is (= ["the cluster autoscaler halts all scaling: too many nodes are unready"]
           (mapv :summary (autoscaler/detect-autoscaler-status e))))))

(deftest a-blank-status-leaves-the-check-unread
  (is (true? (detective/unread? check (evidence (configmap "" "2026-11-20 11:59:55 +0000 UTC"))))))

(deftest the-parsed-status-collector-parses-the-configmap
  (testing "the scan collects the parsed status beside the ConfigMap"
    (is (= (set ["cluster-autoscaler-status" autoscaler/parsed-status-key]) (set (:requires check))))
    (is (= [autoscaler/parsed-status-key] (:joins check))))
  (let [collect (registry/collector-for autoscaler/parsed-status-key)]
    (with-redefs [kubectl/get-namespaced-resource
                  (fn [_ _ _ _] (configmap text-status "2026-11-20 11:59:55 +0000 UTC"))]
      (is (nil? (collect "ctx" autoscaler/parsed-status-key))))
    (with-redefs [kubectl/get-namespaced-resource
                  (fn [_ _ _ _] (configmap structured-status "2026-11-20 11:59:55 +0000 UTC"))]
      (is (= "Running" (:autoscalerStatus (collect "ctx" autoscaler/parsed-status-key)))))
    (with-redefs [kubectl/get-namespaced-resource (fn [_ _ _ _] nil)]
      (is (nil? (collect "ctx" autoscaler/parsed-status-key))))))
