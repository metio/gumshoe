;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pooler-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.detectives.pooler :as pooler]))

(defn- summaries [findings] (set (map :summary findings)))

(defn- pooler-of
  [namespace name cluster]
  {:metadata {:namespace namespace :name name}
   :spec {:cluster {:name cluster}}})

(defn- pooler-pod
  [namespace name pooler created & {:keys [hash ready resources]
                                    :or {hash "aaa" ready true
                                         resources {:requests {:cpu "100m"}
                                                    :limits {:memory "256Mi"}}}}]
  {:metadata {:namespace namespace
              :name name
              :creationTimestamp created
              :labels {:cnpg.io/poolerName pooler :pod-template-hash hash}}
   :spec {:containers [{:name "pgbouncer" :resources resources}]}
   :status {:conditions [{:type "Ready" :status (if ready "True" "False")}]}})

(defn- instance-pod
  [namespace name cluster created & {:keys [by-owner]}]
  {:metadata (cond-> {:namespace namespace :name name :creationTimestamp created}
               by-owner (assoc :ownerReferences [{:kind "Cluster" :name cluster}])
               (not by-owner) (assoc :labels {:cnpg.io/cluster cluster}))
   :spec {:containers [{:name "postgres" :resources {:requests {:cpu "1"}
                                                     :limits {:memory "2Gi"}}}]}})

(defn- cluster-of
  [namespace name & {:keys [phase instances ready]
                     :or {phase "Cluster in healthy state" instances 3 ready 3}}]
  {:metadata {:namespace namespace :name name}
   :spec {:instances instances}
   :status {:phase phase :readyInstances ready}})

(defn- evidence
  ([poolers pods] (evidence poolers pods [(cluster-of "db" "db")]))
  ([poolers pods clusters]
   {pooler/pooler-type {:items poolers}
    pooler/cluster-type {:items clusters}
    "pods" {:items pods}}))

(deftest pooler-older-than-every-cluster-instance-is-flagged-test
  (let [findings (pooler/detect-stale-poolers
                  (evidence [(pooler-of "db" "db-rw-transaction" "db")]
                            [(instance-pod "db" "db-1" "db" "2026-11-04T06:20:00Z")
                             (instance-pod "db" "db-2" "db" "2026-11-04T06:28:00Z")
                             (pooler-pod "db" "db-rw-transaction-abc" "db-rw-transaction"
                                         "2026-10-21T17:16:00Z")]))]
    (is (= #{"1 of its pods predate every instance of cluster db"} (summaries findings)))
    (is (= [:warning] (map :severity findings)))
    (is (re-find #"rollout restart deployment/db-rw-transaction" (:hint (first findings))))
    (is (re-find #"connect failed" (:hint (first findings)))
        "the hint distinguishes the unreachable-backend signature")))

(deftest one-replaced-instance-is-not-a-rebuild-test
  (is (empty? (pooler/detect-stale-poolers
               (evidence [(pooler-of "db" "db-rw" "db")]
                         [(instance-pod "db" "db-1" "db" "2026-10-01T00:00:00Z")
                          (instance-pod "db" "db-2" "db" "2026-11-04T06:20:00Z")
                          (pooler-pod "db" "db-rw-abc" "db-rw" "2026-10-21T17:16:00Z")])))
      "a failover or reschedule renews one instance pod and must stay quiet"))

(deftest an-unhealthy-cluster-suppresses-the-pooler-finding-test
  (let [pods [(instance-pod "db" "db-1" "db" "2026-11-04T06:20:00Z")
              (pooler-pod "db" "db-rw-abc" "db-rw" "2026-10-19T18:27:00Z")]
        poolers [(pooler-of "db" "db-rw" "db")]]
    (is (empty? (pooler/detect-stale-poolers
                 (evidence poolers pods
                           [(cluster-of "db" "db" :phase "Not enough disk space")])))
        "the cnpg detectives report the cluster; pooler age is not actionable")
    (is (empty? (pooler/detect-stale-poolers
                 (evidence poolers pods
                           [(cluster-of "db" "db" :instances 3 :ready 2)])))
        "an instance short of ready means the backend may be unreachable")
    (is (seq (pooler/detect-stale-poolers (evidence poolers pods)))
        "the same evidence on a healthy cluster is a finding")))

(deftest a-pooler-whose-cluster-is-absent-is-quiet-test
  (is (empty? (pooler/detect-stale-poolers
               (evidence [(pooler-of "db" "db-rw" "db")]
                         [(instance-pod "db" "db-1" "db" "2026-11-04T06:20:00Z")
                          (pooler-pod "db" "db-rw-abc" "db-rw" "2026-10-19T18:27:00Z")]
                         [])))))

(deftest pooler-newer-than-its-cluster-instances-is-quiet-test
  (is (empty? (pooler/detect-stale-poolers
               (evidence [(pooler-of "db" "db-rw-transaction" "db")]
                         [(instance-pod "db" "db-1" "db" "2026-11-04T06:20:00Z")
                          (pooler-pod "db" "db-rw-transaction-abc" "db-rw-transaction"
                                      "2026-11-04T12:38:00Z")])))))

(deftest instance-pods-are-recognised-by-owner-reference-test
  (is (seq (pooler/detect-stale-poolers
            (evidence [(pooler-of "db" "db-rw" "db")]
                      [(instance-pod "db" "db-1" "db" "2026-11-04T06:20:00Z" :by-owner true)
                       (pooler-pod "db" "db-rw-abc" "db-rw" "2026-10-21T00:00:00Z")])))))

(deftest another-namespaces-instances-do-not-count-test
  (is (empty? (pooler/detect-stale-poolers
               (evidence [(pooler-of "db" "db-rw" "db")]
                         [(instance-pod "other" "db-1" "db" "2026-11-04T06:20:00Z")
                          (pooler-pod "db" "db-rw-abc" "db-rw" "2026-10-21T00:00:00Z")])))))

(deftest a-cluster-with-no-instance-pods-is-quiet-test
  (is (empty? (pooler/detect-stale-poolers
               (evidence [(pooler-of "db" "db-rw" "db")]
                         [(pooler-pod "db" "db-rw-abc" "db-rw" "2026-10-21T00:00:00Z")])))))

(deftest two-ready-generations-behind-one-pooler-are-flagged-test
  (let [findings (pooler/detect-unfinished-rollouts
                  (evidence [(pooler-of "db" "db-rw" "db")]
                            [(pooler-pod "db" "db-rw-old" "db-rw" "2026-10-30T19:11:00Z"
                                         :hash "old")
                             (pooler-pod "db" "db-rw-new" "db-rw" "2026-11-04T12:38:00Z"
                                         :hash "new")]))]
    (is (= #{"2 ready pods across 2 generations"} (summaries findings)))
    (is (= [:warning] (map :severity findings)))))

(deftest a-draining-old-pod-is-not-a-second-generation-test
  (is (empty? (pooler/detect-unfinished-rollouts
               (evidence [(pooler-of "db" "db-rw" "db")]
                         [(pooler-pod "db" "db-rw-old" "db-rw" "2026-10-30T19:11:00Z"
                                      :hash "old" :ready false)
                          (pooler-pod "db" "db-rw-new" "db-rw" "2026-11-04T12:38:00Z"
                                      :hash "new")])))))

(deftest two-replicas-of-one-generation-are-quiet-test
  (is (empty? (pooler/detect-unfinished-rollouts
               (evidence [(pooler-of "db" "db-rw" "db")]
                         [(pooler-pod "db" "db-rw-a" "db-rw" "2026-11-04T12:38:00Z")
                          (pooler-pod "db" "db-rw-b" "db-rw" "2026-11-04T12:38:00Z")])))))

(deftest pooler-pods-without-resources-are-flagged-test
  (let [findings (pooler/detect-poolers-without-resources
                  (evidence [(pooler-of "db" "db-rw" "db")]
                            [(pooler-pod "db" "db-rw-a" "db-rw" "2026-11-04T12:38:00Z"
                                         :resources {})]))]
    (is (= #{"pods run without resource requests or limits"} (summaries findings)))
    (is (= [:info] (map :severity findings)))))

(deftest a-pooler-with-only-requests-is-still-flagged-test
  (is (seq (pooler/detect-poolers-without-resources
            (evidence [(pooler-of "db" "db-rw" "db")]
                      [(pooler-pod "db" "db-rw-a" "db-rw" "2026-11-04T12:38:00Z"
                                   :resources {:requests {:cpu "100m"}})])))))

(deftest a-fully-specified-pooler-is-quiet-test
  (is (empty? (pooler/detect-poolers-without-resources
               (evidence [(pooler-of "db" "db-rw" "db")]
                         [(pooler-pod "db" "db-rw-a" "db-rw" "2026-11-04T12:38:00Z")])))))
