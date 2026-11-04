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

(defn- evidence
  [poolers pods]
  {pooler/pooler-type {:items poolers} "pods" {:items pods}})

(deftest pooler-older-than-its-cluster-instances-is-flagged-test
  (let [findings (pooler/detect-stale-poolers
                  (evidence [(pooler-of "db" "db-rw-transaction" "db")]
                            [(instance-pod "db" "db-1" "db" "2026-11-04T06:20:00Z")
                             (instance-pod "db" "db-2" "db" "2026-11-04T06:28:00Z")
                             (pooler-pod "db" "db-rw-transaction-abc" "db-rw-transaction"
                                         "2026-10-21T17:16:00Z")]))]
    (is (= #{"1 of its pods predate the instances of cluster db"} (summaries findings)))
    (is (= [:critical] (map :severity findings)))
    (is (re-find #"rollout restart deployment/db-rw-transaction" (:hint (first findings))))))

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
