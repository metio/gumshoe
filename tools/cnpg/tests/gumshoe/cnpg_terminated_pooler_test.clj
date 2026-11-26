;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-terminated-pooler-test
  "An evicted or otherwise terminated pooler pod stays in the API until pod
   garbage collection and serves nothing, so neither the stale-pooler check
   nor the pooler-logs collector counts it."
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.cnpg.instances :as inst]
            [gumshoe.cnpg.pooler-logs :as pooler-logs]
            [gumshoe.detectives.pooler :as pooler]))

(def ^:private cluster
  {:metadata {:namespace "db" :name "pg"}
   :spec {:instances 1}
   :status {:phase "Cluster in healthy state" :readyInstances 1}})

(def ^:private the-pooler
  {:metadata {:namespace "db" :name "pg-rw"} :spec {:cluster {:name "pg"}}})

(def ^:private instance
  {:metadata {:namespace "db" :name "pg-1" :creationTimestamp "2026-11-20T00:00:00Z"
              :labels {:cnpg.io/cluster "pg" :cnpg.io/podRole "instance" :cnpg.io/instanceName "pg-1"}
              :ownerReferences [{:kind "Cluster" :name "pg" :controller true}]}
   :status {:phase "Running"}})

(defn- pooler-pod
  [name created phase]
  {:metadata {:namespace "db" :name name :creationTimestamp created
              :labels {:cnpg.io/cluster "pg" :cnpg.io/poolerName "pg-rw" :cnpg.io/podRole "pooler"}}
   :status {:phase phase}})

(defn- stale
  [& pooler-pods]
  (vec (pooler/detect-stale-poolers {pooler/pooler-type {:items [the-pooler]}
                                     pooler/cluster-type {:items [cluster]}
                                     "pods" {:items (into [instance] pooler-pods)}})))

(deftest terminated-pods-are-recognised
  (is (inst/terminated? {:status {:phase "Failed"}}))
  (is (inst/terminated? {:status {:phase "Succeeded"}}))
  (is (not (inst/terminated? {:status {:phase "Running"}})))
  (is (not (inst/terminated? {:status {:phase "Pending"}})))
  (is (not (inst/terminated? {}))))

(deftest an-old-failed-pooler-pod-beside-fresh-running-ones-is-not-stale
  (is (= [] (stale (pooler-pod "pg-rw-new" "2026-11-21T00:00:00Z" "Running")
                   (pooler-pod "pg-rw-evicted" "2026-11-01T00:00:00Z" "Failed")))))

(deftest an-old-succeeded-pooler-pod-is-not-stale
  (is (= [] (stale (pooler-pod "pg-rw-done" "2026-11-01T00:00:00Z" "Succeeded")))))

(deftest an-old-running-pooler-pod-is-still-stale
  (let [[finding :as all] (stale (pooler-pod "pg-rw-old" "2026-11-01T00:00:00Z" "Running")
                                 (pooler-pod "pg-rw-evicted" "2026-11-01T00:00:00Z" "Failed"))]
    (is (= 1 (count all)))
    (is (= "1 of its pods predate every instance of cluster pg" (:summary finding)))))

(deftest the-log-collector-skips-terminated-pooler-pods
  (let [read (atom [])
        rows (pooler-logs/collect (constantly [(pooler-pod "pg-rw-new" "2026-11-21T00:00:00Z" "Running")
                                               (pooler-pod "pg-rw-evicted" "2026-11-01T00:00:00Z" "Failed")])
                                  (fn [namespace pod] (swap! read conj [namespace pod]) {:read? true :log ""}))]
    (is (= [["db" "pg-rw-new"]] @read))
    (is (= ["pg-rw-new"] (mapv :pod rows))))
  (is (nil? (pooler-logs/collect (constantly nil) (fn [& _] (throw (ex-info "never read" {})))))
      "a pod list that did not answer stays unread"))
