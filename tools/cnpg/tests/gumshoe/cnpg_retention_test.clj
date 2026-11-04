;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-retention-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.cnpg.retention :as retention]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.detectives.cnpg-retention :as detective]))

(defn- cluster
  [name size params]
  {:metadata {:namespace name :name name}
   :spec (cond-> {:storage {:size size}}
           params (assoc :postgresql {:parameters params}))})

(defn- findings
  [& clusters]
  (detective/detect-unbounded-retention {cnpg/cluster-type {:items clusters}}))

(deftest postgres-size-units-are-binary-and-its-own-test
  (is (= 1073741824.0 (retention/parse-size "1GB")))
  (is (= 10737418240.0 (retention/parse-size "10GB")))
  (is (= 1048576.0 (retention/parse-size "1MB")))
  (is (= 1024.0 (retention/parse-size "1kB")))
  (is (= 1024.0 (retention/parse-size "1KB")))
  (is (= 2097152.0 (retention/parse-size "2"))
      "a bare number for this setting is megabytes"))

(deftest minus-one-is-unbounded-not-a-size-test
  (is (= :unbounded (retention/parse-size "-1"))))

(deftest an-unreadable-size-has-no-value-test
  (is (nil? (retention/parse-size "")))
  (is (nil? (retention/parse-size nil)))
  (is (nil? (retention/parse-size "lots")))
  (is (nil? (retention/parse-size "10Gi")) "a Kubernetes quantity is not a postgres size"))

(deftest a-cluster-with-no-parameter-is-exposed-test
  (let [[finding :as all] (findings (cluster "a" "40Gi" nil))]
    (is (= 1 (count all)))
    (is (= "a/a" (:component finding)))
    (is (re-find #"no max_slot_wal_keep_size is set" (:summary finding)))))

(deftest a-cluster-with-parameters-but-not-this-one-is-exposed-test
  (is (re-find #"no max_slot_wal_keep_size is set"
               (:summary (first (findings (cluster "a" "40Gi" {:max_connections "200"})))))))

(deftest an-explicit-minus-one-is-reported-as-the-default-it-is-test
  (let [finding (first (findings (cluster "a" "40Gi" {:max_slot_wal_keep_size "-1"})))]
    (is (re-find #"is -1, so slot retention is unbounded" (:summary finding)))))

(deftest a-cap-worth-half-the-volume-or-more-is-no-floor-test
  (let [finding (first (findings (cluster "a" "40Gi" {:max_slot_wal_keep_size "30GB"})))]
    (is (re-find #"30.0GiB of a 40.0GiB volume, which is no floor" (:summary finding)))))

(deftest an-unparseable-cap-is-reported-rather-than-assumed-safe-test
  (let [finding (first (findings (cluster "a" "40Gi" {:max_slot_wal_keep_size "10Gi"})))]
    (is (re-find #"set to 10Gi, which does not parse" (:summary finding)))))

(deftest a-cap-well-under-the-volume-is-a-floor-test
  (is (empty? (findings (cluster "a" "40Gi" {:max_slot_wal_keep_size "10GB"}))))
  (is (= :bounded (:state (retention/assess (cluster "a" "40Gi" {:max_slot_wal_keep_size "10GB"}))))))

(deftest a-cap-without-a-readable-volume-is-taken-at-face-value-test
  (is (empty? (findings (cluster "a" nil {:max_slot_wal_keep_size "10GB"})))
      "an unreadable volume cannot condemn a cap that parses"))

(deftest every-exposed-cluster-is-reported-test
  (is (= 2 (count (findings (cluster "a" "40Gi" nil)
                            (cluster "b" "40Gi" {:max_slot_wal_keep_size "10GB"})
                            (cluster "c" "40Gi" {:max_slot_wal_keep_size "-1"}))))))

(deftest the-fix-is-named-in-every-hint-test
  (doseq [c [(cluster "a" "40Gi" nil)
             (cluster "b" "40Gi" {:max_slot_wal_keep_size "-1"})
             (cluster "c" "40Gi" {:max_slot_wal_keep_size "30GB"})]]
    (is (re-find #"set max_slot_wal_keep_size" (:hint (first (findings c)))))))
