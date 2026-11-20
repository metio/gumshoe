;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.duplicate-env-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.detectives.duplicate-env :as dup]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.subject :as subject]))

(defn- container [name & env]
  {:name name :image "x:1" :env (vec (for [[k v] (partition 2 env)] {:name k :value v}))})

(defn- deployment [name & containers]
  {:metadata {:namespace "web" :name name} :spec {:template {:spec {:containers (vec containers)}}}})

(deftest finds-names-defined-twice
  (is (= [{:name "FOO" :definitions [{:name "FOO" :value "a"} {:name "FOO" :value "b"}]}]
         (dup/duplicates (container "main" "FOO" "a" "BAR" "x" "FOO" "b")))))

(deftest a-clean-container-has-none
  (is (empty? (dup/duplicates (container "main" "FOO" "a" "BAR" "b"))))
  (is (empty? (dup/duplicates {:name "main"}))))

(deftest one-finding-per-workload-naming-each-duplicate
  (let [[finding & more] (dup/detect-duplicate-env
                          {"deployments" {:items [(deployment "shop"
                                                              (container "main" "FOO" "a" "FOO" "b" "BAR" "x" "BAR" "x" "BAR" "x")
                                                              (container "side" "OK" "1"))]}})]
    (is (nil? more))
    (is (= :warning (:severity finding)))
    (is (= "web/shop" (:component finding)))
    (is (= (subject/subject "Deployment" "web" "shop") (:subject finding)))
    (is (= (str "Deployment web/shop: main: FOO defined 2 times, with different values; "
                "main: BAR defined 3 times, all alike - the container sees only the last definition, and an apply may drop either; keep one")
           (:hint finding)))))

(deftest value-from-counts-as-a-different-value
  (let [c {:name "main" :env [{:name "T" :value "x"} {:name "T" :valueFrom {:secretKeyRef {:name "s" :key "t"}}}]}
        [finding] (dup/detect-duplicate-env {"statefulsets" {:items [{:metadata {:namespace "db" :name "pg"}
                                                                       :spec {:template {:spec {:containers [c]}}}}]}})]
    (is (re-find #"T defined 2 times, with different values" (:hint finding)))
    (is (= "StatefulSet" (:kind (:subject finding))))))

(deftest init-containers-and-cronjobs
  (let [spec {:initContainers [(container "init" "A" "1" "A" "2")] :containers [(container "main")]}
        [finding] (dup/detect-duplicate-env
                   {"cronjobs" {:items [{:metadata {:namespace "ops" :name "nightly"}
                                         :spec {:jobTemplate {:spec {:template {:spec spec}}}}}]}})]
    (is (= (subject/subject "CronJob" "ops" "nightly") (:subject finding)))
    (is (re-find #"^CronJob ops/nightly: init: A defined 2 times" (:hint finding)))))

(deftest daemonsets-and-clean-workloads
  (is (empty? (dup/detect-duplicate-env {"daemonsets" {:items [(deployment "agent" (container "a" "X" "1"))]}
                                        "deployments" {:items []}}))))

(deftest joins-the-workloads-scope
  (is (some #(= "duplicate-env" (:name %)) (registry/for-scope :workloads))))
