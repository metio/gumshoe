;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.loki-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.loki :as loki]
            [gumshoe.http :as http]
            [gumshoe.loki :as loki-evidence]
            [gumshoe.shell :as shell]))

(defn- summaries [findings] (set (map :summary findings)))

(defn- microservice-workloads
  []
  [{:kind "Deployment" :name "loki-distributor" :component "distributor" :desired 3 :ready 3}
   {:kind "StatefulSet" :name "loki-ingester" :component "ingester" :desired 3 :ready 3}
   {:kind "Deployment" :name "loki-querier" :component "querier" :desired 2 :ready 2}
   {:kind "Deployment" :name "loki-query-frontend" :component "query-frontend" :desired 2 :ready 2}
   {:kind "StatefulSet" :name "loki-compactor" :component "compactor" :desired 1 :ready 1}])

(deftest loki-deployment-mode-test
  (testing "microservice components classify as microservice mode"
    (is (= :microservice (loki/deployment-mode (microservice-workloads)))))
  (testing "only read/write/backend is the retired simple-scalable mode"
    (is (= :simple-scalable
           (loki/deployment-mode [{:kind "StatefulSet" :name "loki-write" :component "write" :desired 3 :ready 3}
                                  {:kind "Deployment" :name "loki-read" :component "read" :desired 3 :ready 3}
                                  {:kind "StatefulSet" :name "loki-backend" :component "backend" :desired 3 :ready 3}]))))
  (testing "no workloads is :none"
    (is (= :none (loki/deployment-mode []))))
  (testing "the mode detective reports simple-scalable as a warning and an empty namespace as critical"
    (is (= #{"Loki is running in retired simple-scalable mode"}
           (summaries (loki/detect-deployment-mode
                       {"namespace" "loki"
                        "workloads" [{:kind "StatefulSet" :name "loki-write" :component "write" :desired 1 :ready 1}]}))))
    (is (= #{"no Loki components found in this namespace"}
           (summaries (loki/detect-deployment-mode {"namespace" "loki" "workloads" []}))))))

(deftest loki-missing-components-test
  (testing "an essential component absent in microservice mode is critical"
    (let [without-querier (remove #(= "querier" (:component %)) (microservice-workloads))]
      (is (= #{"essential Loki component is missing"}
             (summaries (loki/detect-missing-components {"workloads" without-querier}))))
      (is (= ["querier"] (map :component (loki/detect-missing-components {"workloads" without-querier}))))))
  (testing "a complete deployment reports nothing"
    (is (empty? (loki/detect-missing-components {"workloads" (microservice-workloads)}))))
  (testing "simple-scalable mode does not trigger microservice-component warnings"
    (is (empty? (loki/detect-missing-components
                 {"workloads" [{:kind "StatefulSet" :name "loki-write" :component "write" :desired 1 :ready 1}]})))))

(deftest loki-workloads-from-test
  (testing "workload records carry the component label, desired, and ready counts, scoped to the namespace"
    (let [items [{:kind "Deployment"
                  :metadata {:namespace "loki" :name "loki-distributor"
                             :labels {(keyword "app.kubernetes.io/component") "distributor"}}
                  :spec {:replicas 3} :status {:readyReplicas 2}}
                 {:kind "StatefulSet"
                  :metadata {:namespace "other" :name "loki-ingester"
                             :labels {(keyword "app.kubernetes.io/component") "ingester"}}
                  :spec {:replicas 3} :status {:readyReplicas 3}}]
          records (loki-evidence/workloads-from items "loki")]
      (is (= 1 (count records)) "workloads outside the namespace are dropped")
      (is (= {:kind "Deployment" :name "loki-distributor" :component "distributor" :desired 3 :ready 2}
             (first records)))))
  (testing "a workload with no readyReplicas status reads as zero ready"
    (is (= 0 (:ready (loki-evidence/workload-record
                      {:kind "Deployment" :metadata {:name "x"} :spec {:replicas 1}}))))))

(deftest loki-component-health-test
  (testing "a component with zero ready replicas is critical, a partially-ready one warns"
    (let [workloads [{:kind "Deployment" :name "loki-distributor" :component "distributor" :desired 3 :ready 0}
                     {:kind "StatefulSet" :name "loki-ingester" :component "ingester" :desired 3 :ready 2}
                     {:kind "Deployment" :name "loki-querier" :component "querier" :desired 2 :ready 2}]
          findings (loki/detect-component-health {"workloads" workloads})]
      (is (= :critical (:severity (first (filter #(= "distributor" (:component %)) findings)))))
      (is (= :warning (:severity (first (filter #(= "ingester" (:component %)) findings)))))
      (is (nil? (first (filter #(= "querier" (:component %)) findings))) "a fully-ready component is silent"))))

(deftest loki-rings-test
  (testing "UNHEALTHY members are critical, transitional ones warn, unreachable rings are skipped"
    (let [evidence {"rings" {"ingester" {:reachable true
                                         :json {:shards [{:id "loki-0" :state "ACTIVE"}
                                                         {:id "loki-1" :state "UNHEALTHY"}]}}
                             "distributor" {:reachable true
                                            :json {:shards [{:id "loki-2" :state "JOINING"}]}}
                             "compactor" {:reachable false :error "404"}}}
          findings (loki/detect-rings evidence)]
      (is (= #{"ring member is UNHEALTHY" "ring member is JOINING"} (summaries findings)))
      (is (= :critical (:severity (first (filter #(= "ingester/loki-1" (:component %)) findings)))))
      (is (= :warning (:severity (first (filter #(= "distributor/loki-2" (:component %)) findings)))))))
  (testing "a fully ACTIVE ring is silent"
    (is (empty? (loki/detect-rings {"rings" {"ingester" {:reachable true
                                                         :json {:shards [{:id "loki-0" :state "ACTIVE"}]}}}})))))

(defn- unread-checks
  [evidence]
  (set (map :name (filter :unread (detective/mark-unread loki/detectives evidence)))))

(defn- quietly
  [f]
  (binding [*err* (java.io.StringWriter.)] (f)))

(deftest an-unanswered-workload-read-leaves-the-checks-unread-test
  (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server"})]
    (let [evidence (quietly #(loki-evidence/collect-workloads! {:context "ctx" :namespace "loki"
                                                                 :selector "app.kubernetes.io/name=loki"}))]
      (is (nil? (get evidence "workloads")))
      (is (= #{"loki-mode" "loki-components" "loki-health" "loki-rings"} (unread-checks evidence))))))

(deftest an-answered-workload-read-is-read-test
  (with-redefs [shell/execute (fn [& _] {:exit 0 :err ""
                                         :out (json/generate-string
                                               {:items [{:kind "Deployment"
                                                         :metadata {:namespace "loki" :name "loki-distributor"
                                                                    :labels {(keyword "app.kubernetes.io/component") "distributor"}}
                                                         :spec {:replicas 1} :status {:readyReplicas 1}}]})})]
    (let [evidence (quietly #(loki-evidence/collect-workloads! {:context "ctx" :namespace "loki"
                                                                 :selector "app.kubernetes.io/name=loki"}))]
      (is (= 1 (count (get evidence "workloads"))))
      (is (= #{"loki-rings"} (unread-checks evidence))))))

(deftest rings-that-did-not-answer-with-shards-are-unread-test
  (doseq [[label response] [["unreachable" {:reachable false :error "refused"}]
                            ["404" {:reachable true :status 404 :body "404 page not found"}]
                            ["html" {:reachable true :status 200 :body "<html></html>" :json nil}]]]
    (with-redefs [http/fetch (fn [& _] response)]
      (let [evidence (quietly #(loki-evidence/collect-rings! "http://localhost:13100"))]
        (is (nil? (get evidence "rings")) label)
        (is (contains? (unread-checks evidence) "loki-rings") label)))))

(deftest answered-rings-keeps-the-rings-with-shards-test
  (let [active {:reachable true :status 200 :json {:shards [{:id "i-1" :state "ACTIVE"}]}}]
    (is (= {"ingester" active}
           (loki-evidence/answered-rings {"ingester" active
                                          "ruler" {:reachable true :status 404}
                                          "compactor" {:reachable false}})))
    (is (nil? (loki-evidence/answered-rings {"ruler" {:reachable true :status 404}})))))
