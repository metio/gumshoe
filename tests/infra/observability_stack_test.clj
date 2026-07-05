;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.observability-stack-test
  (:require [clojure.test :refer [deftest is testing]]
            [infra.detectives.loki :as loki]
            [infra.detectives.thanos :as thanos]
            [infra.http :as http]
            [infra.loki :as loki-evidence]))

(defn- summaries
  [findings]
  (set (map :summary findings)))

(deftest http-ok-test
  (is (http/ok? {:reachable true :status 200}))
  (is (http/ok? {:reachable true :status 204}))
  (is (not (http/ok? {:reachable true :status 503})))
  (is (not (http/ok? {:reachable false :error "connection refused"}))))

;; ---------------------------------------------------------------------------
;; thanos

(deftest thanos-readiness-test
  (is (= #{"Thanos Query is unreachable"}
         (summaries (thanos/detect-readiness {"url" "http://localhost:19090"
                                              "ready" {:reachable false :error "refused"}}))))
  (is (= #{"readiness endpoint returned HTTP 503"}
         (summaries (thanos/detect-readiness {"url" "http://localhost:19090"
                                              "ready" {:reachable true :status 503}}))))
  (is (empty? (thanos/detect-readiness {"url" "http://localhost:19090"
                                        "ready" {:reachable true :status 200}}))))

(deftest thanos-stores-test
  (testing "no connected stores is critical"
    (is (= #{"Thanos Query has no connected stores"}
           (summaries (thanos/detect-stores {"url" "http://localhost:19090"
                                             "stores" {:reachable true :json {:data {}}}})))))
  (testing "a store endpoint with a lastError is critical, healthy ones are silent"
    (let [evidence {"url" "http://localhost:19090"
                    "stores" {:reachable true
                              :json {:data {:sidecar [{:name "1.2.3.4:10901" :lastError nil}]
                                            :store [{:name "5.6.7.8:10901"
                                                     :lastError "rpc error: connection timeout"}]}}}}
          findings (thanos/detect-stores evidence)]
      (is (= #{"store store endpoint reports an error"} (summaries findings)))
      (is (= "5.6.7.8:10901" (:component (first findings)))))))

(deftest thanos-rules-test
  (is (= #{"rule fails to evaluate"}
         (summaries (thanos/detect-rules
                     {"rules" {:json {:data {:groups [{:name "recording"
                                                       :rules [{:name "job:up" :health "ok"}
                                                               {:name "job:errs" :health "err"
                                                                :lastError "parse error"}]}]}}}}))))
  (is (empty? (thanos/detect-rules
               {"rules" {:json {:data {:groups [{:name "g" :rules [{:name "r" :health "ok"}]}]}}}}))))

;; ---------------------------------------------------------------------------
;; loki

(defn- microservice-workloads
  "A healthy full microservice deployment, for tweaking per test."
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
