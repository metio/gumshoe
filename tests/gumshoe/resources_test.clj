;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.resources-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.capacity :as capacity]
            [gumshoe.detectives.controlplane :as controlplane]
            [gumshoe.detectives.quotas :as quotas]
            [gumshoe.quantity :as quantity]))

(defn- summaries
  [findings]
  (set (map :summary findings)))

(deftest quantity-parsing-test
  (testing "cpu quantities become cores"
    (is (= 0.1 (quantity/quantity->number "100m")))
    (is (= 2.0 (quantity/quantity->number "2")))
    (is (= 1.5 (quantity/quantity->number "1500m"))))
  (testing "memory quantities become bytes, binary and decimal"
    (is (= 134217728.0 (quantity/quantity->number "128Mi")))
    (is (= 1073741824.0 (quantity/quantity->number "1Gi")))
    (is (= 5.0E8 (quantity/quantity->number "500M"))))
  (testing "plain counts and unparseable input"
    (is (= 10.0 (quantity/quantity->number "10")))
    (is (nil? (quantity/quantity->number "notaquantity")))
    (is (nil? (quantity/quantity->number nil))))
  (testing "sum skips unparseable, ratio guards zero and nil"
    (is (= 2.6 (quantity/sum ["100m" "2500m" "bad"])))
    (is (= 3/4 (rationalize (quantity/ratio "1500m" "2"))))
    (is (nil? (quantity/ratio "1" "0")))
    (is (nil? (quantity/ratio "1" "bad")))))

(deftest control-plane-detective-test
  (let [evidence {"pods" {:items [{:metadata {:namespace "kube-system" :name "kube-apiserver-cp1"
                                              :labels {:component "kube-apiserver"}}
                                   :status {:phase "Running"
                                            :conditions [{:type "Ready" :status "True"}]}}
                                  {:metadata {:namespace "kube-system" :name "kube-scheduler-cp1"
                                              :labels {:component "kube-scheduler"}}
                                   :status {:phase "Running"
                                            :conditions [{:type "Ready" :status "False"}]}}
                                  {:metadata {:namespace "kube-system" :name "etcd-cp1"
                                              :labels {:component "etcd"}}
                                   :status {:phase "CrashLoopBackOff"}}
                                  {:metadata {:namespace "app" :name "web"
                                              :labels {:component "web"}}
                                   :status {:phase "Running"
                                            :conditions [{:type "Ready" :status "True"}]}}]}}
        findings (controlplane/detect-control-plane evidence)]
    (testing "only unready control-plane components are reported, app pods are ignored"
      (is (= #{"control-plane scheduler is not ready (phase Running)"
               "control-plane etcd is not ready (phase CrashLoopBackOff)"}
             (summaries findings)))
      (is (every? #(= :critical (:severity %)) findings)))))

(deftest quota-detective-test
  (let [evidence {"resourcequotas"
                  {:items [{:metadata {:namespace "team-a" :name "compute"}
                            :status {:hard {:requests.cpu "10" :requests.memory "20Gi"}
                                     :used {:requests.cpu "10" :requests.memory "12Gi"}}}
                           {:metadata {:namespace "team-b" :name "compute"}
                            :status {:hard {:pods "50"}
                                     :used {:pods "47"}}}
                           {:metadata {:namespace "team-c" :name "roomy"}
                            :status {:hard {:requests.cpu "10"}
                                     :used {:requests.cpu "2"}}}]}}
        findings (quotas/detect-quota-pressure evidence)]
    (testing "exhausted quotas are critical, near-full warn, roomy ones are silent"
      (is (= #{"quota is 100% used (10 of 10)"
               "quota is 94% used (47 of 50)"}
             (summaries findings)))
      (is (= :critical (:severity (first (filter #(re-find #"cpu" (:component %)) findings)))))
      (is (= :warning (:severity (first (filter #(re-find #"pods" (:component %)) findings))))))))

(deftest overcommit-detective-test
  (testing "requests over total capacity are critical"
    (is (= #{"CPU requests (10.0 cores) exceed total allocatable (8.0 cores)"}
           (summaries (filter #(re-find #"CPU" (:summary %))
                              (capacity/detect-overcommit
                               {"nodes" {:items [{:status {:allocatable {:cpu "4" :memory "8Gi"}}}
                                                 {:status {:allocatable {:cpu "4" :memory "8Gi"}}}]}
                               "pods" {:items [{:spec {:containers [{:resources {:requests {:cpu "10" :memory "1Gi"}}}]}}]}}))))))
  (testing "requests over capacity-minus-largest-node warn about node-failure tolerance"
    (let [findings (capacity/detect-overcommit
                    {"nodes" {:items [{:status {:allocatable {:cpu "4" :memory "8Gi"}}}
                                      {:status {:allocatable {:cpu "4" :memory "8Gi"}}}]}
                     "pods" {:items [{:spec {:containers [{:resources {:requests {:cpu "6" :memory "1Gi"}}}]}}]}})]
      (is (contains? (summaries findings)
                     "CPU requests (6.0 cores) exceed capacity without the largest node (4.0 cores)"))
      (is (= :warning (:severity (first (filter #(re-find #"CPU" (:summary %)) findings)))))))
  (testing "a comfortably-provisioned cluster is silent"
    (is (empty? (capacity/detect-overcommit
                 {"nodes" {:items [{:status {:allocatable {:cpu "8" :memory "16Gi"}}}
                                   {:status {:allocatable {:cpu "8" :memory "16Gi"}}}]}
                  "pods" {:items [{:spec {:containers [{:resources {:requests {:cpu "2" :memory "4Gi"}}}]}}]}})))))
