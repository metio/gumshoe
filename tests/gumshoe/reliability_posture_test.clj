;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.reliability-posture-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.posture :as posture]
            [gumshoe.detectives.registry :as registry]))

(defn- deployment [replicas & {:keys [strategy name] :or {name "web"}}]
  {:metadata {:namespace "shop" :name name}
   :spec (cond-> {:replicas replicas :template {:metadata {:labels {:app name}} :spec {:containers [{:name "app"}]}}}
           strategy (assoc :strategy strategy))})

(defn- rolling [surge unavailable]
  {:type "RollingUpdate" :rollingUpdate (cond-> {} (some? surge) (assoc :maxSurge surge) (some? unavailable) (assoc :maxUnavailable unavailable))})

(deftest rolling-update-bounds-resolve-like-the-controller
  (is (= {:surge 1 :unavailable 0} (posture/rolling-update-bounds (deployment 3))) "25% of 3: surge rounds up, unavailable down")
  (is (= {:surge 3 :unavailable 2} (posture/rolling-update-bounds (deployment 10 :strategy (rolling "25%" "25%")))))
  (is (= {:surge 0 :unavailable 2} (posture/rolling-update-bounds (deployment 2 :strategy (rolling 0 2)))))
  (is (= {:surge 0 :unavailable 1} (posture/rolling-update-bounds (deployment 3 :strategy (rolling "0%" "10%"))))
      "both resolving to zero lets one pod go")
  (is (nil? (posture/rolling-update-bounds (deployment 3 :strategy {:type "Recreate"})))))

(defn- rollouts [& deployments]
  (vec (posture/detect-rollouts-without-capacity {"deployments" {:items (vec deployments)}})))

(deftest rollouts-that-take-everything-down
  (let [[finding] (rollouts (deployment 2 :strategy (rolling 0 2)))]
    (is (= :warning (:severity finding)))
    (is (= "shop/web" (:component finding)))
    (is (= "Deployment's rolling update may take all 2 pod(s) down at once (maxSurge 0, maxUnavailable 2)" (:summary finding))))
  (is (seq (rollouts (deployment 3 :strategy (rolling 0 "100%")))))
  (is (seq (rollouts (deployment 1 :strategy (rolling 0 1)))) "one replica with no surge")
  (testing "safe or deliberate shapes stay silent"
    (is (empty? (rollouts (deployment 3))))
    (is (empty? (rollouts (deployment 3 :strategy (rolling 0 1)))))
    (is (empty? (rollouts (deployment 2 :strategy (rolling 1 2)))) "a surge pod starts first")
    (is (empty? (rollouts (deployment 0 :strategy (rolling 0 1)))) "nothing runs")
    (is (empty? (rollouts (deployment 3 :strategy {:type "Recreate"}))) "Recreate is an explicit choice")))

(defn- pod [name node & {:keys [ready workload] :or {ready true workload "web"}}]
  {:metadata {:namespace "shop" :name name :labels {:pod-template-hash "abc"}
              :ownerReferences [{:kind "ReplicaSet" :name (str workload "-abc") :controller true}]}
   :spec {:nodeName node}
   :status {:phase "Running" :conditions [{:type "Ready" :status (if ready "True" "False")}]}})

(def ^:private two-nodes {:items [{:metadata {:name "n1"}} {:metadata {:name "n2"}}]})

(defn- one-node [& {:keys [pods deployments nodes] :or {nodes two-nodes}}]
  (vec (posture/detect-replicas-on-one-node {"deployments" {:items (vec deployments)} "statefulsets" {:items []}
                                             "pods" {:items (vec pods)} "nodes" nodes})))

(deftest replicas-sharing-one-node
  (let [[finding & more] (one-node :deployments [(deployment 3)] :pods [(pod "a" "n1") (pod "b" "n1") (pod "c" "n1")])]
    (is (nil? more))
    (is (= "shop/web" (:component finding)))
    (is (= "Deployment runs all 3 ready replicas on node n1" (:summary finding))))
  (testing "spread, unready, single or unavoidable placements stay silent"
    (is (empty? (one-node :deployments [(deployment 2)] :pods [(pod "a" "n1") (pod "b" "n2")])))
    (is (empty? (one-node :deployments [(deployment 2)] :pods [(pod "a" "n1") (pod "b" "n1" :ready false)])) "one ready pod")
    (is (empty? (one-node :deployments [(deployment 1)] :pods [(pod "a" "n1")])))
    (is (empty? (one-node :deployments [(deployment 2)] :pods [(pod "a" "n1") (pod "b" "n1")] :nodes {:items [{:metadata {:name "n1"}}]}))
        "a one-node cluster has nowhere else")
    (is (empty? (one-node :deployments [(deployment 2)] :pods [(pod "a" "n1") (pod "b" "n1")]
                          :nodes {:items [{:metadata {:name "n1"}} {:metadata {:name "n2"} :spec {:unschedulable true}}]}))
        "a cordoned node does not count")
    (is (empty? (one-node :deployments [(deployment 2 :name "other")] :pods [(pod "a" "n1") (pod "b" "n1")]))
        "pods of a workload that does not want several replicas")))

(deftest limit-range-aware-resource-gaps
  (is (= ["cpu request" "memory request" "memory limit"] (posture/missing-resources {:name "a"} "shop" #{})))
  (is (= [] (posture/missing-resources {:resources {:requests {:cpu "100m" :memory "64Mi"} :limits {:memory "128Mi"}}} "shop" #{})))
  (is (= ["memory limit"] (posture/missing-resources {:resources {:requests {:cpu "100m" :memory "64Mi"} :limits {:cpu "1"}}} "shop" #{}))
      "a CPU limit is never asked for")
  (is (= [] (posture/missing-resources {:name "a"} "shop" #{["shop" :requests :cpu] ["shop" :requests :memory] ["shop" :limits :memory]})))
  (is (= ["cpu request" "memory request" "memory limit"]
         (posture/missing-resources {:name "a"} "shop" #{["other" :requests :cpu] ["other" :requests :memory] ["other" :limits :memory]}))))

(defn- resources [& {:keys [deployments limitranges]}]
  (vec (posture/detect-missing-resources {"deployments" {:items (vec deployments)} "statefulsets" {:items []} "daemonsets" {:items []}
                                          "cronjobs" {:items []} "limitranges" {:items (vec limitranges)}})))

(deftest missing-resources-per-workload
  (let [[finding] (resources :deployments [(deployment 2)])]
    (is (= :info (:severity finding)))
    (is (= "Deployment containers lack resource settings: app (cpu request, memory request, memory limit)" (:summary finding))))
  (testing "a LimitRange default fills the gap; its default limit also serves as the request"
    (is (empty? (resources :deployments [(deployment 2)]
                           :limitranges [{:metadata {:namespace "shop" :name "defaults"}
                                          :spec {:limits [{:type "Container" :default {:cpu "1" :memory "256Mi"}}]}}])))
    (is (= ["Deployment containers lack resource settings: app (memory limit)"]
           (map :summary (resources :deployments [(deployment 2)]
                                    :limitranges [{:metadata {:namespace "shop" :name "requests-only"}
                                                   :spec {:limits [{:type "Container" :defaultRequest {:cpu "100m" :memory "64Mi"}}]}}]))))
    (is (seq (resources :deployments [(deployment 2)]
                        :limitranges [{:metadata {:namespace "shop" :name "pods"} :spec {:limits [{:type "Pod" :default {:cpu "1" :memory "1Gi"}}]}}]))
        "a Pod-type limit sets no container default")))

(deftest the-checks-live-in-the-posture-scope
  (let [posture-names (set (map :name (registry/for-scope :posture)))
        all-names (set (map :name (registry/for-scope :all)))]
    (doseq [name ["rollouts-without-capacity" "replicas-on-one-node" "missing-resources"]]
      (is (contains? posture-names name))
      (is (not (contains? all-names name))))))
