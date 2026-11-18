;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.scheduling-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.pods :as pods]
            [gumshoe.scheduling :as scheduling]))

(def ^:private mixed
  (str "0/6 nodes are available: 1 node(s) had untolerated taint {node-role.kubernetes.io/control-plane: }, "
       "2 Insufficient memory, 3 node(s) didn't match Pod's node affinity/selector. "
       "preemption: 0/6 nodes are available: 3 Preemption is not helpful for scheduling, "
       "3 No preemption victims found for incoming pod."))

(deftest causes-are-ranked-by-rejected-nodes
  (let [causes (scheduling/causes mixed)]
    (is (= [[3 :node-affinity] [2 :insufficient-resource] [1 :taint]]
           (map (juxt :nodes :cause) causes)))
    (testing "the preemption report is not read as node verdicts"
      (is (= 3 (count causes))))
    (testing "the resource and the taint are named"
      (is (= "lack free memory" (:explain (second causes))))
      (is (str/includes? (:explain (last causes)) "{node-role.kubernetes.io/control-plane: }")))))

(deftest every-scheduler-cause-is-recognised
  (doseq [[clause cause] [["Insufficient cpu" :insufficient-resource]
                          ["Insufficient nvidia.com/gpu" :insufficient-resource]
                          ["Too many pods" :max-pods]
                          ["node(s) had taint {dedicated: db}, that the pod didn't tolerate" :taint]
                          ["node(s) didn't match Pod's node affinity/selector" :node-affinity]
                          ["node(s) didn't match pod anti-affinity rules" :pod-anti-affinity]
                          ["node(s) didn't satisfy existing pods anti-affinity rules" :pod-anti-affinity]
                          ["node(s) didn't match pod affinity rules" :pod-affinity]
                          ["node(s) didn't match pod topology spread constraints" :topology-spread]
                          ["node(s) had volume node affinity conflict" :volume-node-affinity]
                          ["node(s) didn't find available persistent volumes to bind" :volume-binding]
                          ["pod has unbound immediate PersistentVolumeClaims" :volume-binding]
                          ["node(s) exceed max volume count" :volume-count]
                          ["node(s) didn't have free ports for the requested pod ports" :host-ports]
                          ["node(s) were unschedulable" :cordoned]
                          ["node(s) had something new" :other]]]
    (is (= [cause] (map :cause (scheduling/causes (str "0/4 nodes are available: 4 " clause ".")))) clause)))

(deftest a-message-without-node-verdicts-has-no-causes
  (is (empty? (scheduling/causes "0/3 nodes available")))
  (is (empty? (scheduling/causes nil))))

(defn- pending
  [condition & {:keys [gates]}]
  {:metadata {:namespace "shop" :name "web-1"}
   :spec (cond-> {} gates (assoc :schedulingGates gates))
   :status {:phase "Pending" :conditions [condition]}})

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

(deftest an-unschedulable-pod-names-its-main-cause
  (let [condition {:type "PodScheduled" :status "False" :reason "Unschedulable"
                   :lastTransitionTime "2026-11-18T11:57:00Z" :message mixed}
        [finding] (pods/detect-unhealthy-pods {:now now "pods" {:items [(pending condition)]}})]
    (is (= :warning (:severity finding)))
    (is (= "pod is unschedulable: 3 node(s) lack labels the pod's nodeSelector or node affinity requires"
           (:summary finding)))
    (is (str/starts-with? (:hint finding) "3 node(s) lack labels"))
    (is (str/includes? (:hint finding) "2 node(s) lack free memory - lower the pod's memory request"))
    (testing "it turns critical after 10 minutes"
      (is (= :critical
             (:severity (first (pods/detect-unhealthy-pods
                                {:now now
                                 "pods" {:items [(pending (assoc condition :lastTransitionTime
                                                                 "2026-11-18T11:45:00Z"))]}}))))))))

(deftest a-gated-pod-waits-on-purpose
  (let [[finding] (pods/detect-unhealthy-pods
                   {:now now
                    "pods" {:items [(pending {:type "PodScheduled" :status "False" :reason "SchedulingGated"}
                                             :gates [{:name "example.com/quota"}])]}})]
    (is (= :info (:severity finding)))
    (is (= "pod is held by scheduling gates: example.com/quota" (:summary finding)))))

(deftest a-scheduled-pending-pod-keeps-the-generic-finding
  (let [[finding] (pods/detect-unhealthy-pods
                   {:now now "pods" {:items [(pending {:type "PodScheduled" :status "True"})]}})]
    (is (= "pod is stuck in Pending" (:summary finding)))))
