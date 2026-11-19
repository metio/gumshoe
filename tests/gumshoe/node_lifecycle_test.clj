;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.node-lifecycle-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.nodes :as nodes]))

(def ^:private now (java.time.Instant/parse "2026-11-19T12:00:00Z"))

(defn- node [name & {:keys [taints deleted finalizers kubelet]}]
  {:metadata (cond-> {:name name}
               deleted (assoc :deletionTimestamp deleted)
               finalizers (assoc :finalizers finalizers))
   :spec (cond-> {} taints (assoc :taints taints))
   :status (cond-> {} kubelet (assoc :nodeInfo {:kubeletVersion kubelet}))})

(defn- epoch [iso] (str (.getEpochSecond (java.time.Instant/parse iso))))

(defn- removals [& ns] (vec (nodes/detect-stalled-removals {:now now "nodes" {:items (vec ns)}})))

(deftest who-removes-a-node
  (is (= {:by "the cluster autoscaler" :since (java.time.Instant/parse "2026-11-19T11:40:00Z")}
         (nodes/removal (node "a" :taints [{:key "ToBeDeletedByClusterAutoscaler" :value (epoch "2026-11-19T11:40:00Z") :effect "NoSchedule"}]))))
  (is (= "Karpenter" (:by (nodes/removal (node "a" :taints [{:key "karpenter.sh/disrupted" :effect "NoSchedule"}])))))
  (is (= "Karpenter" (:by (nodes/removal (node "a" :taints [{:key "karpenter.sh/disruption" :value "disrupting"}])))) "the older taint")
  (is (= {:by "a delete" :since (java.time.Instant/parse "2026-11-19T11:00:00Z")} (nodes/removal (node "a" :deleted "2026-11-19T11:00:00Z"))))
  (is (nil? (nodes/removal (node "a" :taints [{:key "node.kubernetes.io/unschedulable"}]))))
  (is (nil? (nodes/removal (node "a" :taints [{:key "DeletionCandidateOfClusterAutoscaler" :value "1"}]))) "a candidate is not a removal")
  (testing "an autoscaler taint without a readable time falls back to the deletion"
    (is (= (java.time.Instant/parse "2026-11-19T11:00:00Z")
           (:since (nodes/removal (node "a" :deleted "2026-11-19T11:00:00Z" :taints [{:key "ToBeDeletedByClusterAutoscaler" :value "soon"}])))))))

(deftest stalled-removals-escalate-with-time
  (let [ca (fn [iso] (node "n" :taints [{:key "ToBeDeletedByClusterAutoscaler" :value (epoch iso)}]))]
    (is (empty? (removals (ca "2026-11-19T11:55:00Z"))) "five minutes is normal")
    (let [[finding] (removals (ca "2026-11-19T11:45:00Z"))]
      (is (= :warning (:severity finding)))
      (is (= "n" (:component finding)))
      (is (= "node removal by the cluster autoscaler has not finished after 15 minutes" (:summary finding)))
      (is (= "2026-11-19T11:45:00Z" (str (:since finding)))))
    (is (= :critical (:severity (first (removals (ca "2026-11-19T11:20:00Z"))))))))

(deftest a-karpenter-removal-needs-the-deletion-time
  (is (empty? (removals (node "k" :taints [{:key "karpenter.sh/disrupted"}]))) "tainted but not deleted yet: no time to judge")
  (is (= "node removal by Karpenter has not finished after 40 minutes"
         (:summary (first (removals (node "k" :taints [{:key "karpenter.sh/disrupted"}] :deleted "2026-11-19T11:20:00Z")))))))

(deftest finalizers-are-named
  (let [[finding] (removals (node "d" :deleted "2026-11-19T11:00:00Z" :finalizers ["example.com/protect"]))]
    (is (re-find #"^finalizers example.com/protect hold the node; " (:hint finding)))))

(deftest kubelet-minors
  (is (= "1.31" (nodes/kubelet-minor (node "a" :kubelet "v1.31.4"))))
  (is (= "1.31" (nodes/kubelet-minor (node "a" :kubelet "v1.31.4+k3s1"))))
  (is (nil? (nodes/kubelet-minor (node "a")))))

(defn- skew [& ns] (vec (nodes/detect-kubelet-skew {"nodes" {:items (vec ns)}})))

(deftest kubelet-skew
  (is (empty? (skew (node "a" :kubelet "v1.31.1") (node "b" :kubelet "v1.31.4"))) "patch versions may differ")
  (let [[finding] (skew (node "a" :kubelet "v1.31.1") (node "b" :kubelet "v1.30.9") (node "c" :kubelet "v1.31.0"))]
    (is (= :info (:severity finding)))
    (is (= "nodes run kubelet 1.30 (1), 1.31 (2)" (:summary finding))))
  (let [[finding] (skew (node "a" :kubelet "v1.29.0") (node "b" :kubelet "v1.31.0"))]
    (is (= :warning (:severity finding)))
    (is (re-find #"^more than one minor apart" (:hint finding))))
  (is (= "nodes run kubelet 1.9 (1), 1.10 (1)" (:summary (first (skew (node "a" :kubelet "v1.10.0") (node "b" :kubelet "v1.9.3")))))
      "minors sort numerically")
  (is (empty? (skew (node "a" :kubelet "v1.31.0") (node "b"))) "an unknown version is not a second version"))
