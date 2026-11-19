;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.karpenter-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.karpenter :as karpenter]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.karpenter]))

(def ^:private now (java.time.Instant/parse "2026-11-19T12:00:00Z"))

(defn- condition [type status & {:keys [reason message at] :or {at "2026-11-19T11:00:00Z"}}]
  (cond-> {:type type :status status :lastTransitionTime at}
    reason (assoc :reason reason)
    message (assoc :message message)))

(defn- pool [& {:keys [conditions limits resources]}]
  {:metadata {:name "default"}
   :spec (cond-> {} limits (assoc :limits limits))
   :status (cond-> {:conditions (or conditions [(condition "Ready" "True")])} resources (assoc :resources resources))})

(deftest a-healthy-pool-says-nothing
  (is (empty? (karpenter/nodepool-findings (pool :limits {:cpu "1000"} :resources {:cpu "10" :memory "40Gi"})))))

(deftest a-pool-that-is-not-ready-names-the-failing-condition
  (let [[finding] (karpenter/nodepool-findings
                   (pool :conditions [(condition "Ready" "False" :reason "NodeClassReadyFalse")
                                      (condition "ValidationSucceeded" "True")
                                      (condition "NodeClassReady" "False" :reason "NodeClassNotReady" :message "EC2NodeClass default not ready" :at "2026-11-19T10:00:00Z")]))]
    (is (= :critical (:severity finding)))
    (is (= "default" (:component finding)))
    (is (= "NodePool is not Ready (NodeClassReady)" (:summary finding)))
    (is (str/starts-with? (:hint finding) "NodeClassNotReady - EC2NodeClass default not ready - Karpenter launches no nodes"))
    (is (= "2026-11-19T11:00:00Z" (:since finding)) "onset of the Ready condition"))
  (testing "a failed validation"
    (is (= "NodePool is not Ready (ValidationSucceeded)"
           (:summary (first (karpenter/nodepool-findings
                             (pool :conditions [(condition "Ready" "False")
                                                (condition "ValidationSucceeded" "False" :message "invalid requirement")])))))))
  (testing "no sub-condition explains it"
    (is (= "NodePool is not Ready (Unknown reason)"
           (:summary (first (karpenter/nodepool-findings (pool :conditions [(condition "Ready" "False" :reason "Unknown reason")]))))))
    (is (= "NodePool is not Ready (False)"
           (:summary (first (karpenter/nodepool-findings (pool :conditions [(condition "Ready" "False")])))))))
  (testing "Unknown is not False"
    (is (empty? (karpenter/nodepool-findings (pool :conditions [(condition "Ready" "Unknown")]))))))

(deftest nodes-that-fail-to-register
  (let [[finding] (karpenter/nodepool-findings
                   (pool :conditions [(condition "Ready" "True")
                                      (condition "NodeRegistrationHealthy" "False" :reason "Unhealthy" :message "Failed to launch node")]))]
    (is (= "NodePool's nodes fail to register" (:summary finding)))
    (is (str/starts-with? (:hint finding) "Unhealthy - Failed to launch node - instances launch"))))

(deftest limits
  (is (= [["cpu" "100" "100"] ["memory" "500Gi" "400Gi"]]
         (karpenter/reached-limits (pool :limits {:cpu "100" :memory "400Gi" :nvidia.com/gpu "4"}
                                         :resources {:cpu "100" :memory "500Gi" :nvidia.com/gpu "2"}))))
  (is (= ["NodePool reached its cpu limit (100 of 100)" "NodePool reached its memory limit (500Gi of 400Gi)"]
         (map :summary (karpenter/nodepool-findings (pool :limits {:cpu "100" :memory "400Gi"} :resources {:cpu "100" :memory "500Gi"})))))
  (is (= ["NodePool reached its cpu limit (1.5 of 1.5)"]
         (map :summary (karpenter/nodepool-findings (pool :limits {:cpu "1500m"} :resources {:cpu "1.5"})))))
  (testing "below the limit, without usage, or with unparseable values nothing is reported"
    (is (empty? (karpenter/reached-limits (pool :limits {:cpu "100"} :resources {:cpu "99"}))))
    (is (empty? (karpenter/reached-limits (pool :limits {:cpu "100"}))))
    (is (empty? (karpenter/reached-limits (pool :limits {:cpu "lots"} :resources {:cpu "100"}))))))

(defn- claim [& {:keys [created conditions deleting]}]
  {:metadata (cond-> {:name "default-abc12" :creationTimestamp (or created "2026-11-19T11:00:00Z")}
               deleting (assoc :deletionTimestamp "2026-11-19T11:50:00Z"))
   :status {:conditions conditions}})

(deftest claims-that-never-become-nodes
  (let [[finding] (karpenter/nodeclaim-findings
                   (claim :conditions [(condition "Launched" "True")
                                       (condition "Registered" "Unknown" :reason "AwaitingReconciliation" :message "Node not registered with cluster")
                                       (condition "Ready" "Unknown")])
                   now)]
    (is (= :critical (:severity finding)))
    (is (= "default-abc12" (:component finding)))
    (is (= "NodeClaim is not Registered after 60 minutes" (:summary finding)))
    (is (str/starts-with? (:hint finding) "AwaitingReconciliation - Node not registered with cluster - the instance runs but"))
    (is (= "2026-11-19T11:00:00Z" (:since finding))))
  (testing "each step names its own cause"
    (is (str/includes? (:hint (first (karpenter/nodeclaim-findings (claim :conditions [(condition "Launched" "False" :reason "InsufficientCapacity")]) now)))
                       "InsufficientCapacity - the cloud provider did not create the instance"))
    (is (= "NodeClaim is not Initialized after 60 minutes"
           (:summary (first (karpenter/nodeclaim-findings (claim :conditions [(condition "Launched" "True") (condition "Registered" "True")
                                                                              (condition "Initialized" "False")]) now))))))
  (testing "all steps passed but not Ready"
    (is (= "NodeClaim is not Ready after 60 minutes"
           (:summary (first (karpenter/nodeclaim-findings (claim :conditions (conj (mapv #(condition % "True") ["Launched" "Registered" "Initialized"])
                                                                                   (condition "Ready" "False"))) now))))))
  (testing "a young claim, a Ready claim and a deleting claim are left alone"
    (is (empty? (karpenter/nodeclaim-findings (claim :created "2026-11-19T11:50:00Z" :conditions []) now)))
    (is (empty? (karpenter/nodeclaim-findings (claim :conditions [(condition "Ready" "True")]) now)))
    (is (empty? (karpenter/nodeclaim-findings (claim :deleting true :conditions []) now))))
  (testing "fifteen minutes is late"
    (is (seq (karpenter/nodeclaim-findings (claim :created "2026-11-19T11:45:00Z" :conditions []) now)))))

(deftest drifted-claims-that-are-not-replaced
  (let [ready (condition "Ready" "True")
        [finding] (karpenter/nodeclaim-findings (claim :conditions [ready (condition "Drifted" "True" :reason "NodePoolDrifted" :at "2026-11-19T09:30:00Z")]) now)]
    (is (= :warning (:severity finding)))
    (is (= "NodeClaim has been Drifted for 150 minutes without being replaced" (:summary finding)))
    (is (str/starts-with? (:hint finding) "NodePoolDrifted - a disruption budget"))
    (is (= "2026-11-19T09:30:00Z" (:since finding)))
    (is (empty? (karpenter/nodeclaim-findings (claim :conditions [ready (condition "Drifted" "True" :at "2026-11-19T11:30:00Z")]) now)) "replacement under way")
    (is (empty? (karpenter/nodeclaim-findings (claim :conditions [ready (condition "Drifted" "False" :at "2026-11-19T01:00:00Z")]) now)))))

(deftest the-package-joins-the-platform-scope
  (is (every? (set (map :name (registry/for-scope :platform))) (map :name karpenter/detectives)))
  (is (empty? (detective/run-detectives karpenter/detectives {:now now})) "a cluster without Karpenter")
  (is (= 1 (count (karpenter/detect-nodeclaims {:now now karpenter/nodeclaim-type {:items [(claim :conditions [])]}})))))
