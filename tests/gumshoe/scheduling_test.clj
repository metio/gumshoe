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

(deftest an-unknown-cause-keeps-its-text-without-the-node-prefix
  (is (= [{:cause :other :explain "had a brand new problem" :action nil :nodes 2}]
         (scheduling/causes "0/2 nodes are available: 2 node(s) had a brand new problem.")))
  (is (= "were struck by lightning"
         (:explain (first (scheduling/causes "0/1 nodes are available: 1 were struck by lightning."))))))

(def ^:private daemonset-message
  (str "0/52 nodes are available: 1 Insufficient memory, 51 node(s) didn't satisfy plugin(s) [NodeAffinity]. "
       "preemption: 0/52 nodes are available: 1 No preemption victims found for incoming pod, "
       "51 Preemption is not helpful for scheduling."))

(defn- daemonset-pod
  "A DaemonSet pod as the controller creates it: required node affinity pinning
   it to one node by matchFields metadata.name."
  [node message]
  {:metadata {:namespace "logging" :name "default-fluentbit-x7k2p"
              :ownerReferences [{:kind "DaemonSet" :name "default-fluentbit" :controller true}]}
   :spec {:affinity {:nodeAffinity
                     {:requiredDuringSchedulingIgnoredDuringExecution
                      {:nodeSelectorTerms [{:matchFields [{:key "metadata.name" :operator "In" :values [node]}]}]}}}
          :priority 0}
   :status {:phase "Pending"
            :conditions [{:type "PodScheduled" :status "False" :reason "Unschedulable"
                          :lastTransitionTime "2026-11-18T07:00:00Z" :message message}]}})

(deftest the-node-affinity-plugin-wording-is-node-affinity
  (doseq [clause ["node(s) didn't satisfy plugin(s) [NodeAffinity]"
                  "node(s) didn't match Pod's node affinity"
                  "node(s) didn't match node selector"
                  "node(s) didn't match Pod's node affinity/selector"]]
    (is (= [:node-affinity] (map :cause (scheduling/causes (str "0/4 nodes are available: 4 " clause ".")))) clause))
  (is (= [:taint] (map :cause (scheduling/causes "0/2 nodes are available: 2 node(s) had untolerated taint(s).")))))

(deftest the-message-names-how-many-nodes-were-considered
  (is (= 52 (scheduling/total daemonset-message)))
  (is (nil? (scheduling/total "no verdict here"))))

(deftest a-pod-pinned-by-matchfields-names-its-nodes
  (is (= #{"worker-392jb94c.kube.infra.run"}
         (:pinned (scheduling/placement (daemonset-pod "worker-392jb94c.kube.infra.run" daemonset-message)))))
  (is (:restricted? (scheduling/placement {:spec {:nodeSelector {"disk" "ssd"}}})))
  (is (nil? (:pinned (scheduling/placement {:spec {:nodeSelector {"disk" "ssd"}}}))))
  (is (= {:pinned nil :restricted? false} (scheduling/placement {:spec {}}))))

(deftest a-daemonset-pod-leads-with-what-blocks-its-one-node
  (let [pod (daemonset-pod "worker-392jb94c.kube.infra.run" daemonset-message)
        [finding] (pods/detect-unhealthy-pods {:now now "pods" {:items [pod]}})]
    (is (= :critical (:severity finding)))
    (is (= "pod is unschedulable: 1 node(s) lack free memory" (:summary finding)))
    (testing "the hint names the target node, then the real reason, then the structural count"
      (is (str/starts-with? (:hint finding) "pinned to node worker-392jb94c.kube.infra.run; 1 node(s) lack free memory"))
      (is (re-find #"lack free memory.*51 node\(s\).*the pod's own node selection" (:hint finding))))
    (testing "the finding keeps every reason as data"
      (is (= [[1 :insufficient-resource false] [51 :node-affinity true]]
             (map (juxt :nodes :cause :structural) (:causes finding)))))))

(deftest a-pod-whose-target-node-fails-affinity-leads-with-affinity
  (let [message "0/3 nodes are available: 3 node(s) didn't satisfy plugin(s) [NodeAffinity]."
        [finding] (pods/detect-unhealthy-pods {:now now "pods" {:items [(daemonset-pod "gone" message)]}})]
    (is (= "pod is unschedulable: 3 node(s) lack labels the pod's nodeSelector or node affinity requires"
           (:summary finding)))))

(deftest a-pod-with-a-node-selector-leads-with-what-blocks-its-candidate-nodes
  (let [message (str "0/10 nodes are available: 2 Insufficient cpu, 8 node(s) didn't match Pod's node affinity/selector. "
                     "preemption: 0/10 nodes are available: 2 No preemption victims found for incoming pod, "
                     "8 Preemption is not helpful for scheduling.")
        pod (assoc-in (pending {:type "PodScheduled" :status "False" :reason "Unschedulable"
                                :lastTransitionTime "2026-11-18T11:57:00Z" :message message})
                      [:spec :nodeSelector] {"node-role" "db"})
        [finding] (pods/detect-unhealthy-pods {:now now "pods" {:items [pod]}})]
    (is (= "pod is unschedulable: 2 node(s) lack free cpu" (:summary finding)))
    (is (str/starts-with? (:hint finding) "2 node(s) lack free cpu"))))

(deftest a-plain-pod-leads-with-the-reason-rejecting-most-nodes
  (let [message (str "0/6 nodes are available: 1 Too many pods, 3 Insufficient cpu, "
                     "2 node(s) had untolerated taint {dedicated: db}.")
        [finding] (pods/detect-unhealthy-pods
                   {:now now "pods" {:items [(pending {:type "PodScheduled" :status "False" :reason "Unschedulable"
                                                       :lastTransitionTime "2026-11-18T11:57:00Z" :message message})]}})]
    (is (= "pod is unschedulable: 3 node(s) lack free cpu" (:summary finding)))
    (is (= [:insufficient-resource :taint :max-pods] (map :cause (:causes finding))))
    (is (not-any? :structural (:causes finding)))))

(deftest an-unparseable-message-falls-back-to-the-raw-message
  (let [message "scheduler is having a bad day"
        [finding] (pods/detect-unhealthy-pods
                   {:now now "pods" {:items [(pending {:type "PodScheduled" :status "False" :reason "Unschedulable"
                                                       :lastTransitionTime "2026-11-18T11:57:00Z" :message message})]}})]
    (is (= "pod is unschedulable" (:summary finding)))
    (is (= message (:hint finding)))
    (is (empty? (:causes finding)))))
