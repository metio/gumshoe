;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.nodes
  "Detectives for node health: readiness, every node-problem condition, cordoned
   nodes, removals that stall, and kubelets on different minor versions."
  (:require [clojure.string :as str]
            [gumshoe.conditions :as conditions]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def healthy-when-true
  "Node conditions besides Ready whose status True means the subsystem works,
   so False is the problem: the conditions the EKS node monitoring agent sets."
  #{"KernelReady" "NetworkingReady" "StorageReady" "AcceleratedHardwareReady" "ContainerRuntimeReady"})

(def problem-when-true
  "Node conditions whose status True means the problem is present: the
   kubelet's pressures and NetworkUnavailable."
  #{"MemoryPressure" "DiskPressure" "PIDPressure" "NetworkUnavailable"})

(defn condition-polarity
  "How a node condition other than Ready reads: :healthy-when-true or
   :problem-when-true. A type neither list names is read by its name: one
   ending in Ready follows Ready's convention, as the EKS agent's do, and any
   other follows the pressures' and node-problem-detector's (KernelDeadlock,
   ReadonlyFilesystem, FrequentKubeletRestart, ...)."
  [type]
  (cond
    (healthy-when-true type) :healthy-when-true
    (problem-when-true type) :problem-when-true
    (str/ends-with? (str type) "Ready") :healthy-when-true
    :else :problem-when-true))

(defn condition-problem
  "The summary of a node condition other than Ready that reports a problem,
   or nil when it reports none: a condition in its unhealthy state, or one
   whose status is Unknown, whichever its polarity."
  [{:keys [type status]}]
  (let [bad (case (condition-polarity type) :healthy-when-true "False" :problem-when-true "True")]
    (cond
      (= bad status) (if (= "True" bad) (format "%s is active" type) (format "%s is False" type))
      (= "Unknown" status) (format "%s is Unknown" type)
      :else nil)))

(defn detect-node-problems
  [evidence]
  (let [nodes (kubectl/items-of (get evidence "nodes"))]
    (concat
     (for [node nodes
           :let [ready (conditions/condition node "Ready")]
           :when (not= "True" (:status ready))]
       (onset/since
        {:severity :critical
         :component (kubectl/name-of node)
         :summary (format "node is not Ready (status %s)" (or (:status ready) "unknown"))
         :hint (:message ready)}
        (:lastTransitionTime ready)))
     ;; A node that is not Ready is already critical above, and a kubelet that
     ;; stopped posting status turns every other condition Unknown; those
     ;; Unknowns follow from the finding above and would only repeat it.
     (for [node nodes
           :let [ready? (= "True" (:status (conditions/condition node "Ready")))]
           condition (-> node :status :conditions)
           :when (not= "Ready" (:type condition))
           :let [summary (condition-problem condition)]
           :when summary
           :when (or ready? (not= "Unknown" (:status condition)))]
       (onset/since
        {:severity :warning
         :component (kubectl/name-of node)
         :summary summary
         :hint (:message condition)}
        (:lastTransitionTime condition)))
     (for [node nodes
           :when (true? (-> node :spec :unschedulable))]
       {:severity :info
        :component (kubectl/name-of node)
        :summary "node is cordoned (unschedulable)"
        :hint "uncordon with runbooks/kubernetes/nodes/uncordon.clj when the work on it is done"}))))

;; --- removals that stall ------------------------------------------------------------

(def ^:private removal-warning-minutes 10)
(def ^:private removal-critical-minutes 30)

(defn- taint
  [node key]
  (first (filter #(= key (:key %)) (-> node :spec :taints))))

(defn removal
  "Who is removing the node and since when, or nil. The cluster autoscaler's
   taint carries the unix second it started as its value; Karpenter's taint
   carries no time, so its removal is timed from the node's deletion, as is a
   plain delete held up by finalizers."
  [node]
  (let [deleted (some-> node :metadata :deletionTimestamp java.time.Instant/parse)
        autoscaler (taint node "ToBeDeletedByClusterAutoscaler")
        karpenter (or (taint node "karpenter.sh/disrupted") (taint node "karpenter.sh/disruption"))]
    (cond
      autoscaler {:by "the cluster autoscaler"
                  :since (or (some-> (:value autoscaler) str parse-long java.time.Instant/ofEpochSecond) deleted)}
      karpenter {:by "Karpenter" :since deleted}
      deleted {:by "a delete" :since deleted}
      :else nil)))

(defn detect-stalled-removals
  [evidence]
  (let [now ^java.time.Instant (:now evidence)]
    (for [node (kubectl/items-of (get evidence "nodes"))
          :let [{:keys [by since]} (removal node)]
          :when (and by since now)
          :let [minutes (.toMinutes (java.time.Duration/between ^java.time.Instant since now))]
          :when (>= minutes removal-warning-minutes)]
      (onset/since
       {:severity (if (>= minutes removal-critical-minutes) :critical :warning)
        :component (kubectl/name-of node)
        :summary (format "node removal by %s has not finished after %d minutes" by minutes)
        :hint (str (when-let [finalizers (seq (-> node :metadata :finalizers))]
                     (str "finalizers " (str/join ", " finalizers) " hold the node; "))
                   "a pod that cannot be evicted (a PodDisruptionBudget at zero, a pod without a controller) or a finalizer usually blocks it - check the remover's logs and the pods still on the node")}
       since))))

;; --- kubelet versions -------------------------------------------------------------------

(defn kubelet-minor
  "The major.minor of a node's kubelet, e.g. \"1.31\", or nil."
  [node]
  (some->> node :status :nodeInfo :kubeletVersion (re-find #"^v?(\d+)\.(\d+)") rest (str/join ".")))

(defn detect-kubelet-skew
  [evidence]
  (let [by-minor (group-by kubelet-minor (kubectl/items-of (get evidence "nodes")))
        minors (sort-by #(mapv parse-long (str/split % #"\.")) (remove nil? (keys by-minor)))]
    (when (< 1 (count minors))
      (let [spread (- (parse-long (second (str/split (last minors) #"\.")))
                      (parse-long (second (str/split (first minors) #"\."))))]
        [{:severity (if (< 1 spread) :warning :info)
          :component "nodes"
          :summary (format "nodes run kubelet %s"
                           (str/join ", " (for [minor minors] (format "%s (%d)" minor (count (by-minor minor))))))
          :hint (str (when (< 1 spread) "more than one minor apart - ")
                     "an upgrade that has not reached every node; finish it, since each kubelet may only trail the API server by a limited number of minors")}]))))

(def detectives
  [{:name "nodes"
    :description "Node health: readiness, every node condition in its unhealthy state (pressures, NetworkUnavailable, node-problem-detector, the EKS agent's *Ready conditions), cordoned nodes"
    :requires ["nodes"]
    :detect detect-node-problems}
   {:name "node-removals"
    :description "Nodes whose removal by the cluster autoscaler, Karpenter or a delete has not finished"
    :requires ["nodes"]
    :detect detect-stalled-removals}
   {:name "kubelet-skew"
    :description "Nodes running different kubelet minor versions"
    :requires ["nodes"]
    :detect detect-kubelet-skew}])
