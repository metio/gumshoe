;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.pod-startup
  "Detective for pods the scheduler placed on a node that then never start. The
   kubelet reports why as events on the pod: the sandbox (network namespace,
   pod IP) cannot be created, a volume cannot be attached or mounted. A pod with
   no such event that is still creating its containers long after it was bound
   is reported too, because the cause then sits in the node's kubelet or
   runtime."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def ^:private recent-window (java.time.Duration/ofHours 1))
(def ^:private stall-minutes 10)
(def ^:private critical-minutes 30)

(defn- instant [s] (some-> s java.time.Instant/parse))

(defn- older-than?
  [^java.time.Instant now ^java.time.Instant since minutes]
  (and now since (.isBefore since (.minus now (java.time.Duration/ofMinutes minutes)))))

(def ^:private ip-exhaustion
  #"(?i)assign an IP|InsufficientFreeAddresses|no IP addresses available|failed to allocate for range|IP pool .*exhausted|no more IPs")

(defn- classify
  "The cause an event names, as [cause severity summary advice], or nil for an
   event that says nothing about startup."
  [{:keys [reason message]}]
  (let [message (str message)]
    (case reason
      "FailedCreatePodSandBox"
      (if (re-find ip-exhaustion message)
        [:ip-exhaustion :critical "pod gets no IP address: the pod network's address pool is exhausted"
         "free addresses (delete finished pods, enlarge the IP pool or node podCIDR) - check the CNI's IPAM"]
        [:sandbox :warning "the pod sandbox cannot be created"
         "the CNI plugin or the container runtime on the node fails - check their pods and the kubelet log"])

      "FailedAttachVolume"
      (if (re-find #"(?i)Multi-Attach|waiting for detach" message)
        [:multi-attach :critical "a volume of the pod is still attached to another node"
         "a ReadWriteOnce volume follows only one node - wait for or force the old pod to stop, or delete its stale VolumeAttachment"]
        [:attach :warning "a volume of the pod cannot be attached"
         "check the CSI controller's logs and the VolumeAttachment of the volume"])

      "FailedMount"
      [:mount :warning "a volume of the pod cannot be mounted"
       "a missing Secret or ConfigMap, or the CSI node plugin on that node - the message names it"]

      nil)))

(defn- startup-pod?
  "A pod bound to a node that has not started: Pending, or its containers are
   still being created."
  [pod]
  (and (-> pod :spec :nodeName)
       (= "Pending" (-> pod :status :phase))
       (nil? (-> pod :metadata :deletionTimestamp))))

(defn- bound-at
  [pod]
  (or (some->> (-> pod :status :conditions)
               (filter #(and (= "PodScheduled" (:type %)) (= "True" (:status %))))
               first :lastTransitionTime instant)
      (instant (-> pod :metadata :creationTimestamp))))

(defn- pod-events
  "The recent pod events by [namespace name], newest last."
  [evidence]
  (let [now (:now evidence)]
    (->> (kubectl/items-of (get evidence "events"))
         (filter #(and (= "Pod" (-> % :involvedObject :kind))
                       (when-let [at (instant (or (:lastTimestamp %) (:eventTime %)))]
                         (.isAfter ^java.time.Instant at (.minus ^java.time.Instant now recent-window)))))
         (sort-by #(or (:lastTimestamp %) (:eventTime %)))
         (group-by (juxt (comp :namespace :involvedObject) (comp :name :involvedObject))))))

(defn detect-stalled-startups
  [evidence]
  (let [now (:now evidence)
        events (pod-events evidence)]
    (for [pod (kubectl/items-of (get evidence "pods"))
          :when (startup-pod? pod)
          :let [component (kubectl/namespace-name-of pod)
                causes (->> (get events [(kubectl/namespace-of pod) (kubectl/name-of pod)])
                            (keep (fn [event] (when-let [cause (classify event)] [cause event])))
                            ;; the newest event of each cause carries its current message
                            (reduce (fn [acc [cause event]] (assoc acc (first cause) [cause event])) {})
                            vals)
                since (bound-at pod)
                node (-> pod :spec :nodeName)]
          finding (if (seq causes)
                    (for [[[_ severity summary advice] event] (sort-by (comp first first) causes)]
                      (onset/since
                       {:severity (if (and (= :warning severity) (older-than? now since critical-minutes))
                                   :critical severity)
                       :component component
                       :summary summary
                       :hint (format "on node %s - %s - %s" node advice
                                     (str/replace (str (:message event)) #"\s+" " "))}
                       since))
                    (when (older-than? now since stall-minutes)
                      [(onset/since
                        {:severity (if (older-than? now since critical-minutes) :critical :warning)
                        :component component
                        :summary (format "pod is bound to node %s but has not started for over %d minutes"
                                         node (if (older-than? now since critical-minutes) critical-minutes stall-minutes))
                        :hint (str "no event names the cause - check the kubelet and container runtime on "
                                   node " (crictl pods, journalctl -u kubelet)")}
                        since)]))]
      finding)))

(def detectives
  [{:name "pod-startup"
    :description "Scheduled pods that never start: sandbox and IP errors, Multi-Attach, attach and mount failures"
    :requires ["pods" "events"]
    :detect detect-stalled-startups}])
