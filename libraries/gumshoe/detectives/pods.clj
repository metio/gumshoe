;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.pods
  "Detectives for workload health: crash loops, image pull errors, OOM kills,
   excessive restarts, pods stuck in Pending or Failed."
  (:require [clojure.string]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.scheduling :as scheduling]))

(def ^:private bad-waiting-reasons
  {"CrashLoopBackOff" :critical
   "ImagePullBackOff" :critical
   "ErrImagePull" :critical
   "CreateContainerConfigError" :critical
   "InvalidImageName" :critical})

(def ^:private restart-threshold 5)

(defn- container-findings
  [pod]
  (let [component (kubectl/namespace-name-of pod)
        statuses (-> pod :status :containerStatuses)]
    (concat
     (for [status statuses
           :let [reason (-> status :state :waiting :reason)
                 severity (bad-waiting-reasons reason)]
           :when severity]
       {:severity severity
        :component component
        :summary (format "container %s is waiting: %s" (:name status) reason)
        :hint (-> status :state :waiting :message)})
     (for [status statuses
           ;; A container restarted after an OOM kill records it in lastState; a
           ;; one-shot container (restartPolicy Never) is left terminated in its
           ;; current state instead, so both must be checked.
           :when (= "OOMKilled" (or (-> status :state :terminated :reason)
                                    (-> status :lastState :terminated :reason)))]
       {:severity :warning
        :component component
        :summary (format "container %s was OOMKilled" (:name status))
        :hint "compare the memory limit with actual usage"})
     (for [status statuses
           :when (< restart-threshold (or (:restartCount status) 0))]
       {:severity :warning
        :component component
        :summary (format "container %s restarted %d times" (:name status) (:restartCount status))}))))

(defn- scheduled-condition
  [pod]
  (->> (-> pod :status :conditions)
       (filter #(= "PodScheduled" (:type %)))
       first))

(def ^:private unschedulable-critical-minutes 10)

(defn- pending-finding
  "What a Pending pod waits for. A pod the scheduler rejected gets the causes
   from the scheduler's message, and turns critical once it has waited 10
   minutes; a pod held by scheduling gates waits on purpose. A pod already
   bound to a node is left to the pod-startup detective."
  [pod now]
  (let [component (kubectl/namespace-name-of pod)
        condition (scheduled-condition pod)
        since (some-> (:lastTransitionTime condition) java.time.Instant/parse)]
    (cond
      (seq (-> pod :spec :schedulingGates))
      {:severity :info
       :component component
       :summary (format "pod is held by scheduling gates: %s"
                        (clojure.string/join ", " (map :name (-> pod :spec :schedulingGates))))
       :hint "a controller removes the gates when the pod may be scheduled"}

      (and (= "False" (:status condition)) (= "Unschedulable" (:reason condition)))
      (let [causes (scheduling/causes (:message condition))
            stuck? (and since now
                        (.isBefore ^java.time.Instant since
                                   (.minus ^java.time.Instant now
                                           (java.time.Duration/ofMinutes unschedulable-critical-minutes))))]
        {:severity (if stuck? :critical :warning)
         :component component
         :summary (if-let [top (first causes)]
                    (format "pod is unschedulable: %d node(s) %s" (:nodes top) (:explain top))
                    "pod is unschedulable")
         :hint (if (seq causes) (scheduling/describe causes) (:message condition))})

      ;; a pod bound to a node waits for its kubelet; pod-startup reads why
      (-> pod :spec :nodeName)
      nil

      :else
      {:severity :warning
       :component component
       :summary "pod is stuck in Pending"
       :hint (:message condition)})))

(defn detect-unhealthy-pods
  [evidence]
  (let [pods (kubectl/items-of (get evidence "pods"))]
    (concat
     (mapcat container-findings pods)
     (for [pod pods
           :when (= "Failed" (-> pod :status :phase))]
       {:severity :warning
        :component (kubectl/namespace-name-of pod)
        :summary (format "pod is in phase Failed (%s)" (or (-> pod :status :reason) "unknown reason"))})
     (keep #(pending-finding % (:now evidence))
           (filter #(= "Pending" (-> % :status :phase)) pods)))))

(def ^:private stuck-terminating-minutes 10)

(defn detect-stuck-terminating
  [evidence]
  (let [now (:now evidence)]
    (for [pod (kubectl/items-of (get evidence "pods"))
          :let [deletion (-> pod :metadata :deletionTimestamp)]
          :when (and deletion
                     (.isBefore (java.time.Instant/parse deletion)
                                (.minus ^java.time.Instant now
                                        (java.time.Duration/ofMinutes stuck-terminating-minutes))))]
      {:severity :warning
       :component (kubectl/namespace-name-of pod)
       :summary (format "pod is stuck terminating since %s" deletion)
       :hint "often a stuck volume unmount or a finalizer - check the kubelet log of its node"})))

(def detectives
  [{:name "pods"
    :description "Unhealthy pods: crash loops, image pull errors, OOM kills, restarts, Pending/Failed"
    :requires ["pods"]
    :detect detect-unhealthy-pods}
   {:name "stuck-pods"
    :description "Pods stuck in Terminating - usually stuck mounts or finalizers"
    :requires ["pods"]
    :detect detect-stuck-terminating}])
