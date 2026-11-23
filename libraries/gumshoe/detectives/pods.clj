;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.pods
  "Detectives for workload health: restart loops, image pull errors, OOM
   kills, pods stuck in Pending or Failed."
  (:require [clojure.string]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]
            [gumshoe.scheduling :as scheduling]))

;; CrashLoopBackOff is judged with the restart loops below, by how much of
;; its workload loops; these block a container outright.
(def ^:private bad-waiting-reasons
  {"ImagePullBackOff" :critical
   "ErrImagePull" :critical
   "CreateContainerConfigError" :critical
   "InvalidImageName" :critical})

(def ^:private loop-min-restarts 3)
(def ^:private loop-recent-minutes 30)
(def ^:private loop-fast-run-minutes 10)
(def ^:private settled-minutes 5)

(defn- instant [s] (some-> s java.time.Instant/parse))

(defn- minutes-between
  [^java.time.Instant from ^java.time.Instant to]
  (/ (.toSeconds (java.time.Duration/between from to)) 60.0))

(defn- crash-loop-back-off?
  [status]
  (= "CrashLoopBackOff" (-> status :state :waiting :reason)))

(defn- looping?
  "Whether a container is in a restart loop now, as opposed to having restarted
   at some point in its life: it restarted at least 3 times, the last run ended
   within 30 minutes after lasting under 10, and it has not run 5 minutes
   since. The kubelet's CrashLoopBackOff is a loop by its own account."
  [status now]
  (or (crash-loop-back-off? status)
      (let [last (-> status :lastState :terminated)
            finished (instant (:finishedAt last))
            started (instant (:startedAt last))
            running (instant (-> status :state :running :startedAt))]
        (boolean
         (and now finished
              (<= loop-min-restarts (or (:restartCount status) 0))
              (< (minutes-between finished now) loop-recent-minutes)
              (or (nil? started) (< (minutes-between started finished) loop-fast-run-minutes))
              (not (and running (<= settled-minutes (minutes-between running now)))))))))

(defn- live?
  [pod]
  (not (contains? #{"Succeeded" "Failed"} (-> pod :status :phase))))

(defn- owner-key
  [pod]
  [(kubectl/namespace-of pod) (kubectl/workload-of pod)])

(defn reported-waiting?
  "Whether a container waits in a way this detective reports by name - a
   crash loop, or a waiting reason it rates - so another detective can leave
   the pod to it."
  [status]
  (boolean (or (crash-loop-back-off? status)
               (bad-waiting-reasons (-> status :state :waiting :reason)))))

(defn- outage-owners
  "The workloads where at least half of the live pods loop: there the loop is
   an outage, while one looping replica among healthy ones is degradation."
  [pods now]
  (set (for [[owner members] (group-by owner-key (filter live? pods))
             :let [loops (count (filter (fn [pod] (some #(looping? % now) (kubectl/container-statuses pod)))
                                        members))]
             :when (<= 1/2 (/ loops (count members)))]
         owner)))

(def ^:private oom-recent-hours 24)

(defn- recent-oom?
  "Whether a terminated state is an OOM kill within the last day; one without
   a finishedAt counts, since its age is unknown."
  [terminated now]
  (and (= "OOMKilled" (:reason terminated))
       (let [finished (instant (:finishedAt terminated))]
         (or (nil? finished) (nil? now)
             (< (minutes-between finished now) (* 60 oom-recent-hours))))))

(defn- last-exit
  [status]
  (when-let [last (-> status :lastState :terminated)]
    (format "last exit code %s (%s)" (:exitCode last) (or (:reason last) "no reason"))))

(defn- container-findings
  [pod now outage?]
  (let [component (kubectl/namespace-name-of pod)
        statuses (kubectl/container-statuses pod)
        loop-severity (if outage? :critical :warning)
        loop-hint (fn [status & more]
                    (clojure.string/join " - " (remove nil? (concat more [(last-exit status)
                                                                          (format "kubectl logs --previous -c %s" (:name status))]))))]
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
           :when (crash-loop-back-off? status)]
       {:severity loop-severity
        :component component
        :summary (format "container %s is waiting: CrashLoopBackOff" (:name status))
        :hint (loop-hint status (-> status :state :waiting :message))})
     (for [status statuses
           ;; A container restarted after an OOM kill records it in lastState,
           ;; which the kubelet keeps until the next termination, so only a
           ;; recent one counts; a one-shot container (restartPolicy Never) is
           ;; left terminated in its current state instead, and counts as is.
           :when (or (= "OOMKilled" (-> status :state :terminated :reason))
                     (recent-oom? (-> status :lastState :terminated) now))]
       {:severity :warning
        :component component
        :summary (format "container %s was OOMKilled" (:name status))
        :hint "compare the memory limit with actual usage"})
     (for [status statuses
           :when (and (not (crash-loop-back-off? status)) (looping? status now))]
       {:severity loop-severity
        :component component
        :summary (format "container %s keeps restarting (%d restarts)" (:name status) (:restartCount status))
        :hint (loop-hint status)}))))

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
        (onset/since
         {:severity (if stuck? :critical :warning)
          :component component
          :summary (if-let [top (first causes)]
                     (format "pod is unschedulable: %d node(s) %s" (:nodes top) (:explain top))
                     "pod is unschedulable")
          :hint (if (seq causes) (scheduling/describe causes) (:message condition))}
         since))

      ;; a pod bound to a node waits for its kubelet; pod-startup reads why
      (-> pod :spec :nodeName)
      nil

      :else
      (onset/since
       {:severity :warning
        :component component
        :summary "pod is stuck in Pending"
        :hint (:message condition)}
       (or since (-> pod :metadata :creationTimestamp))))))

(defn eviction-cause
  "Why the kubelet evicted a pod, read from the eviction message."
  [message]
  (let [message (str message)]
    (if-let [[_ resource] (re-find #"low on resource: ([\w./-]+?)\.?(?:\s|$)" message)]
      (format "node ran low on %s" resource)
      (if-let [[_ condition] (re-find #"had condition: \[([^\]]+)\]" message)]
        (format "node reported %s" condition)
        (if (re-find #"(?i)ephemeral (?:local )?storage|EmptyDir volume" message)
          "pod exceeded its ephemeral storage limit"
          "kubelet eviction")))))

(defn- eviction-finding
  "An evicted pod a controller owns has been replaced already; the finding
   explains the eviction. A bare pod is gone for good."
  [pod]
  (let [owned? (some :controller (-> pod :metadata :ownerReferences))
        message (-> pod :status :message)]
    {:severity (if owned? :info :warning)
     :component (kubectl/namespace-name-of pod)
     :summary (format "pod was evicted: %s" (eviction-cause message))
     :hint (str "on node " (or (-> pod :spec :nodeName) "unknown") " - " message
                (when-not owned? " - no controller recreates this pod"))}))

(defn detect-unhealthy-pods
  [evidence]
  (let [pods (kubectl/items-of (get evidence "pods"))
        now (:now evidence)
        outages (outage-owners pods now)]
    (concat
     (mapcat #(container-findings % now (contains? outages (owner-key %))) pods)
     (for [pod pods
           :when (and (= "Failed" (-> pod :status :phase)) (not= "Evicted" (-> pod :status :reason)))]
       {:severity :warning
        :component (kubectl/namespace-name-of pod)
        :summary (format "pod is in phase Failed (%s)" (or (-> pod :status :reason) "unknown reason"))})
     (for [pod pods
           :when (and (= "Failed" (-> pod :status :phase)) (= "Evicted" (-> pod :status :reason)))]
       (eviction-finding pod))
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
    :description "Unhealthy pods: restart loops, image pull errors, OOM kills, Pending/Failed"
    :requires ["pods"]
    :detect detect-unhealthy-pods}
   {:name "stuck-pods"
    :description "Pods stuck in Terminating - usually stuck mounts or finalizers"
    :requires ["pods"]
    :detect detect-stuck-terminating}])
