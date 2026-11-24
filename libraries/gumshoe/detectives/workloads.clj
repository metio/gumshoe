;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.workloads
  "Detectives for controllers: deployments, statefulsets, and daemonsets whose
   pods are not all up, rollouts past their deadline, jobs that failed and
   CronJobs that stopped running."
  (:require [gumshoe.conditions :as conditions]
            [gumshoe.cron :as cron]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(defn- replica-findings
  [resources kind]
  (for [resource resources
        :let [wanted (or (-> resource :spec :replicas) 0)
              ready (or (-> resource :status :readyReplicas) 0)]
        :when (< ready wanted)]
    {:severity (if (zero? ready) :critical :warning)
     :component (kubectl/namespace-name-of resource)
     :summary (format "%s has %d of %d replicas ready" kind ready wanted)}))

(defn detect-deployment-problems
  [evidence]
  (replica-findings (kubectl/items-of (get evidence "deployments")) "Deployment"))

(defn detect-statefulset-problems
  [evidence]
  (replica-findings (kubectl/items-of (get evidence "statefulsets")) "StatefulSet"))

(defn detect-stalled-rollouts
  [evidence]
  (for [deployment (kubectl/items-of (get evidence "deployments"))
        :let [progressing (conditions/condition deployment "Progressing")]
        :when (and (= "False" (:status progressing)) (= "ProgressDeadlineExceeded" (:reason progressing)))]
    (onset/since
     {:severity :critical
      :component (kubectl/namespace-name-of deployment)
      :summary (format "Deployment rollout is stuck: no progress within %ss"
                       (or (-> deployment :spec :progressDeadlineSeconds) 600))
      :hint (str (:message progressing)
                 " - the new pods never became ready; the pods, pod-creation and pod-startup detectives say why")}
     (:lastTransitionTime progressing))))

(defn detect-daemonset-problems
  [evidence]
  (for [daemonset (kubectl/items-of (get evidence "daemonsets"))
        :let [{:keys [numberUnavailable numberMisscheduled desiredNumberScheduled currentNumberScheduled]
               :or {numberUnavailable 0 numberMisscheduled 0}} (:status daemonset)
              unscheduled (if (and desiredNumberScheduled currentNumberScheduled)
                            (- desiredNumberScheduled currentNumberScheduled) 0)
              component (kubectl/namespace-name-of daemonset)]
        finding [(when (pos? numberUnavailable)
                   {:severity :warning
                    :component component
                    :summary (format "DaemonSet has %d unavailable pods" numberUnavailable)})
                 (when (pos? unscheduled)
                   {:severity :warning
                    :component component
                    :summary (format "DaemonSet has no pod on %d node(s) it should run on" unscheduled)
                    :hint "a node it targets does not admit the pod - check the pod's resources and the node's taints"})
                 (when (pos? numberMisscheduled)
                   {:severity :warning
                    :component component
                    :summary (format "DaemonSet runs %d pod(s) on nodes it no longer targets" numberMisscheduled)
                    :hint "node labels or the DaemonSet's nodeSelector changed - the pods stay until deleted"})]
        :when finding]
    finding))

(def ^:private stale-floor (java.time.Duration/ofHours 24))

(defn- stale-after
  "How long a CronJob may go without a run before it counts as stopped: one
   and a half intervals, and never under a day so a slow catch-up after an
   outage is no alarm."
  [schedule]
  (when-let [interval (cron/interval schedule)]
    (let [scaled (java.time.Duration/ofSeconds (long (* 1.5 (.toSeconds ^java.time.Duration interval))))]
      (if (pos? (compare scaled stale-floor)) scaled stale-floor))))

(defn detect-stale-cronjobs
  [evidence]
  (let [now ^java.time.Instant (:now evidence)]
    (for [cronjob (kubectl/items-of (get evidence "cronjobs"))
          :let [schedule (-> cronjob :spec :schedule)
                limit (stale-after schedule)
                last-run (some-> cronjob :status :lastScheduleTime java.time.Instant/parse)
                since (or last-run (some-> cronjob :metadata :creationTimestamp java.time.Instant/parse))]
          :when (and limit since now (not (-> cronjob :spec :suspend))
                     (.isBefore since (.minus now ^java.time.Duration limit)))]
      {:severity :warning
       :component (kubectl/namespace-name-of cronjob)
       :summary (if last-run
                  (format "CronJob (%s) has not run since %s" schedule last-run)
                  (format "CronJob (%s) has never run" schedule))
       :hint (str "a run missed by more than startingDeadlineSeconds is skipped, and without that deadline the controller"
                  " gives up after 100 missed runs - check the CronJob's events and the kube-controller-manager")})))

(defn detect-failed-jobs
  [evidence]
  (for [job (kubectl/items-of (get evidence "jobs"))
        :let [failed (conditions/condition job "Failed")]
        :when (= "True" (:status failed))]
    (onset/since
     {:severity :warning
      :component (kubectl/namespace-name-of job)
      :summary (format "Job failed (%s)" (or (:reason failed) "unknown reason"))
      :hint (:message failed)}
     (:lastTransitionTime failed))))

(def detectives
  [{:name "deployments"
    :description "Deployments with missing replicas"
    :requires ["deployments"]
    :detect detect-deployment-problems}
   {:name "statefulsets"
    :description "StatefulSets with missing replicas"
    :requires ["statefulsets"]
    :detect detect-statefulset-problems}
   {:name "rollouts"
    :description "Deployments whose rollout passed its progress deadline"
    :requires ["deployments"]
    :detect detect-stalled-rollouts}
   {:name "daemonsets"
    :description "DaemonSets with unavailable, unscheduled or misscheduled pods"
    :requires ["daemonsets"]
    :detect detect-daemonset-problems}
   {:name "jobs"
    :description "Jobs that failed"
    :requires ["jobs"]
    :detect detect-failed-jobs}
   {:name "cronjobs"
    :description "CronJobs that stopped running"
    :requires ["cronjobs"]
    :detect detect-stale-cronjobs}])
