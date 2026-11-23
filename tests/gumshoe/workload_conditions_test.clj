;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.workload-conditions-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.cron :as cron]
            [gumshoe.detectives.disruption :as disruption]
            [gumshoe.detectives.pods :as pods]
            [gumshoe.detectives.workloads :as workloads]))

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

(defn- meta-of [name] {:namespace "shop" :name name})

(deftest a-rollout-past-its-deadline-is-critical
  (let [stuck {:metadata (meta-of "web") :spec {:progressDeadlineSeconds 300}
               :status {:conditions [{:type "Progressing" :status "False" :reason "ProgressDeadlineExceeded"
                                      :message "ReplicaSet \"web-8f1\" has timed out progressing."}]}}
        progressing (assoc-in stuck [:status :conditions 0] {:type "Progressing" :status "True" :reason "NewReplicaSetAvailable"})
        findings (workloads/detect-stalled-rollouts {"deployments" {:items [stuck progressing]}})]
    (is (= ["Deployment rollout is stuck: no progress within 300s"] (map :summary findings)))
    (is (= [:critical] (map :severity findings)))
    (is (str/starts-with? (:hint (first findings)) "ReplicaSet \"web-8f1\" has timed out progressing."))))

(deftest daemonset-placement-problems-are-reported
  (is (= ["DaemonSet has no pod on 2 node(s) it should run on"
          "DaemonSet runs 1 pod(s) on nodes it no longer targets"]
         (map :summary (workloads/detect-daemonset-problems
                        {"daemonsets" {:items [{:metadata (meta-of "agent")
                                                :status {:desiredNumberScheduled 5 :currentNumberScheduled 3
                                                         :numberMisscheduled 1 :numberUnavailable 0}}]}}))))
  (is (empty? (workloads/detect-daemonset-problems
               {"daemonsets" {:items [{:metadata (meta-of "agent")
                                       :status {:desiredNumberScheduled 3 :currentNumberScheduled 3}}]}}))))

(deftest schedule-intervals-round-up
  (doseq [[schedule hours] [["@hourly" 1] ["@daily" 24] ["@weekly" 168] ["*/15 * * * *" 1/4]
                            ["0 * * * *" 1] ["0 */6 * * *" 6] ["30 2 * * *" 24] ["0 3 * * 1" 168]
                            ["0 3 1 * *" 744] ["0 0 1 1 *" 8784] ["CRON_TZ=Europe/Berlin 0 3 * * *" 24]
                            ["* * * * *" 1/60]]]
    (is (= (long (* 3600 hours)) (.toSeconds ^java.time.Duration (cron/interval schedule))) schedule))
  (is (nil? (cron/interval "not a schedule"))))

(defn- cronjob
  [schedule & {:keys [last created suspend] :or {created "2026-01-01T00:00:00Z"}}]
  {:metadata (assoc (meta-of "backup") :creationTimestamp created)
   :spec (cond-> {:schedule schedule} suspend (assoc :suspend true))
   :status (cond-> {} last (assoc :lastScheduleTime last))})

(defn- stale [& cronjobs] (map :summary (workloads/detect-stale-cronjobs {:now now "cronjobs" {:items cronjobs}})))

(deftest a-cronjob-that-stopped-running-is-reported
  (testing "an hourly job gets a day before it counts as stopped"
    (is (empty? (stale (cronjob "@hourly" :last "2026-11-17T13:00:00Z"))))
    (is (= ["CronJob (@hourly) has not run since 2026-11-17T11:00:00Z"]
           (stale (cronjob "@hourly" :last "2026-11-17T11:00:00Z")))))
  (testing "a weekly job gets one and a half weeks"
    (is (empty? (stale (cronjob "0 3 * * 1" :last "2026-11-09T03:00:00Z"))))
    (is (= 1 (count (stale (cronjob "0 3 * * 1" :last "2026-11-05T03:00:00Z"))))))
  (testing "one that never ran is judged from its creation"
    (is (= ["CronJob (@daily) has never run"] (stale (cronjob "@daily"))))
    (is (empty? (stale (cronjob "@daily" :created "2026-11-18T00:00:00Z")))))
  (testing "a suspended or unreadable CronJob is left alone"
    (is (empty? (stale (cronjob "@daily" :suspend true) (cronjob "whenever"))))))

(defn- hpa [& conditions]
  {:metadata (meta-of "web") :spec {:maxReplicas 10} :status {:currentReplicas 2 :conditions (vec conditions)}})

(deftest an-hpa-that-cannot-read-metrics-names-the-cause
  (let [[finding] (disruption/detect-failing-hpas
                   {"horizontalpodautoscalers"
                    {:items [(hpa {:type "AbleToScale" :status "True"}
                                  {:type "ScalingActive" :status "False" :reason "FailedGetResourceMetric"
                                   :message "the HPA was unable to compute the replica count: failed to get cpu utilization: missing request for cpu in container app of Pod web-1"})]}})]
    (is (= :critical (:severity finding)))
    (is (= "HorizontalPodAutoscaler cannot read its metrics (FailedGetResourceMetric)" (:summary finding)))
    (is (str/starts-with? (:hint finding) "a container of the target has no cpu request")))
  (is (str/starts-with? (:hint (first (disruption/detect-failing-hpas
                                       {"horizontalpodautoscalers"
                                        {:items [(hpa {:type "ScalingActive" :status "False" :reason "FailedGetExternalMetric"
                                                       :message "unable to get external metric"})]}})))
                        "the external metrics adapter"))
  (is (= ["HorizontalPodAutoscaler cannot scale its target (FailedGetScale)"]
         (map :summary (disruption/detect-failing-hpas
                        {"horizontalpodautoscalers" {:items [(hpa {:type "AbleToScale" :status "False" :reason "FailedGetScale"
                                                                   :message "deployments/scale \"web\" not found"})]}}))))
  (testing "an HPA standing down for a target scaled to 0 is quiet"
    (is (empty? (disruption/detect-failing-hpas
                 {"horizontalpodautoscalers" {:items [(hpa {:type "ScalingActive" :status "False" :reason "ScalingDisabled"})]}})))))

(deftest evictions-are-explained
  (doseq [[message cause] [["The node was low on resource: memory. Threshold quantity: 100Mi, available: 50Mi." "node ran low on memory"]
                           ["The node was low on resource: ephemeral-storage. Threshold quantity: 1Gi." "node ran low on ephemeral-storage"]
                           ["The node had condition: [DiskPressure]. " "node reported DiskPressure"]
                           ["Pod ephemeral local storage usage exceeds the total limit of containers 1Gi. " "pod exceeded its ephemeral storage limit"]
                           ["Usage of EmptyDir volume \"cache\" exceeds the limit \"1Gi\". " "pod exceeded its ephemeral storage limit"]]]
    (is (= cause (pods/eviction-cause message)) message))
  (let [evicted (fn [owned?] {:metadata (cond-> (meta-of "web-1") owned? (assoc :ownerReferences [{:kind "ReplicaSet" :name "web-7d9" :controller true}]))
                              :spec {:nodeName "worker-2"}
                              :status {:phase "Failed" :reason "Evicted" :message "The node was low on resource: memory."}})
        findings (pods/detect-unhealthy-pods {:now now "pods" {:items [(evicted true) (evicted false)]}})]
    (is (= [["pod was evicted: node ran low on memory" :info]
            ["pod was evicted: node ran low on memory" :warning]]
           (map (juxt :summary :severity) findings)))
    (is (str/ends-with? (:hint (second findings)) "no controller recreates this pod"))))

(deftest a-question-mark-is-every-day
  (is (= (java.time.Duration/ofDays 1) (cron/interval "0 2 ? * *")))
  (is (= (java.time.Duration/ofDays 1) (cron/interval "0 2 * * ?"))))
