;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.workload-conditions-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
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

(def ^:private collected (java.time.Instant/parse "2026-11-24T20:30:00Z"))

(defn- failing-hpa
  "An HPA in shop/<name> whose condition of type is False since transitioned,
   which is a string, nil for a missing lastTransitionTime, or :absent."
  [name type reason transitioned]
  {:metadata (meta-of name) :spec {:maxReplicas 10}
   :status {:currentReplicas 1
            :conditions [(cond-> {:type type :status "False" :reason reason :message "no metrics returned"}
                           transitioned (assoc :lastTransitionTime transitioned))]}})

(defn- hpa-findings
  ([hpas] (hpa-findings hpas nil))
  ([hpas options]
   (vec (disruption/detect-failing-hpas
         (cond-> {:now collected "horizontalpodautoscalers" {:items hpas}}
           options (assoc "options" options))))))

(deftest a-scaling-active-false-younger-than-the-threshold-is-a-warning
  (let [[finding] (hpa-findings [(failing-hpa "young" "ScalingActive" "FailedGetPodsMetric" "2026-11-24T20:27:00Z")])]
    (is (= :warning (:severity finding)))
    (is (= "HorizontalPodAutoscaler cannot read its metrics (FailedGetPodsMetric), held under 10m" (:summary finding)))
    (is (str/includes? (:hint finding) "False for 3m"))
    (is (str/includes? (:hint finding) "may recover on its own"))
    (is (= "2026-11-24T20:27:00Z" (:since finding)))))

(deftest a-scaling-active-false-at-or-beyond-the-threshold-stays-critical
  (doseq [transitioned ["2026-11-24T20:00:00Z" "2026-11-24T20:20:00Z"]]
    (let [[finding] (hpa-findings [(failing-hpa "old" "ScalingActive" "FailedGetPodsMetric" transitioned)])]
      (is (= :critical (:severity finding)) transitioned)
      (is (= "HorizontalPodAutoscaler cannot read its metrics (FailedGetPodsMetric)" (:summary finding))))))

(deftest a-condition-of-unknown-age-stays-critical-and-says-so
  (doseq [transitioned [nil "yesterday"]]
    (let [[finding] (hpa-findings [(failing-hpa "unknown" "ScalingActive" "FailedGetPodsMetric" transitioned)])]
      (is (= :critical (:severity finding)) (pr-str transitioned))
      (is (str/includes? (:hint finding) "how long it has held is unknown") (pr-str transitioned))))
  (testing "without the collection time no age can be read either"
    (let [[finding] (disruption/detect-failing-hpas
                     {"horizontalpodautoscalers"
                      {:items [(failing-hpa "no-clock" "ScalingActive" "FailedGetPodsMetric" "2026-11-24T20:27:00Z")]}})]
      (is (= :critical (:severity finding)))
      (is (str/includes? (:hint finding) "how long it has held is unknown")))))

(deftest a-scaling-active-true-is-quiet
  (is (empty? (hpa-findings [{:metadata (meta-of "fine")
                              :status {:conditions [{:type "AbleToScale" :status "True" :lastTransitionTime "2026-11-24T20:29:00Z"}
                                                    {:type "ScalingActive" :status "True" :lastTransitionTime "2026-11-24T20:29:00Z"}]}}]))))

(deftest able-to-scale-false-follows-the-same-threshold
  (is (= [:warning] (map :severity (hpa-findings [(failing-hpa "young" "AbleToScale" "FailedGetScale" "2026-11-24T20:27:00Z")]))))
  (is (= [:critical] (map :severity (hpa-findings [(failing-hpa "old" "AbleToScale" "FailedGetScale" "2026-11-24T20:00:00Z")]))))
  (is (= [:critical] (map :severity (hpa-findings [(failing-hpa "unknown" "AbleToScale" "FailedGetScale" nil)])))))

(deftest the-threshold-comes-from-the-book-option
  (let [young (failing-hpa "young" "ScalingActive" "FailedGetPodsMetric" "2026-11-24T20:27:00Z")]
    (is (= [:critical] (map :severity (hpa-findings [young] {:hpa-grace-minutes 2}))))
    (is (= [:critical] (map :severity (hpa-findings [young] {:hpa-grace-minutes 0}))))
    (is (= ["HorizontalPodAutoscaler cannot read its metrics (FailedGetPodsMetric), held under 5m"]
           (map :summary (hpa-findings [young] {:hpa-grace-minutes 5}))))))

;; --- cron intervals over real calendars ------------------------------------------

(defn- longest-gap-in-days
  "The longest gap, in days, between two dates from 2024 to 2110 that satisfy
   fires? - a brute-force reading of the calendar the interval must bound."
  [fires?]
  (let [days (->> (iterate #(.plusDays ^java.time.LocalDate % 1) (java.time.LocalDate/of 2024 1 1))
                  (take-while #(.isBefore ^java.time.LocalDate % (java.time.LocalDate/of 2110 1 1)))
                  (filter fires?))]
    (->> (map (fn [a b] (.between java.time.temporal.ChronoUnit/DAYS a b)) days (rest days))
         (reduce max 0))))

(defn- days-of [schedule] (.toDays ^java.time.Duration (cron/interval schedule)))

(deftest a-late-day-of-month-bounds-the-months-it-skips
  (doseq [day (range 1 32)]
    (let [schedule (str "0 0 " day " * *")
          real (longest-gap-in-days #(= day (.getDayOfMonth ^java.time.LocalDate %)))]
      (is (<= real (days-of schedule)) schedule)
      (when (<= day 28)
        (is (= 31 (days-of schedule)) "a day every month has keeps the one-month bound")))))

(deftest a-day-31-cronjob-is-not-stopped-in-july
  (is (empty? (workloads/detect-stale-cronjobs
               {:now (java.time.Instant/parse "2026-07-20T00:00:00Z")
                "cronjobs" {:items [(cronjob "0 0 31 * *" :last "2026-05-31T00:00:00Z")]}}))))

(deftest day-of-month-lists-and-ranges-property
  (let [property (prop/for-all [days (gen/not-empty (gen/set (gen/choose 1 31) {:max-elements 3}))
                                as-range? gen/boolean]
                   (let [days (sort days)
                         field (if (and as-range? (> (count days) 1))
                                 (str (first days) "-" (last days))
                                 (clojure.string/join "," days))
                         wanted (if (and as-range? (> (count days) 1))
                                  (set (range (first days) (inc (last days))))
                                  (set days))
                         real (longest-gap-in-days #(contains? wanted (.getDayOfMonth ^java.time.LocalDate %)))]
                     (<= real (days-of (str "15 4 " field " * *")))))]
    (is (:pass? (tc/quick-check 60 property :seed 20261125)))))

(deftest twenty-ninth-of-february-spans-a-century-year
  (let [real (longest-gap-in-days #(and (= 2 (.getMonthValue ^java.time.LocalDate %))
                                        (= 29 (.getDayOfMonth ^java.time.LocalDate %))))]
    (is (= 2921 real) "2096-02-29 to 2104-02-29")
    (doseq [schedule ["0 0 29 2 *" "0 0 29 FEB *" "0 0 29 1-3 *" "0 0 */2 */1 *"]]
      (is (<= real (days-of schedule)) schedule)))
  (testing "a restricted month that can not be February keeps the year bound"
    (is (= 366 (days-of "0 0 31 1,3 *")))
    (is (= 366 (days-of "0 0 31 JAN *")))
    (is (= 366 (days-of "0 0 1 2 *")))))

(defn- robfig-hours
  "The hours robfig/cron fires a */n hour field at: 0, n, 2n ... up to 23."
  [n]
  (filter #(zero? (mod % n)) (range 24)))

(defn- longest-gap-in-a-wrapping-field
  [fired range]
  (let [times (concat fired [(+ range (first fired))])]
    (reduce max (map - (rest times) times))))

(deftest hour-and-minute-steps-bound-the-real-gap
  (doseq [n (range 1 200)]
    (let [real-hours (longest-gap-in-a-wrapping-field (robfig-hours n) 24)
          hours (.toHours ^java.time.Duration (cron/interval (str "0 */" n " * * *")))
          real-minutes (longest-gap-in-a-wrapping-field (filter #(zero? (mod % n)) (range 60)) 60)
          minutes (.toMinutes ^java.time.Duration (cron/interval (str "*/" n " * * * *")))]
      (is (<= real-hours hours 24) (str "*/" n " hours"))
      (is (<= real-minutes minutes 60) (str "*/" n " minutes")))))

(deftest huge-steps-are-the-coarser-unit
  (doseq [n ["24" "60" "9999999999999999" "99999999999999999999999999"]]
    (is (= (java.time.Duration/ofDays 1) (cron/interval (str "0 */" n " * * *"))) n))
  (doseq [n ["60" "61" "10000000000000000" "99999999999999999999999999"]]
    (is (= (java.time.Duration/ofHours 1) (cron/interval (str "*/" n " * * * *"))) n))
  (testing "a huge step no longer stops the stale check for every other CronJob"
    (is (= ["CronJob (@hourly) has not run since 2026-11-17T11:00:00Z"]
           (stale (cronjob "0 */9999999999999999 * * *" :last "2026-11-18T00:00:00Z")
                  (cronjob "*/99999999999999999999 * * * *" :last "2026-11-18T11:30:00Z")
                  (cronjob "@hourly" :last "2026-11-17T11:00:00Z"))))))

(deftest a-zero-step-never-fires
  (doseq [schedule ["*/0 * * * *" "0 */0 * * *" "0 0 */0 * *" "0 0 1 */00 *" "0 0 * * */0"
                    "0 0 1-10/0 * *" "0,5/0 * * * *"]]
    (is (nil? (cron/interval schedule)) schedule)))

(def ^:private field-item
  (gen/one-of [(gen/return "*") (gen/return "?")
               (gen/fmap str (gen/choose 0 40))
               (gen/fmap (fn [[a b]] (str a "-" b)) (gen/tuple (gen/choose 0 31) (gen/choose 0 31)))
               (gen/fmap #(str "*/" %) (gen/one-of [(gen/choose 0 100)
                                                     (gen/return "99999999999999999999")]))
               (gen/fmap (fn [[a b]] (str a "/" b)) (gen/tuple (gen/choose 0 31) (gen/choose 0 70)))
               (gen/elements ["MON" "FEB" "JAN-MAR" "L" "x"])]))

(deftest every-schedule-reads-or-is-nil-property
  (let [field (gen/fmap #(clojure.string/join "," %) (gen/not-empty (gen/vector field-item 1 3)))
        property (prop/for-all [fields (gen/vector field 5)]
                   (let [interval (cron/interval (clojure.string/join " " fields))]
                     (or (nil? interval)
                         (and (pos? (.toSeconds ^java.time.Duration interval))
                              ;; the stale check scales it and subtracts it from now
                              (some? (.minus (java.time.Instant/parse "2026-11-18T12:00:00Z")
                                             (.multipliedBy ^java.time.Duration interval 2)))))))]
    (is (:pass? (tc/quick-check 1000 property :seed 20261125)))))
