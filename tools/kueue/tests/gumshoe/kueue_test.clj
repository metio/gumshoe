;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.kueue-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.kueue :as kueue]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.kueue]))

(def ^:private now (java.time.Instant/parse "2026-11-19T12:00:00Z"))

(defn- condition [type status & {:keys [reason message at] :or {at "2026-11-19T11:00:00Z"}}]
  (cond-> {:type type :status status :lastTransitionTime at}
    reason (assoc :reason reason)
    message (assoc :message message)))

(defn- queue [namespace name & conditions]
  {:metadata (cond-> {:name name} namespace (assoc :namespace namespace))
   :status {:conditions (vec conditions)}})

(deftest queues
  (let [findings (kueue/detect-queues
                  {kueue/clusterqueue-type {:items [(queue nil "team-a" (condition "Active" "False" :reason "FlavorNotFound"
                                                                                      :message "Can't admit new workloads: references missing ResourceFlavor(s): gpu."))
                                                    (queue nil "team-b" (condition "Active" "True"))
                                                    (queue nil "paused" (condition "Active" "False" :reason "Stopped"))]}
                   kueue/localqueue-type {:items [(queue "ml" "default" (condition "Active" "False" :reason "ClusterQueueDoesNotExist"
                                                                                  :message "Can't submit new workloads to clusterQueue"))]}})
        by-component (into {} (map (juxt :component identity)) findings)]
    (is (= 3 (count findings)))
    (is (= [:critical "ClusterQueue is not Active (FlavorNotFound)"] ((juxt :severity :summary) (by-component "team-a"))))
    (is (str/starts-with? (:hint (by-component "team-a")) "Can't admit new workloads: references missing ResourceFlavor(s): gpu. - Kueue admits no workload"))
    (is (= "2026-11-19T11:00:00Z" (:since (by-component "team-a"))))
    (is (= [:info "ClusterQueue is stopped"] ((juxt :severity :summary) (by-component "paused"))))
    (is (str/includes? (:hint (by-component "paused")) "stopPolicy"))
    (is (= [:warning "LocalQueue is not Active (ClusterQueueDoesNotExist)"] ((juxt :severity :summary) (by-component "ml/default")))))
  (testing "a queue without conditions yet is not judged"
    (is (empty? (kueue/detect-queues {kueue/clusterqueue-type {:items [(queue nil "new")]}})))))

(defn- workload [& {:keys [created conditions active finished queue] :or {created "2026-11-19T11:30:00Z" queue "default"}}]
  {:metadata {:namespace "ml" :name "job-train-1a2b3" :creationTimestamp created}
   :spec (cond-> {:queueName queue} (some? active) (assoc :active active))
   :status {:conditions (cond-> (vec conditions) finished (conj (condition "Finished" "True")))}})

(deftest workloads-that-can-never-be-admitted
  (doseq [reason (sort kueue/never-fits)]
    (let [[finding] (kueue/workload-findings
                     (workload :conditions [(condition "QuotaReserved" "False" :reason reason
                                                       :message "couldn't assign flavors to pod set main: insufficient quota for nvidia.com/gpu in flavor gpu, request > maximum capacity")])
                     now)]
      (is (= :warning (:severity finding)) reason)
      (is (= (format "Workload cannot be admitted (%s)" reason) (:summary finding)))
      (is (str/includes? (:hint finding) "waiting does not help"))
      (is (str/ends-with? (:hint finding) "LocalQueue default's ClusterQueue"))))
  (testing "reported at once, however young"
    (is (seq (kueue/workload-findings (workload :created "2026-11-19T11:59:00Z"
                                                :conditions [(condition "QuotaReserved" "False" :reason "ExceedsMaxQuota")]) now)))))

(deftest workloads-waiting-for-quota
  (let [[finding] (kueue/workload-findings
                   (workload :created "2026-11-19T10:00:00Z"
                             :conditions [(condition "QuotaReserved" "False" :reason "WaitingForQuota" :message "insufficient unused quota for cpu in flavor default, 4 more needed")])
                   now)]
    (is (= :info (:severity finding)))
    (is (= "Workload has waited 120 minutes for quota" (:summary finding)))
    (is (str/starts-with? (:hint finding) "insufficient unused quota for cpu"))
    (is (= "2026-11-19T10:00:00Z" (:since finding))))
  (testing "an hour is normal for a batch queue"
    (is (empty? (kueue/workload-findings (workload :created "2026-11-19T11:01:00Z"
                                                   :conditions [(condition "QuotaReserved" "False" :reason "WaitingForQuota")]) now))))
  (testing "a workload without any condition yet waits too"
    (is (= "Workload has waited 120 minutes for quota"
           (:summary (first (kueue/workload-findings (workload :created "2026-11-19T10:00:00Z") now)))))))

(deftest admitted-workloads-whose-pods-do-not-start
  (let [admitted [(condition "QuotaReserved" "True") (condition "Admitted" "True")]
        [finding] (kueue/workload-findings
                   (workload :conditions (conj admitted (condition "PodsReady" "False" :reason "WaitForStart" :message "Not all pods are ready or succeeded" :at "2026-11-19T11:30:00Z")))
                   now)]
    (is (= :warning (:severity finding)))
    (is (= "Workload is admitted but its pods are not ready" (:summary finding)))
    (is (str/starts-with? (:hint finding) "Not all pods are ready or succeeded - its pods hold quota"))
    (is (empty? (kueue/workload-findings (workload :conditions (conj admitted (condition "PodsReady" "False" :at "2026-11-19T11:50:00Z"))) now))
        "pods get time to start")
    (is (empty? (kueue/workload-findings (workload :conditions (conj admitted (condition "PodsReady" "True"))) now)))
    (is (empty? (kueue/workload-findings (workload :conditions admitted) now)) "no PodsReady condition when waitForPodsReady is off")))

(deftest settled-workloads-say-nothing
  (let [stuck [(condition "QuotaReserved" "False" :reason "ExceedsMaxQuota")]]
    (is (empty? (kueue/workload-findings (workload :conditions stuck :finished true) now)))
    (is (empty? (kueue/workload-findings (workload :conditions stuck :active false) now)) "deactivated on purpose")
    (is (empty? (kueue/workload-findings (assoc-in (workload :conditions stuck) [:metadata :deletionTimestamp] "2026-11-19T11:00:00Z") now)))
    (is (seq (kueue/workload-findings (workload :conditions stuck :active true) now)))))

(deftest the-package-joins-the-workloads-scope
  (is (every? (set (map :name (registry/for-scope :workloads))) (map :name kueue/detectives)))
  (is (empty? (detective/run-detectives kueue/detectives {:now now})) "a cluster without Kueue"))
