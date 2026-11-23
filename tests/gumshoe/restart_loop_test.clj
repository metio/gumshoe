;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.restart-loop-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.pods :as pods]))

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

(defn- container
  [& {:keys [restarts started finished running waiting exit reason]
      :or {restarts 4 started "2026-11-18T11:55:00Z" finished "2026-11-18T11:57:00Z" exit 1 reason "Error"}}]
  (cond-> {:name "app" :restartCount restarts
           :lastState {:terminated {:startedAt started :finishedAt finished :exitCode exit :reason reason}}
           :state (cond
                    waiting {:waiting {:reason waiting :message "back-off 5m0s restarting failed container"}}
                    running {:running {:startedAt running}}
                    :else {:running {:startedAt "2026-11-18T11:58:00Z"}})}))

(defn- pod
  [name status & {:keys [owner] :or {owner "web-7d9"}}]
  {:metadata {:namespace "shop" :name name :labels {:pod-template-hash (last (str/split owner #"-"))}
              :ownerReferences [{:kind "ReplicaSet" :name owner :controller true}]}
   :status {:phase "Running" :containerStatuses [status]}})

(defn- healthy [name] (pod name (container :restarts 0 :finished nil)))

(defn- run [& pods] (pods/detect-unhealthy-pods {:now now "pods" {:items pods}}))

(deftest a-loop-in-one-replica-of-many-is-a-warning
  (let [[finding :as findings] (run (pod "web-a" (container)) (healthy "web-b") (healthy "web-c"))]
    (is (= 1 (count findings)))
    (is (= :warning (:severity finding)))
    (is (= "container app keeps restarting (4 restarts)" (:summary finding)))
    (is (= "last exit code 1 (Error) - kubectl logs --previous -c app" (:hint finding)))))

(deftest a-loop-in-half-of-the-replicas-is-critical
  (is (= [:critical :critical]
         (map :severity (run (pod "web-a" (container)) (pod "web-b" (container :waiting "CrashLoopBackOff"))
                             (healthy "web-c") (healthy "web-d")))))
  (testing "a lone pod looping is an outage of what it serves"
    (is (= [:critical] (map :severity (run (pod "web-a" (container))))))))

(deftest crash-loop-back-off-shows-the-last-exit
  (let [[finding] (run (pod "web-a" (container :waiting "CrashLoopBackOff" :exit 137 :reason "Error")) (healthy "web-b")
                       (healthy "web-c"))]
    (is (= "container app is waiting: CrashLoopBackOff" (:summary finding)))
    (is (= :warning (:severity finding)))
    (is (str/includes? (:hint finding) "last exit code 137 (Error)"))))

(deftest restarts-in-the-past-are-not-a-loop
  (testing "the last crash was over 30 minutes ago"
    (is (empty? (run (pod "web-a" (container :restarts 40 :started "2026-11-18T10:00:00Z"
                                            :finished "2026-11-18T11:00:00Z" :running "2026-11-18T11:00:01Z"))))))
  (testing "it has been running for 5 minutes since"
    (is (empty? (run (pod "web-a" (container :running "2026-11-18T11:54:00Z" :finished "2026-11-18T11:53:59Z"
                                            :started "2026-11-18T11:50:00Z"))))))
  (testing "each run lasted longer than 10 minutes"
    (is (empty? (run (pod "web-a" (container :started "2026-11-18T11:30:00Z" :finished "2026-11-18T11:57:00Z"))))))
  (testing "fewer than 3 restarts"
    (is (empty? (run (pod "web-a" (container :restarts 2)))))))

(deftest two-revisions-of-one-deployment-share-their-loop-count
  (testing "a looping new ReplicaSet next to two healthy old pods is one of three"
    (is (= [:warning]
           (map :severity (run (pod "web-new" (container) :owner "web-8f1")
                               (healthy "web-a") (healthy "web-b")))))))

(deftest an-oom-kill-long-ago-is-history
  (let [oom (fn [finished] {:name "app" :restartCount 1
                            :lastState {:terminated {:reason "OOMKilled" :exitCode 137
                                                     :startedAt "2026-09-01T00:00:00Z" :finishedAt finished}}
                            :state {:running {:startedAt finished}}})
        summaries (fn [status] (set (map :summary (pods/detect-unhealthy-pods {:now now "pods" {:items [(pod "web-1" status)]}}))))]
    (is (not (contains? (summaries (oom "2026-09-26T10:00:00Z")) "container app was OOMKilled"))
        "53 days of healthy running since")
    (is (contains? (summaries (oom "2026-11-18T09:00:00Z")) "container app was OOMKilled")
        "three hours ago is worth a look")
    (is (contains? (summaries {:name "job" :state {:terminated {:reason "OOMKilled" :finishedAt "2026-10-01T00:00:00Z"}}})
                   "container job was OOMKilled")
        "a one-shot container still lying terminated is reported whatever its age")))
