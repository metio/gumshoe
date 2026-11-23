;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pod-startup-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.pod-startup :as pod-startup]
            [gumshoe.detectives.pods :as pods]))

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

(defn- pod
  [name & {:keys [bound phase node] :or {bound "2026-11-18T11:55:00Z" phase "Pending" node "worker-1"}}]
  {:metadata {:namespace "shop" :name name :creationTimestamp bound}
   :spec (cond-> {} node (assoc :nodeName node))
   :status {:phase phase
            :conditions [{:type "PodScheduled" :status "True" :lastTransitionTime bound}]}})

(defn- event
  [pod-name reason message & {:keys [at] :or {at "2026-11-18T11:58:00Z"}}]
  {:type "Warning" :reason reason :message message :lastTimestamp at
   :involvedObject {:kind "Pod" :namespace "shop" :name pod-name}})

(defn- run
  [pods events]
  (pod-startup/detect-stalled-startups {:now now "pods" {:items pods} "events" {:items events}}))

(deftest ip-exhaustion-is-told-apart-from-other-sandbox-failures
  (let [[finding] (run [(pod "web-1")]
                       [(event "web-1" "FailedCreatePodSandBox"
                               "Failed to create pod sandbox: plugin type=\"calico\" failed (add): failed to request IPv4 addresses: failed to assign an IP address to container")])]
    (is (= :critical (:severity finding)))
    (is (str/starts-with? (:summary finding) "pod gets no IP address"))
    (is (str/starts-with? (:hint finding) "on node worker-1 - free addresses")))
  (let [[finding] (run [(pod "web-1")]
                       [(event "web-1" "FailedCreatePodSandBox" "rpc error: code = Unknown desc = failed to setup network for sandbox")])]
    (is (= :warning (:severity finding)))
    (is (= "the pod sandbox cannot be created" (:summary finding)))))

(deftest a-volume-held-by-another-node-is-critical
  (let [[finding] (run [(pod "db-0")]
                       [(event "db-0" "FailedAttachVolume"
                               "Multi-Attach error for volume \"pvc-1\" Volume is already exclusively attached to one node and can't be attached to another")])]
    (is (= :critical (:severity finding)))
    (is (= "a volume of the pod is still attached to another node" (:summary finding)))))

(deftest attach-and-mount-failures-are-warnings-that-age-into-critical
  (is (= [["a volume of the pod cannot be attached" :warning]
          ["a volume of the pod cannot be mounted" :warning]]
         (map (juxt :summary :severity)
              (run [(pod "web-1")]
                   [(event "web-1" "FailedMount" "MountVolume.SetUp failed for volume \"config\" : configmap \"web\" not found")
                    (event "web-1" "FailedAttachVolume" "AttachVolume.Attach failed for volume \"pvc-2\" : timed out")]))))
  (testing "after 30 minutes bound, the same failure is critical"
    (is (= [:critical]
           (map :severity (run [(pod "web-1" :bound "2026-11-18T11:00:00Z")]
                               [(event "web-1" "FailedMount" "configmap \"web\" not found")]))))))

(deftest repeated-events-of-one-cause-are-reported-once-with-the-newest-message
  (let [findings (run [(pod "web-1")]
                      [(event "web-1" "FailedMount" "secret \"old\" not found" :at "2026-11-18T11:56:00Z")
                       (event "web-1" "FailedMount" "secret \"new\" not found" :at "2026-11-18T11:59:00Z")])]
    (is (= 1 (count findings)))
    (is (str/includes? (:hint (first findings)) "secret \"new\" not found"))))

(deftest a-pod-without-events-is-reported-once-it-stalls
  (is (empty? (run [(pod "web-1")] [])) "five minutes bound is still a normal start")
  (is (= [:warning] (map :severity (run [(pod "web-1" :bound "2026-11-18T11:45:00Z")] []))))
  (is (= [:critical] (map :severity (run [(pod "web-1" :bound "2026-11-18T11:00:00Z")] [])))))

(deftest only-bound-pending-pods-are-judged
  (is (empty? (run [(pod "web-1" :phase "Running" :bound "2026-11-18T09:00:00Z")
                    (pod "web-2" :node nil :bound "2026-11-18T09:00:00Z")]
                   [(event "web-1" "FailedMount" "old trouble")
                    (event "web-2" "FailedMount" "old trouble")])))
  (testing "events older than an hour are ignored"
    (is (empty? (run [(pod "web-1")] [(event "web-1" "FailedMount" "x" :at "2026-11-18T10:00:00Z")])))))

(deftest the-pods-detective-leaves-bound-pods-to-pod-startup
  (is (empty? (pods/detect-unhealthy-pods {:now now "pods" {:items [(pod "web-1")]}}))))

(deftest a-failing-init-container-is-named-instead-of-the-kubelet
  (let [stuck (-> (pod "web-1" :bound "2026-11-18T11:00:00Z")
                  (assoc-in [:status :initContainerStatuses]
                            [{:name "migrate" :restartCount 12
                              :state {:waiting {:reason "CrashLoopBackOff" :message "back-off 5m0s"}}}])
                  (assoc-in [:status :containerStatuses]
                            [{:name "app" :restartCount 0 :state {:waiting {:reason "PodInitializing"}}}]))]
    (testing "the pods detective reports the init container"
      (is (= ["container migrate is waiting: CrashLoopBackOff"]
             (map :summary (pods/detect-unhealthy-pods {:now now "pods" {:items [stuck]}})))))
    (testing "and pod-startup does not blame the node for it"
      (is (empty? (run [stuck] []))))))

(deftest a-crash-looping-native-sidecar-is-reported
  (let [running {:metadata {:namespace "shop" :name "web-2"}
                 :spec {:nodeName "worker-1"}
                 :status {:phase "Running"
                          :initContainerStatuses [{:name "proxy" :restartCount 9
                                                   :state {:waiting {:reason "CrashLoopBackOff"}}}]
                          :containerStatuses [{:name "app" :restartCount 0 :ready true
                                               :state {:running {:startedAt "2026-11-18T10:00:00Z"}}}]}}]
    (is (= ["container proxy is waiting: CrashLoopBackOff"]
           (map :summary (pods/detect-unhealthy-pods {:now now "pods" {:items [running]}}))))))
