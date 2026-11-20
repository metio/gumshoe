;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.autoscaler-status-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.autoscaler :as autoscaler]
            [gumshoe.detectives.registry :as registry]))

(def ^:private now (java.time.Instant/parse "2026-11-20T12:00:00Z"))

(def ^:private healthy-status
  "time: 2026-11-20 11:59:55.123456789 +0000 UTC
autoscalerStatus: Running
clusterWide:
  health:
    status: Healthy
    nodeCounts:
      registered:
        total: 5
        ready: 5
        notStarted: 0
        suspended: 0
        unready:
          total: 0
          resourceUnready: 0
      longUnregistered: 0
      unregistered: 0
    lastProbeTime: \"2026-11-20T11:59:55Z\"
    lastTransitionTime: \"2026-11-18T08:00:00Z\"
  scaleUp:
    status: NoActivity
    lastProbeTime: \"2026-11-20T11:59:55Z\"
    lastTransitionTime: \"2026-11-18T08:00:00Z\"
  scaleDown:
    status: NoCandidates
    lastProbeTime: \"2026-11-20T11:59:55Z\"
    lastTransitionTime: \"2026-11-18T08:00:00Z\"
nodeGroups:
- name: pool-a
  health:
    status: Healthy
    nodeCounts:
      registered:
        total: 3
        ready: 3
        notStarted: 0
        suspended: 0
        unready:
          total: 0
          resourceUnready: 0
      longUnregistered: 0
      unregistered: 0
    cloudProviderTarget: 3
    minSize: 1
    maxSize: 10
    lastProbeTime: \"2026-11-20T11:59:55Z\"
    lastTransitionTime: \"2026-11-18T08:00:00Z\"
  scaleUp:
    status: NoActivity
    lastProbeTime: \"2026-11-20T11:59:55Z\"
    lastTransitionTime: \"2026-11-18T08:00:00Z\"
  scaleDown:
    status: NoCandidates
    lastProbeTime: \"2026-11-20T11:59:55Z\"
    lastTransitionTime: \"2026-11-18T08:00:00Z\"
")

(defn- configmap [status & {:keys [updated] :or {updated "2026-11-20 11:59:55.123456789 +0000 UTC"}}]
  {:metadata {:namespace "kube-system" :name "cluster-autoscaler-status"
              :annotations {(keyword autoscaler/last-updated-annotation) updated}}
   :data {:status status}})

(defn- detect [cm]
  (vec (autoscaler/detect-autoscaler-status {"cluster-autoscaler-status" cm :now now})))

(deftest timestamps-in-the-annotation-format
  (is (= (java.time.Instant/parse "2026-11-20T11:59:55.123456789Z")
         (autoscaler/parse-update-time "2026-11-20 11:59:55.123456789 +0000 UTC")))
  (is (= (java.time.Instant/parse "2026-11-20T10:59:55Z")
         (autoscaler/parse-update-time "2026-11-20 11:59:55 +0100 CET")) "no fraction, another zone")
  (is (= (java.time.Instant/parse "2026-11-20T11:59:55.5Z")
         (autoscaler/parse-update-time "2026-11-20 11:59:55.5 +0000 UTC")) "trailing zeros dropped")
  (is (nil? (autoscaler/parse-update-time "2026-11-20T11:59:55Z")))
  (is (nil? (autoscaler/parse-update-time nil))))

(deftest a-healthy-autoscaler-is-quiet
  (is (= [] (detect (configmap healthy-status)))))

(deftest no-autoscaler-no-findings
  (is (= [] (detect nil))))

(deftest a-stale-status-means-the-autoscaler-stopped
  (let [[finding & more] (detect (configmap healthy-status :updated "2026-11-20 11:40:00 +0000 UTC"))]
    (is (nil? more))
    (is (= :critical (:severity finding)))
    (is (= "the cluster autoscaler has stopped updating its status" (:summary finding)))
    (is (= "2026-11-20T11:40:00Z" (:since finding))))
  (is (= [] (detect (configmap healthy-status :updated "2026-11-20 11:51:00 +0000 UTC"))) "nine minutes is fine")
  (testing "a stale status hides the verdicts of its last loop"
    (let [unhealthy (str/replace-first healthy-status "status: Healthy" "status: Unhealthy")]
      (is (= ["the cluster autoscaler has stopped updating its status"]
             (map :summary (detect (configmap unhealthy :updated "2026-11-19 11:00:00 +0000 UTC")))))))
  (is (= [] (detect (configmap healthy-status :updated "garbage"))) "an unreadable time is not staleness"))

(deftest an-unhealthy-cluster-halts-scaling
  (let [status (-> healthy-status
                   (str/replace-first "status: Healthy" "status: Unhealthy")
                   (str/replace-first "ready: 5" "ready: 1")
                   (str/replace-first "unregistered: 0" "unregistered: 2")
                   (str/replace-first "lastTransitionTime: \"2026-11-18T08:00:00Z\"" "lastTransitionTime: \"2026-11-20T11:30:00Z\""))
        [finding] (detect (configmap status))]
    (is (= "the cluster autoscaler halts all scaling: too many nodes are unready" (:summary finding)))
    (is (= :critical (:severity finding)))
    (is (str/starts-with? (:hint finding) "1 of 5 registered nodes ready, 2 not registered yet - it stops once"))
    (is (= "2026-11-20T11:30:00Z" (:since finding)))))

(deftest backoff-names-the-cloud-providers-error
  (let [status (str/replace healthy-status
                            "  scaleUp:\n    status: NoActivity\n    lastProbeTime: \"2026-11-20T11:59:55Z\"\n    lastTransitionTime: \"2026-11-18T08:00:00Z\"\n  scaleDown:\n    status: NoCandidates\n    lastProbeTime: \"2026-11-20T11:59:55Z\"\n    lastTransitionTime: \"2026-11-18T08:00:00Z\"\n"
                            "  scaleUp:\n    status: Backoff\n    backoffInfo:\n      errorCode: QUOTA_EXCEEDED\n      errorMessage: 'Instance quota exceeded in region'\n    lastProbeTime: \"2026-11-20T11:59:55Z\"\n    lastTransitionTime: 2026-11-20T11:45:00Z\n  scaleDown:\n    status: NoCandidates\n")
        findings (detect (configmap status))]
    (is (= 1 (count findings)) "only the node group, whose scaleUp block was replaced")
    (is (= {:severity :critical
            :component "node group pool-a"
            :summary "the cluster autoscaler backs off scaling this node group up (QUOTA_EXCEEDED)"
            :hint "Instance quota exceeded in region - a scale-up failed, so pods waiting for this group's nodes stay Pending until the backoff ends - target 3, min 1, max 10"
            :since "2026-11-20T11:45:00Z"}
           (first findings))
        "an unquoted time reads as a timestamp too")))

(deftest backoff-without-error-details
  (is (= "the cluster autoscaler backs off scaling this node group up"
         (:summary (first (autoscaler/detect-autoscaler-status
                           {"cluster-autoscaler-status"
                            (configmap "nodeGroups:\n- name: g\n  health: {status: Healthy}\n  scaleUp: {status: Backoff}\n")
                            :now now}))))))

(deftest an-unhealthy-node-group-is-not-scaled-up
  (let [status "autoscalerStatus: Running
clusterWide: {health: {status: Healthy}}
nodeGroups:
- name: gpu
  health:
    status: Unhealthy
    nodeCounts: {registered: {total: 2, ready: 0}, longUnregistered: 1, unregistered: 0}
    cloudProviderTarget: 3
    minSize: 3
    maxSize: 6
    lastTransitionTime: \"2026-11-20T10:00:00Z\"
  scaleUp: {status: Unhealthy}
"
        [finding] (detect (configmap status))]
    (is (= :warning (:severity finding)))
    (is (= "node group gpu" (:component finding)))
    (is (= "0 of 2 registered nodes ready, 1 never registered, target 3, min 3, max 6 - nodes the cloud provider created are not registering or not becoming Ready"
           (:hint finding)))))

(deftest text-status-of-older-releases-is-left-alone
  (is (= [] (detect (configmap "Cluster-autoscaler status at 2026-11-20 11:59:55 +0000 UTC:\nCluster-wide:\n  Health: Unhealthy (ready=1)\n"))))
  (is (nil? (autoscaler/parse-status (configmap "Cluster-autoscaler status at 2026-11-20:\nCluster-wide:\n"))))
  (is (nil? (autoscaler/parse-status (configmap ""))))
  (is (nil? (autoscaler/parse-status {}))))

(deftest joins-the-platform-scope
  (is (some #(= "cluster-autoscaler" (:name %)) (registry/for-scope :platform))))
