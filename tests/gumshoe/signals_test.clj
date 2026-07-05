;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.signals-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.csi :as csi]
            [gumshoe.detectives.disruption :as disruption]
            [gumshoe.detectives.events :as events]))

(defn- summaries
  [findings]
  (set (map :summary findings)))

(deftest events-detective-test
  (let [now (java.time.Instant/parse "2026-07-04T12:00:00Z")
        evidence {:now now
                  "events" {:items [{:type "Warning"
                                     :reason "BackOff"
                                     :message "Back-off restarting failed container"
                                     :count 41
                                     :lastTimestamp "2026-07-04T11:59:00Z"
                                     :involvedObject {:namespace "moodle" :name "web-1"}}
                                    {:type "Warning"
                                     :reason "BackOff"
                                     :message "Back-off restarting failed container"
                                     :count 1
                                     :lastTimestamp "2026-07-04T11:30:00Z"
                                     :involvedObject {:namespace "moodle" :name "web-1"}}
                                    {:type "Warning"
                                     :reason "FailedScheduling"
                                     :message "0/3 nodes are available"
                                     :lastTimestamp "2026-07-04T05:00:00Z"
                                     :involvedObject {:namespace "old" :name "stale"}}
                                    {:type "Normal"
                                     :reason "Pulled"
                                     :lastTimestamp "2026-07-04T11:59:00Z"
                                     :involvedObject {:namespace "fine" :name "pod"}}]}}
        findings (events/detect-warning-events evidence)]
    (testing "recent warnings are grouped and counted, old and Normal events are ignored"
      (is (= [{:component "moodle/web-1" :summary "BackOff seen 42x in the last hour"}]
             (map #(select-keys % [:component :summary]) findings))))))

(deftest disruption-detective-test
  (testing "PDBs that allow zero disruptions are reported"
    (is (= #{"PodDisruptionBudget allows zero disruptions"}
           (summaries (disruption/detect-blocking-pdbs
                       {"poddisruptionbudgets"
                        {:items [{:metadata {:namespace "moodle" :name "web"}
                                  :status {:disruptionsAllowed 0 :expectedPods 2}}
                                 {:metadata {:namespace "fine" :name "app"}
                                  :status {:disruptionsAllowed 1 :expectedPods 3}}]}})))))
  (testing "HPAs pegged at their maximum are reported"
    (is (= #{"HorizontalPodAutoscaler is pegged at its maximum (5 replicas)"}
           (summaries (disruption/detect-pegged-hpas
                       {"horizontalpodautoscalers"
                        {:items [{:metadata {:namespace "moodle" :name "web"}
                                  :spec {:maxReplicas 5}
                                  :status {:currentReplicas 5}}
                                 {:metadata {:namespace "fine" :name "app"}
                                  :spec {:maxReplicas 5}
                                  :status {:currentReplicas 2}}]}}))))))

(deftest csi-detective-test
  (testing "detached volumes with an attach error are critical"
    (is (= [{:severity :critical
             :component "pvc-1234"
             :summary "volume is not attached to node worker-1"}]
           (map #(select-keys % [:severity :component :summary])
                (csi/detect-attachment-problems
                 {"volumeattachments"
                  {:items [{:metadata {:name "csi-abc"}
                            :spec {:nodeName "worker-1"
                                   :source {:persistentVolumeName "pvc-1234"}}
                            :status {:attached false
                                     :attachError {:message "rbd image busy"}}}
                           {:metadata {:name "csi-def"}
                            :spec {:nodeName "worker-2"}
                            :status {:attached true}}]}})))))
  (testing "failed and released volumes are reported"
    (is (= #{"PersistentVolume is Failed"
             "PersistentVolume is Released - its claim is gone but the data still exists"}
           (summaries (csi/detect-volume-problems
                       {"persistentvolumes"
                        {:items [{:metadata {:name "pv-a"} :status {:phase "Failed"}}
                                 {:metadata {:name "pv-b"} :status {:phase "Released"}}
                                 {:metadata {:name "pv-c"} :status {:phase "Bound"}}]}})))))
  (testing "a ceph driver registered on no schedulable node is critical"
    (let [evidence {"csidrivers" {:items [{:metadata {:name "rbd.csi.ceph.com"}}]}
                    "nodes" {:items [{:metadata {:name "worker-1"} :spec {}}
                                     {:metadata {:name "worker-2"} :spec {}}]}
                    "csinodes" {:items [{:metadata {:name "worker-1"} :spec {:drivers []}}
                                        {:metadata {:name "worker-2"} :spec {:drivers []}}]}}]
      (is (= [:critical] (map :severity (csi/detect-driver-registration evidence))))))
  (testing "tainted control-plane/role nodes are not expected to run the nodeplugin - no false positive"
    (let [evidence {"csidrivers" {:items [{:metadata {:name "rbd.csi.ceph.com"}}]}
                    "nodes" {:items [{:metadata {:name "worker-1"} :spec {}}
                                     {:metadata {:name "control-plane-1"}
                                      :spec {:taints [{:key "node-role.kubernetes.io/control-plane"
                                                       :effect "NoSchedule"}]}}
                                     {:metadata {:name "ingress-1"}
                                      :spec {:taints [{:key "role" :value "ingress" :effect "NoSchedule"}]}}]}
                    "csinodes" {:items [{:metadata {:name "worker-1"}
                                         :spec {:drivers [{:name "rbd.csi.ceph.com"}]}}
                                        {:metadata {:name "control-plane-1"} :spec {:drivers []}}
                                        {:metadata {:name "ingress-1"} :spec {:drivers []}}]}}]
      (is (empty? (csi/detect-driver-registration evidence))
          "a healthy CSI where only tainted nodes lack the driver is silent")))
  (testing "a schedulable worker missing the driver is an actionable warning"
    (let [evidence {"csidrivers" {:items [{:metadata {:name "rbd.csi.ceph.com"}}]}
                    "nodes" {:items [{:metadata {:name "worker-1"} :spec {}}
                                     {:metadata {:name "worker-2"} :spec {}}]}
                    "csinodes" {:items [{:metadata {:name "worker-1"}
                                         :spec {:drivers [{:name "rbd.csi.ceph.com"}]}}
                                        {:metadata {:name "worker-2"} :spec {:drivers []}}]}}]
      (let [findings (csi/detect-driver-registration evidence)]
        (is (= [:warning] (map :severity findings)))
        (is (clojure.string/includes? (:summary (first findings)) "worker-2"))))))

(deftest orphaned-local-volume-test
  (let [evidence {"nodes" {:items [{:metadata {:name "worker-1"}}]}
                  "persistentvolumes"
                  {:items [{:metadata {:name "pv-on-gone-node"}
                            :spec {:nodeAffinity {:required {:nodeSelectorTerms
                                                             [{:matchExpressions
                                                               [{:key "kubernetes.io/hostname"
                                                                 :values ["worker-gone"]}]}]}}}}
                           {:metadata {:name "pv-on-live-node"}
                            :spec {:nodeAffinity {:required {:nodeSelectorTerms
                                                             [{:matchExpressions
                                                               [{:key "kubernetes.io/hostname"
                                                                 :values ["worker-1"]}]}]}}}}
                           {:metadata {:name "pv-ceph"} :spec {}}]}}
        findings (csi/detect-orphaned-local-volumes evidence)]
    (is (= ["pv-on-gone-node"] (map :component findings)))
    (is (= [:critical] (map :severity findings)))))
