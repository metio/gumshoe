;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.storage-conditions-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.storage :as storage]))

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

(defn- claim
  [name & {:keys [phase class created conditions allocated volume]
           :or {phase "Bound" class "ceph" created "2026-11-18T10:00:00Z"}}]
  {:metadata {:namespace "shop" :name name :creationTimestamp created}
   :spec (cond-> {:storageClassName class} volume (assoc :volumeName volume))
   :status (cond-> {:phase phase}
             conditions (assoc :conditions conditions)
             allocated (assoc :allocatedResourceStatuses allocated))})

(defn- mounting
  [claim-name & {:keys [node]}]
  {:metadata {:namespace "shop" :name (str "user-of-" claim-name)}
   :spec (cond-> {:volumes [{:name "v" :persistentVolumeClaim {:claimName claim-name}}]} node (assoc :nodeName node))})

(def ^:private classes
  {:items [{:metadata {:name "ceph"} :volumeBindingMode "Immediate"}
           {:metadata {:name "local"} :volumeBindingMode "WaitForFirstConsumer"}
           {:metadata {:name "legacy"}}]})

(defn- run
  [claims & {:keys [pods events]}]
  (storage/detect-storage-problems {:now now
                                    "persistentvolumeclaims" {:items claims}
                                    "pods" {:items (or pods (map #(mounting (get-in % [:metadata :name]) :node "n1") claims))}
                                    "storageclasses" classes
                                    "events" {:items (or events [])}}))

(defn- summaries [findings] (mapv :summary findings))

(deftest a-pending-claim-gets-five-minutes
  (is (empty? (run [(claim "new" :phase "Pending" :created "2026-11-18T11:58:00Z")])))
  (is (= [[:critical "PersistentVolumeClaim is Pending"]]
         (map (juxt :severity :summary) (run [(claim "old" :phase "Pending")]))))
  (testing "a claim without a creation time is judged at once"
    (is (= 1 (count (run [(claim "unknown" :phase "Pending" :created nil)]))))))

(deftest a-claim-waiting-for-its-first-consumer-is-info
  (let [[finding] (run [(claim "data" :phase "Pending" :class "local")] :pods [])]
    (is (= :info (:severity finding)))
    (is (= "PersistentVolumeClaim waits for its first consumer (StorageClass local)" (:summary finding))))
  (testing "an unscheduled consumer still means waiting"
    (is (= [:info] (map :severity (run [(claim "data" :phase "Pending" :class "local")] :pods [(mounting "data")])))))
  (testing "once its consumer is on a node, a Pending claim is a provisioning failure"
    (is (= [:critical] (map :severity (run [(claim "data" :phase "Pending" :class "local")]
                                           :pods [(mounting "data" :node "n1")])))))
  (testing "a class without a binding mode binds immediately, and an unmounted claim is also unused"
    (is (= [:critical :info] (map :severity (run [(claim "data" :phase "Pending" :class "legacy")] :pods []))))))

(deftest a-pending-claim-quotes-the-latest-provisioning-failure
  (let [event (fn [at message] {:reason "ProvisioningFailed" :lastTimestamp at :message message
                                :involvedObject {:kind "PersistentVolumeClaim" :namespace "shop" :name "data"}})
        [finding] (run [(claim "data" :phase "Pending")]
                       :events [(event "2026-11-18T11:00:00Z" "old: quota")
                                (event "2026-11-18T11:30:00Z" "rpc error: pool ssd is full")
                                (assoc-in (event "2026-11-18T11:59:00Z" "other claim") [:involvedObject :name] "other")])]
    (is (= "the provisioner reports: rpc error: pool ssd is full" (:hint finding)))))

(deftest a-lost-claim-is-critical
  (is (= [[:critical "PersistentVolumeClaim lost its volume pv-123"]]
         (map (juxt :severity :summary) (run [(claim "data" :phase "Lost" :volume "pv-123")]))))
  (is (= ["PersistentVolumeClaim lost its volume"] (summaries (run [(claim "data" :phase "Lost")])))))

(deftest failed-resizes-and-modifications-are-warnings
  (doseq [type ["ControllerResizeError" "NodeResizeError" "ModifyVolumeError"]]
    (let [[finding :as findings] (run [(claim "data" :conditions [{:type type :status "True" :message "backend said no"}])])]
      (is (= 1 (count findings)) type)
      (is (= :warning (:severity finding)) type)
      (is (str/starts-with? (:summary finding) (str "PersistentVolumeClaim " type ":")) type)
      (is (str/starts-with? (:hint finding) "backend said no") type)))
  (testing "a condition that is no longer true is quiet"
    (is (empty? (run [(claim "data" :conditions [{:type "NodeResizeError" :status "False"}])])))))

(deftest an-infeasible-resize-is-a-warning
  (is (= ["PersistentVolumeClaim resize of storage is infeasible (ControllerResizeInfeasible)"]
         (summaries (run [(claim "data" :allocated {:storage "ControllerResizeInfeasible"})]))))
  (is (empty? (run [(claim "data" :allocated {:storage "ControllerResizeInProgress"})]))))

(deftest a-resize-waiting-for-a-mount-is-info
  (is (= [:info] (map :severity (run [(claim "data" :conditions [{:type "FileSystemResizePending" :status "True"}])])))))

(deftest a-bound-claim-in-use-is-quiet
  (is (empty? (run [(claim "data")])))
  (testing "but one nobody mounts is still listed"
    (is (= ["PersistentVolumeClaim is not used by any pod"] (summaries (run [(claim "data")] :pods []))))))

(deftest missing-storage-classes-and-events-do-not-break-the-check
  (is (= ["PersistentVolumeClaim is Pending"]
         (summaries (storage/detect-storage-problems
                     {:now now "persistentvolumeclaims" {:items [(claim "data" :phase "Pending" :class "local")]}
                      "pods" {:items [(mounting "data" :node "n1")]}})))))
