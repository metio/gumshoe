;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-health-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.cnpg :as cnpg]))

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

(defn- cluster
  [& {:keys [phase reason archiving expirations primary target]
      :or {phase "Cluster in healthy state" primary "pg-1"}}]
  {:metadata {:namespace "db" :name "pg"}
   :status (cond-> {:phase phase :phaseReason reason :currentPrimary primary :targetPrimary (or target primary)}
             archiving (assoc :conditions [{:type "ContinuousArchiving" :status archiving :message "archive command failed"}])
             expirations (assoc :certificates {:expirations expirations}))})

;; --- phases and archiving -------------------------------------------------------

(deftest a-phase-waiting-for-someone-is-critical
  (doseq [phase ["Waiting for user action" "Cluster is unrecoverable and needs manual intervention"
                 "Invalid cluster definition" "Unable to create required cluster objects"
                 "Cluster has incomplete or invalid image catalog"]]
    (let [[finding] (cnpg/detect-cnpg-problems {cnpg/cluster-type {:items [(cluster :phase phase :reason "PVC lost")]}})]
      (is (= :critical (:severity finding)) phase)
      (is (= "PVC lost - the operator waits for a change and will not recover on its own" (:hint finding)) phase))))

(deftest a-moving-phase-stays-a-warning
  (doseq [phase ["Switchover in progress" "Failing over" "Upgrading cluster" "Creating a new replica"]]
    (is (= [[:warning (str "cluster phase: " phase)]]
           (map (juxt :severity :summary) (cnpg/detect-cnpg-problems {cnpg/cluster-type {:items [(cluster :phase phase)]}})))
        phase))
  (is (nil? (:hint (first (cnpg/detect-cnpg-problems {cnpg/cluster-type {:items [(cluster :phase "Failing over")]}}))))
      "no reason, no hint"))

(deftest failing-archiving-is-critical
  (let [[finding] (cnpg/detect-cnpg-problems {cnpg/cluster-type {:items [(cluster :archiving "False")]}})]
    (is (= :critical (:severity finding)))
    (is (str/starts-with? (:hint finding) "archive command failed - no new recovery point")))
  (is (empty? (cnpg/detect-cnpg-problems {cnpg/cluster-type {:items [(cluster :archiving "True")]}}))))

;; --- certificates ---------------------------------------------------------------

(deftest expirations-parse-in-either-form
  (doseq [[value expected] [["2026-11-20T10:00:00Z" "2026-11-20T10:00:00Z"]
                            ["2026-11-20T12:00:00+02:00" "2026-11-20T10:00:00Z"]
                            ["2026-11-20 10:00:00 +0000 UTC" "2026-11-20T10:00:00Z"]
                            ["2026-11-20 12:00:00 +0200 CEST" "2026-11-20T10:00:00Z"]
                            ["2026-11-20 10:00:00.5 +0000 UTC" "2026-11-20T10:00:00.500Z"]
                            ["2026-11-20 10:00:00.123456789 +0000 UTC m=+3600.000000001" "2026-11-20T10:00:00.123456789Z"]]]
    (is (= expected (str (cnpg/parse-expiration value))) value))
  (doseq [value [nil "" "next tuesday" "2026-11-20"]]
    (is (nil? (cnpg/parse-expiration value)) (pr-str value))))

(deftest certificates-near-or-past-expiry
  (let [findings (cnpg/detect-certificate-expiry
                  {:now now cnpg/cluster-type
                   {:items [(cluster :expirations {:pg-ca "2027-11-18 12:00:00 +0000 UTC"
                                                   :pg-server "2026-11-21 12:00:00 +0000 UTC"
                                                   :pg-replication "2026-11-17 12:00:00 +0000 UTC"
                                                   :pg-broken "whenever"})]}})]
    (is (= [[:critical "certificate in Secret pg-replication expired 2026-11-17T12:00:00Z"]
            [:warning "certificate in Secret pg-server expires in 3 days"]]
           (map (juxt :severity :summary) findings)))
    (is (= ["2026-11-17T12:00:00Z" nil] (map :since findings))))
  (testing "seven days and more is quiet"
    (is (empty? (cnpg/detect-certificate-expiry
                 {:now now cnpg/cluster-type {:items [(cluster :expirations {:pg-server "2026-11-25T12:00:00Z"})]}}))))
  (testing "no clock, no judgement"
    (is (empty? (cnpg/detect-certificate-expiry
                 {cnpg/cluster-type {:items [(cluster :expirations {:pg-server "2020-01-01T00:00:00Z"})]}})))))

;; --- primary labels -------------------------------------------------------------

(defn- instance
  [name role & {:keys [legacy]}]
  {:metadata {:namespace "db" :name name
              :labels (cond-> {:cnpg.io/cluster "pg" :cnpg.io/podRole "instance"}
                        role (assoc :cnpg.io/instanceRole role)
                        legacy (assoc :role legacy))
              :ownerReferences [{:kind "Cluster" :name "pg" :controller true}]}})

(defn- labels [cluster pods]
  (map :summary (cnpg/detect-primary-label-mismatch {cnpg/cluster-type {:items [cluster]} "pods" {:items pods}})))

(deftest agreeing-labels-are-quiet
  (is (empty? (labels (cluster) [(instance "pg-1" "primary") (instance "pg-2" "replica") (instance "pg-3" "replica")])))
  (is (empty? (labels (cluster) [(instance "pg-1" nil :legacy "primary") (instance "pg-2" nil :legacy "replica")]))
      "the deprecated role label counts"))

(deftest an-unlabelled-primary
  (is (= ["the designated primary pg-1 does not carry the primary role label"]
         (labels (cluster) [(instance "pg-1" "replica") (instance "pg-2" "replica")]))))

(deftest a-replica-labelled-primary
  (is (= ["pg-2 carries the primary role label but pg-1 is the designated primary"]
         (labels (cluster) [(instance "pg-1" "primary") (instance "pg-2" "primary")])))
  (is (= ["the designated primary pg-1 does not carry the primary role label"
          "pg-2, pg-3 carry the primary role label but pg-1 is the designated primary"]
         (labels (cluster) [(instance "pg-1" "replica") (instance "pg-2" "primary") (instance "pg-3" nil :legacy "primary")]))))

(deftest a-cluster-on-the-move-is-left-alone
  (is (empty? (labels (cluster :target "pg-2") [(instance "pg-1" "replica") (instance "pg-2" "primary")])))
  (is (empty? (labels (cluster :phase "Failing over") [(instance "pg-1" "replica") (instance "pg-2" "primary")])))
  (is (empty? (labels (cluster) [])) "no pods read")
  (is (empty? (labels (cluster) [(instance "pg-2" "replica")])) "the primary pod is gone - the crashloop and primary detectives cover it"))

(deftest pooler-pods-are-not-instances
  (is (empty? (labels (cluster) [(instance "pg-1" "primary")
                                 (assoc-in (instance "pg-rw-abc" "primary") [:metadata :labels :cnpg.io/poolerName] "pg-rw")]))))

;; --- scheduled backups ----------------------------------------------------------

(deftest the-seconds-field-is-dropped
  (is (= (java.time.Duration/ofDays 1) (cnpg/schedule-interval "0 0 0 * * *")))
  (is (= (java.time.Duration/ofHours 1) (cnpg/schedule-interval "0 30 * * * *")))
  (is (= (java.time.Duration/ofDays 1) (cnpg/schedule-interval "0 3 * * *")) "five fields read as they are")
  (is (= (java.time.Duration/ofDays 7) (cnpg/schedule-interval "@weekly")))
  (is (nil? (cnpg/schedule-interval "whenever"))))

(defn- scheduled
  [& {:keys [schedule last created suspend] :or {schedule "0 0 0 * * *" created "2026-11-01T00:00:00Z"}}]
  {:metadata {:namespace "db" :name "daily" :creationTimestamp created}
   :spec (cond-> {:schedule schedule :cluster {:name "pg"}} suspend (assoc :suspend true))
   :status (cond-> {} last (assoc :lastScheduleTime last))})

(defn- overdue [& items]
  (map (juxt :severity :summary) (cnpg/detect-overdue-scheduled-backups {:now now cnpg/scheduled-backup-type {:items items}})))

(deftest a-backup-that-stopped
  (is (empty? (overdue (scheduled :last "2026-11-18T00:00:00Z"))))
  (is (empty? (overdue (scheduled :last "2026-11-16T12:00:01Z"))) "inside twice the interval")
  (is (= [[:warning "scheduled backup has not run since 2026-11-16T00:00:00Z"]]
         (overdue (scheduled :last "2026-11-16T00:00:00Z")))))

(deftest a-frequent-schedule-gets-at-least-two-hours
  (is (empty? (overdue (scheduled :schedule "0 */5 * * * *" :last "2026-11-18T10:30:00Z"))))
  (is (= 1 (count (overdue (scheduled :schedule "0 */5 * * * *" :last "2026-11-18T09:30:00Z"))))))

(deftest a-backup-that-never-ran
  (is (= [[:warning "scheduled backup has never run"]] (overdue (scheduled))))
  (is (empty? (overdue (scheduled :created "2026-11-18T06:00:00Z"))) "created today"))

(deftest suspended-unreadable-or-unclocked-schedules-are-not-judged
  (is (empty? (overdue (scheduled :suspend true))))
  (is (empty? (overdue (scheduled :schedule "whenever"))))
  (is (empty? (cnpg/detect-overdue-scheduled-backups {cnpg/scheduled-backup-type {:items [(scheduled)]}}))))
