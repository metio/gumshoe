;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-spec-semantics-test
  "The cnpg checks read a Cluster's spec the way CloudNativePG and PostgreSQL
   read it: resizeInUseVolumes, walStorage, disabled plugins, replica
   clusters, case-sensitive units and the QoS class a pod lands in."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.cnpg.actions :as actions]
            [gumshoe.cnpg.connections :as connections]
            [gumshoe.cnpg.recoverability :as recoverability]
            [gumshoe.cnpg.retention :as retention]
            [gumshoe.cnpg.storage :as storage]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.detectives.cnpg-retention :as cnpg-retention]
            [gumshoe.detectives.cnpg-storage :as cnpg-storage]
            [gumshoe.detectives.pooler :as pooler]))

;; --- pending storage --------------------------------------------------------------

(defn- storage-findings
  [storage-spec]
  (vec (cnpg-storage/detect-pending-storage
        {cnpg/cluster-type {:items [{:metadata {:namespace "db" :name "pg"}
                                     :spec {:storage storage-spec}}]}
         "persistentvolumeclaims" {:items [{:metadata {:namespace "db" :name "pg-1"
                                                       :labels {:cnpg.io/cluster "pg" :cnpg.io/pvcRole "PG_DATA"}}
                                            :spec {:resources {:requests {:storage "10Gi"}}}
                                            :status {:capacity {:storage "10Gi"}}}]}})))

(deftest resizing-off-is-the-re-create-procedure-not-a-critical-test
  (let [[finding :as all] (storage-findings {:size "20Gi" :resizeInUseVolumes false})]
    (is (= 1 (count all)))
    (is (= :info (:severity finding)))
    (is (str/includes? (:summary finding) "resizeInUseVolumes is false"))
    (is (str/includes? (:hint finding) "re-create"))
    (is (not (str/includes? (:hint finding) "patch the claim")))))

(deftest resizing-on-points-at-the-failed-operator-patch-test
  (doseq [spec [{:size "20Gi"} {:size "20Gi" :resizeInUseVolumes true}]]
    (let [[finding] (storage-findings spec)]
      (is (= :critical (:severity finding)) (pr-str spec))
      (is (str/includes? (:hint finding) "allowVolumeExpansion"))
      (is (str/includes? (:hint finding) "operator log")))))

(deftest only-spec-storage-decides-resizing-test
  ;; CloudNativePG's ShouldResizeInUseVolumes reads spec.storage alone, for
  ;; every volume role
  (is (storage/resizes-in-use-volumes? {:spec {:storage {:size "1Gi"}}}))
  (is (storage/resizes-in-use-volumes? {:spec {:storage {:size "1Gi"} :walStorage {:resizeInUseVolumes false}}}))
  (is (not (storage/resizes-in-use-volumes? {:spec {:storage {:resizeInUseVolumes false}}}))))

;; --- WAL retention ----------------------------------------------------------------

(deftest the-cap-is-sized-against-wal-storage-when-it-is-set-test
  (let [cluster {:metadata {:namespace "db" :name "pg"}
                 :spec {:storage {:size "100Gi"} :walStorage {:size "10Gi"}
                        :postgresql {:parameters {:max_slot_wal_keep_size "20GB"}}}}
        assessment (retention/assess cluster)
        [finding] (cnpg-retention/detect-unbounded-retention {cnpg/cluster-type {:items [cluster]}})]
    (is (= :oversized (:state assessment)))
    (is (= :walStorage (:section assessment)))
    (is (= "max_slot_wal_keep_size is 20.0GiB of a 10.0GiB walStorage volume, which is no floor"
           (:summary finding)))))

(deftest without-wal-storage-the-data-volume-is-compared-test
  (is (= {:state :bounded :raw "20GB" :cap 2.147483648E10 :volume 1.073741824E11 :section :storage}
         (retention/assess {:spec {:storage {:size "100Gi"}
                                   :postgresql {:parameters {:max_slot_wal_keep_size "20GB"}}}}))))

(deftest units-are-case-sensitive-as-postgres-reads-them-test
  (doseq [value ["10gb" "512mb" "1Kb" "1kb" "2tb" "1b"]]
    (is (nil? (retention/parse-size value)) value)
    (is (= :unknown (:state (retention/assess {:spec {:storage {:size "100Gi"}
                                                      :postgresql {:parameters {:max_slot_wal_keep_size value}}}})))
        value))
  (is (= 1.0 (retention/parse-size "1B")))
  (is (= 1048576.0 (retention/parse-size "1 MB")) "postgres allows whitespace before the unit"))

;; --- recoverability ---------------------------------------------------------------

(deftest a-disabled-wal-archiver-plugin-is-no-destination-test
  (let [cluster {:metadata {:namespace "db" :name "pg"}
                 :spec {:plugins [{:name "barman-cloud.cloudnative-pg.io" :isWALArchiver true :enabled false}]}}]
    (is (not (recoverability/destination? cluster)))
    (is (= {:state :unconfigured} (recoverability/assess cluster [])))))

(deftest an-enabled-wal-archiver-plugin-is-a-destination-test
  (doseq [plugin [{:name "p" :isWALArchiver true} {:name "p" :isWALArchiver true :enabled true}]]
    (is (recoverability/destination? {:spec {:plugins [plugin]}}) (pr-str plugin))
    (is (recoverability/archives-wal? {:spec {:plugins [plugin]}}) (pr-str plugin))))

(deftest a-volume-snapshot-destination-archives-no-wal-test
  (let [cluster {:spec {:backup {:volumeSnapshot {:className "csi"}}}}]
    (is (recoverability/destination? cluster))
    (is (not (recoverability/archives-wal? cluster)))))

;; --- suspend confirmation ---------------------------------------------------------

(def ^:private scheduled
  {:metadata {:namespace "db" :name "nightly"}
   :spec {:cluster {:name "pg"} :schedule "0 0 3 * * *"}})

(deftest the-suspend-confirmation-reads-whether-the-cluster-archives-wal-test
  (testing "a cluster backed up by volume snapshots alone"
    (is (= "cluster pg archives no WAL, so its newest recovery point is its newest backup, which ages from now on"
           (second (actions/suspend-items scheduled true {:spec {:backup {:volumeSnapshot {:className "csi"}}}})))))
  (testing "a cluster whose WAL archiver plugin is disabled"
    (is (str/includes? (second (actions/suspend-items scheduled true
                                                      {:spec {:plugins [{:name "p" :isWALArchiver true :enabled false}]}}))
                       "archives no WAL")))
  (testing "a cluster archiving to an object store"
    (let [item (second (actions/suspend-items scheduled true {:spec {:backup {:barmanObjectStore {:destinationPath "s3://b"}}}}))]
      (is (str/includes? item "keeps advancing"))
      (is (str/includes? item "newest base backup ages"))))
  (testing "a cluster that does not exist"
    (is (str/includes? (second (actions/suspend-items scheduled true nil)) "whether it archives WAL is unknown")))
  (testing "no item claims archiving continues without reading the Cluster"
    (is (not-any? #(str/includes? % "WAL archiving continues")
                  (mapcat #(actions/suspend-items scheduled true %)
                          [nil {:spec {}} {:spec {:backup {:volumeSnapshot {}}}}])))))

;; --- connections on a replica cluster ---------------------------------------------

(def ^:private recovering-reading
  {:max-connections 100 :superuser-reserved-connections 3 :reserved-connections 0
   :read-only? true :in-recovery? true
   :rows [{:client-addr "10.0.0.5" :backend-type "client backend" :backends 97}]})

(deftest replica-cluster-follows-cloudnative-pgs-is-replica-test
  (is (false? (connections/replica-cluster? {:metadata {:name "pg"} :spec {}})))
  (is (true? (connections/replica-cluster? {:metadata {:name "pg"} :spec {:replica {:enabled true :source "x"}}})))
  (is (false? (connections/replica-cluster? {:metadata {:name "pg"} :spec {:replica {:enabled false :source "x"}}})))
  (testing "the distributed topology: replica unless the cluster is the named primary"
    (is (false? (connections/replica-cluster? {:metadata {:name "pg"} :spec {:replica {:primary "pg" :source "x"}}})))
    (is (true? (connections/replica-cluster? {:metadata {:name "pg"} :spec {:replica {:primary "other" :source "x"}}})))
    (is (false? (connections/replica-cluster? {:metadata {:name "pg-b"}
                                               :spec {:replica {:self "pg" :primary "pg" :source "x"}}})))))

(deftest a-replica-clusters-designated-primary-is-read-test
  (let [ev (connections/collect {:items [{:metadata {:namespace "db" :name "replica-pg"}
                                          :spec {:replica {:enabled true :source "origin"}}
                                          :status {:currentPrimary "replica-pg-1"}}]}
                                (fn [] nil)
                                (fn [_ _] recovering-reading))]
    (is (= [] (:unread ev)))
    (is (= [{:namespace "db" :cluster "replica-pg" :replica? true :pod "replica-pg-1"
             :reading recovering-reading}]
           (:items ev)))))

(deftest a-standby-answering-for-an-ordinary-cluster-stays-unread-test
  (let [ev (connections/collect {:items [{:metadata {:namespace "db" :name "pg"}
                                          :status {:currentPrimary "pg-1"}}]}
                                (fn [] nil)
                                (fn [_ _] recovering-reading))]
    (is (= [] (:items ev)))
    (is (= [:in-recovery] (map :reason (:unread ev))))))

;; --- pooler QoS -------------------------------------------------------------------

(defn- pooler-findings
  [& containers]
  (vec (pooler/detect-poolers-without-resources
        {pooler/pooler-type {:items [{:metadata {:namespace "db" :name "pool"}}]}
         "pods" {:items [{:metadata {:namespace "db" :name "pool-x" :labels {:cnpg.io/poolerName "pool"}}
                          :spec {:containers (vec containers)}}]}})))

(deftest requests-without-limits-is-burstable-not-best-effort-test
  (let [[finding :as all] (pooler-findings {:name "pgbouncer" :resources {:requests {:cpu "100m" :memory "64Mi"}}})]
    (is (= 1 (count all)))
    (is (= "pods run without resource limits" (:summary finding)))
    (is (str/includes? (:hint finding) "Burstable"))
    (is (not (str/includes? (:hint finding) "BestEffort QoS,")))))

(deftest nothing-set-is-best-effort-test
  (let [[finding] (pooler-findings {:name "pgbouncer"})]
    (is (= "pods run without resource requests or limits" (:summary finding)))
    (is (str/starts-with? (:hint finding) "BestEffort QoS"))))

(deftest a-container-without-requests-beside-one-with-them-test
  (let [[finding] (pooler-findings {:name "pgbouncer" :resources {:requests {:cpu "100m"} :limits {:cpu "1"}}}
                                   {:name "exporter"})]
    (is (= "pods have a container without resource requests" (:summary finding)))))

(deftest limits-alone-stand-in-for-requests-test
  (is (= [] (pooler-findings {:name "pgbouncer" :resources {:limits {:cpu "1" :memory "128Mi"}}}))))

(deftest the-resource-gap-of-a-pod-test
  (is (nil? (pooler/resource-gap {:spec {:containers [{:resources {:requests {:cpu "1"} :limits {:cpu "1"}}}]}})))
  (is (= :best-effort (pooler/resource-gap {:spec {:containers [{} {:resources {}}]}})))
  (is (= :limits (pooler/resource-gap {:spec {:containers [{:resources {:requests {:cpu "1"}}}]}}))))
