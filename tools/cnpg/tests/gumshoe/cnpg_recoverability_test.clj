;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-recoverability-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.cnpg.recoverability :as recoverability]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.detectives.cnpg-recoverability :as detective]))

(defn- cluster
  [spec]
  {:metadata {:namespace "db" :name "general"} :spec spec})

(defn- backup
  [phase]
  {:metadata {:namespace "db" :name "general-20261104"}
   :spec {:cluster {:name "general"}}
   :status {:phase phase}})

(defn- findings
  [cluster & backups]
  (detective/detect-unrecoverable-clusters
   {cnpg/cluster-type {:items [cluster]}
    cnpg/backup-type {:items backups}}))

(def ^:private with-store
  (cluster {:backup {:barmanObjectStore {:destinationPath "s3://backups/general"}}}))

;; The shape every cluster in this estate has: no destination anywhere in the
;; spec, while the ContinuousArchiving condition reads True.
(deftest a-cluster-with-no-destination-cannot-be-restored-test
  (let [[finding :as all] (findings (cluster {:storage {:size "40Gi"}}))]
    (is (= 1 (count all)))
    (is (= "db/general" (:component finding)))
    (is (re-find #"no backup destination is configured" (:summary finding)))
    (is (re-find #"reports nothing failing, not a restore path" (:hint finding)))))

(deftest healthy-wal-archiving-is-not-a-restore-path-test
  (let [archiving (cluster {:status {:conditions [{:type "ContinuousArchiving" :status "True"}]}})]
    (is (seq (findings archiving))
        "the condition says nothing is failing, which is not a backup")))

(deftest a-barman-object-store-is-a-destination-test
  (is (recoverability/destination? with-store)))

(deftest a-wal-archiver-plugin-is-a-destination-test
  (is (recoverability/destination?
       (cluster {:plugins [{:name "barman-cloud.cloudnative-pg.io" :isWALArchiver true}]}))))

(deftest a-plugin-that-does-not-archive-is-not-a-destination-test
  (is (not (recoverability/destination?
            (cluster {:plugins [{:name "some-other.cloudnative-pg.io"}]})))))

(deftest a-destination-with-nothing-in-it-is-reported-test
  (let [finding (first (findings with-store))]
    (is (re-find #"no backup has completed \(0 attempted\)" (:summary finding)))))

(deftest a-destination-whose-attempts-all-failed-is-reported-test
  (let [finding (first (findings with-store (backup "failed") (backup "failed")))]
    (is (re-find #"no backup has completed \(2 attempted\)" (:summary finding)))))

(deftest a-completed-backup-is-a-restore-path-test
  (is (empty? (findings with-store (backup "completed"))))
  (is (= :recoverable (:state (recoverability/assess with-store [(backup "completed")])))))

(deftest another-clusters-backup-does-not-count-test
  (let [elsewhere (assoc-in (backup "completed") [:spec :cluster :name] "other")]
    (is (seq (findings with-store elsewhere)))))

(deftest a-backup-in-another-namespace-does-not-count-test
  (let [elsewhere (assoc-in (backup "completed") [:metadata :namespace] "other")]
    (is (seq (findings with-store elsewhere)))))
