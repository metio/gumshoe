;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-pvc-template-size-test
  "A Cluster may declare a volume's size through pvcTemplate alone, and the
   operator resizes the claims to it all the same (GetSizeOrNil), so the
   storage and retention checks read it there too."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.cnpg.retention :as retention]
            [gumshoe.cnpg.storage :as storage]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.detectives.cnpg-retention :as cnpg-retention]
            [gumshoe.detectives.cnpg-storage :as cnpg-storage]))

(def ^:private gi 1073741824.0)

(defn- template [size] {:pvcTemplate {:resources {:requests {:storage size}}}})

(deftest the-size-is-read-as-the-operator-reads-it
  (is (= (* 20 gi) (double (storage/size-of {:size "20Gi"}))))
  (is (= (* 20 gi) (double (storage/size-of (template "20Gi")))) "pvcTemplate alone")
  (is (= (* 30 gi) (double (storage/size-of (merge (template "20Gi") {:size "30Gi"})))) "size wins")
  (is (= (* 20 gi) (double (storage/size-of (merge (template "20Gi") {:size ""})))) "an empty size is unset")
  (testing "a size that does not parse is nil, with no fallback to the template"
    (is (nil? (storage/size-of (merge (template "20Gi") {:size "lots"})))))
  (testing "nothing to read"
    (is (nil? (storage/size-of nil)))
    (is (nil? (storage/size-of {})))
    (is (nil? (storage/size-of {:pvcTemplate {:resources {}}})) "a template without a storage request")))

(defn- pvc
  [name role & [labels]]
  {:metadata {:namespace "db" :name name
              :labels (merge {:cnpg.io/cluster "pg" :cnpg.io/pvcRole role} labels)}
   :spec {:resources {:requests {:storage "10Gi"}}}
   :status {:capacity {:storage "10Gi"}}})

(defn- pending
  [spec & pvcs]
  (vec (cnpg-storage/detect-pending-storage
        {cnpg/cluster-type {:items [{:metadata {:namespace "db" :name "pg"} :spec spec}]}
         "persistentvolumeclaims" {:items (vec pvcs)}})))

(deftest a-data-volume-sized-by-its-template-is-judged
  (let [[finding :as all] (pending {:storage (template "20Gi")} (pvc "pg-1" "PG_DATA"))]
    (is (= 1 (count all)))
    (is (= :critical (:severity finding)))
    (is (str/includes? (:summary finding) "asks for 20.0GiB and this volume requests 10.0GiB"))))

(deftest wal-and-tablespace-volumes-sized-by-their-templates-are-judged
  (let [spec {:storage {:size "10Gi"}
              :walStorage (template "20Gi")
              :tablespaces [{:name "idx" :storage (template "30Gi")}]}
        findings (pending spec
                          (pvc "pg-1" "PG_DATA")
                          (pvc "pg-1-wal" "PG_WAL")
                          (pvc "pg-1-tbs-idx" "PG_TABLESPACE" {:cnpg.io/tablespaceName "idx"}))]
    (is (= #{"db/pg-1-wal" "db/pg-1-tbs-idx"} (set (map :component findings))))
    (is (every? #(= :critical (:severity %)) findings))))

(deftest a-volume-whose-size-cannot-be-read-is-reported-unjudged
  (let [[finding :as all] (pending {:storage {:pvcTemplate {:storageClassName "fast"}}} (pvc "pg-1" "PG_DATA"))]
    (is (= 1 (count all)))
    (is (= :info (:severity finding)))
    (is (str/includes? (:summary finding) "no size"))))

(deftest retention-measures-the-cap-against-a-template-sized-volume
  (let [cluster {:metadata {:namespace "db" :name "pg"}
                 :spec {:storage (template "20Gi")
                        :postgresql {:parameters {:max_slot_wal_keep_size "50GB"}}}}]
    (is (= {:section :storage :bytes (* 20 gi)} (update (retention/wal-volume cluster) :bytes double)))
    (is (= :oversized (:state (retention/assess cluster))))
    (is (seq (cnpg-retention/detect-unbounded-retention {cnpg/cluster-type {:items [cluster]}})))
    (testing "walStorage sized by its template is the volume pg_wal lives on"
      (is (= {:section :walStorage :bytes (* 20 gi)}
             (update (retention/wal-volume (assoc-in cluster [:spec :walStorage] (template "20Gi"))) :bytes double))))))
