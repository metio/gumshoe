;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.storage-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.storage :as storage]))

(def statefulset
  {:metadata {:name "postgres"
              :namespace "databases"
              :uid "abc-123"
              :resourceVersion "42"
              :creationTimestamp "2026-01-01T00:00:00Z"
              :generation 7
              :managedFields [{:manager "kubectl"}]
              :annotations {(keyword "kubectl.kubernetes.io/last-applied-configuration") "{...}"
                            :keep-me "yes"}}
   :spec {:replicas 2
          :volumeClaimTemplates [{:metadata {:name "data"}
                                  :spec {:resources {:requests {:storage "50Gi"}}}}
                                 {:metadata {:name "wal"}
                                  :spec {:resources {:requests {:storage "10Gi"}}}}]}
   :status {:readyReplicas 2}})

(deftest capacity-test
  (is (= "100Gi" (storage/normalize-capacity "100")))
  (is (= "100Gi" (storage/normalize-capacity "100Gi")))
  (is (= 100 (storage/gi-value "100Gi")))
  (is (nil? (storage/gi-value "1Ti"))))

(deftest statefulset-pvc-names-test
  (is (= ["data-postgres-0" "data-postgres-1"]
         (storage/statefulset-pvc-names statefulset "data"))))

(deftest expansion-plan-test
  (let [plan (storage/expansion-plan
              {:pvc-names ["data-postgres-0" "data-postgres-1"]
               :pvcs {:items [{:metadata {:name "data-postgres-0"}
                               :spec {:storageClassName "expandable"
                                      :resources {:requests {:storage "50Gi"}}}}]}
               :storage-classes {:items [{:metadata {:name "expandable"}
                                          :allowVolumeExpansion true}]}
               :capacity "100Gi"})]
    (testing "a missing PVC surfaces as a problem"
      (is (= [true false] (map :exists? plan)))
      (is (some #(re-find #"does not exist" %) (storage/expansion-problems plan))))))

(deftest expansion-problems-test
  (testing "a valid plan has no problems"
    (is (empty? (storage/expansion-problems
                 [{:pvc "x" :exists? true :storage-class "ok" :resizable? true
                   :current "50Gi" :target "100Gi"}]))))
  (testing "shrinking and non-expandable classes are rejected"
    (is (seq (storage/expansion-problems
              [{:pvc "x" :exists? true :storage-class "ok" :resizable? true
                :current "100Gi" :target "50Gi"}])))
    (is (seq (storage/expansion-problems
              [{:pvc "x" :exists? true :storage-class "fixed" :resizable? false
                :current "50Gi" :target "100Gi"}])))))

(deftest recreate-manifest-test
  (let [manifest (storage/recreate-manifest statefulset "data" "100Gi")]
    (testing "server-managed fields are gone"
      (is (nil? (:status manifest)))
      (is (nil? (-> manifest :metadata :uid)))
      (is (nil? (-> manifest :metadata :resourceVersion)))
      (is (nil? (-> manifest :metadata :managedFields)))
      (is (nil? (get-in manifest [:metadata :annotations
                                  (keyword "kubectl.kubernetes.io/last-applied-configuration")]))))
    (testing "other annotations survive"
      (is (= "yes" (-> manifest :metadata :annotations :keep-me))))
    (testing "only the chosen template is resized"
      (is (= ["100Gi" "10Gi"]
             (mapv #(-> % :spec :resources :requests :storage)
                   (-> manifest :spec :volumeClaimTemplates)))))))
