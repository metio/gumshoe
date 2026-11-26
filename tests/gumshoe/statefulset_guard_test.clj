;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.statefulset-guard-test
  "The judgement both StatefulSet resizes share once they deleted a
   StatefulSet with orphaned pods: it exists again with the new template,
   its controller observed the current generation, and every replica it
   wants is ready."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.storage :as storage]))

(def ^:private recreated
  {:metadata {:name "web" :generation 1}
   :spec {:replicas 2
          :volumeClaimTemplates [{:metadata {:name "data"} :spec {:resources {:requests {:storage "20Gi"}}}}]}
   :status {:observedGeneration 1 :replicas 2 :readyReplicas 2}})

(deftest a-recreated-ready-statefulset-is-resized
  (is (true? (storage/statefulset-resized? recreated "data" "20Gi"))))

(deftest a-missing-statefulset-is-never-resized-or-ready
  (is (false? (storage/statefulset-resized? nil "data" "20Gi")))
  (is (false? (storage/statefulset-ready? nil))))

(deftest the-template-must-request-the-capacity
  (is (false? (storage/statefulset-resized? recreated "data" "30Gi")))
  (is (false? (storage/statefulset-resized? recreated "logs" "20Gi")))
  (is (true? (storage/template-requests? recreated "data" "20480Mi")) "compared as quantities"))

(deftest readiness-needs-the-observed-generation-and-every-replica
  (is (false? (storage/statefulset-ready? (assoc-in recreated [:metadata :generation] 2))) "a newer spec not observed")
  (is (false? (storage/statefulset-ready? (assoc-in recreated [:status :readyReplicas] 1))))
  (is (false? (storage/statefulset-ready? (update recreated :status dissoc :readyReplicas)))
      "the API server omits a zero readyReplicas")
  (testing "spec.replicas defaults to 1"
    (is (true? (storage/statefulset-ready? (-> recreated (update :spec dissoc :replicas) (assoc-in [:status :readyReplicas] 1))))))
  (testing "zero replicas wanted, zero ready (omitted)"
    (is (true? (storage/statefulset-ready? (-> recreated (assoc-in [:spec :replicas] 0) (update :status dissoc :readyReplicas)))))))
