;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.prometheus-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.prometheus :as prometheus]
            [gumshoe.storage :as storage]))

(def pods
  {:items [{:metadata {:name "prometheus-main-0" :namespace "monitoring"}
            :spec {:volumes [{:name "prometheus-data"
                              :persistentVolumeClaim {:claimName "data-prometheus-main-0"}}]}}]})

(def pvcs
  {:items [{:metadata {:name "data-prometheus-main-0" :namespace "monitoring"}
            :spec {:storageClassName "expandable"
                   :resources {:requests {:storage "50Gi"}}}}]})

(def storage-classes
  {:items [{:metadata {:name "expandable"} :allowVolumeExpansion true}
           {:metadata {:name "fixed"} :allowVolumeExpansion false}]})

(deftest resize-plan-test
  (let [plan (prometheus/resize-plan {:pods pods
                                      :volume-name "prometheus-data"
                                      :pvcs pvcs
                                      :storage-classes storage-classes
                                      :capacity "100Gi"})]
    (is (= [{:pod "prometheus-main-0"
             :pvc "data-prometheus-main-0"
             :exists? true
             :storage-class "expandable"
             :resizable? true
             :current "50Gi"
             :target "100Gi"}]
           plan))
    (testing "a growing plan on expandable storage has no problems"
      (is (empty? (storage/expansion-problems plan))))))

(deftest an-unnamed-claim-template-uses-the-operators-volume-name
  (is (= "prometheus-k8s-db"
         (prometheus/volume-name-of {:kind "Prometheus" :metadata {:name "k8s"}
                                     :spec {:storage {:volumeClaimTemplate {:spec {}}}}})))
  (is (= "prom-agent-edge-db"
         (prometheus/volume-name-of {:kind "PrometheusAgent" :metadata {:name "edge"}
                                     :spec {:storage {:volumeClaimTemplate {:spec {}}}}})))
  (is (= "data"
         (prometheus/volume-name-of {:kind "Prometheus" :metadata {:name "k8s"}
                                     :spec {:storage {:volumeClaimTemplate {:metadata {:name "data"}}}}}))))
