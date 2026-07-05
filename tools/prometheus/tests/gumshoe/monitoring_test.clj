;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.monitoring-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.detectives.monitoring :as monitoring]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.prometheus]))

(defn- summaries [findings] (set (map :summary findings)))

(deftest monitoring-detective-test
  (let [evidence {monitoring/prometheus-type
                  {:items [{:metadata {:namespace "monitoring" :name "prometheus"}
                            :status {:conditions [{:type "Available" :status "Degraded"
                                                   :reason "SomePodsNotReady"}]}}
                           {:metadata {:namespace "monitoring" :name "paused"}
                            :spec {:paused true}
                            :status {:conditions [{:type "Available" :status "True"}]}}]}}
        findings (monitoring/detect-prometheus-problems evidence)]
    (is (= #{"Prometheus is not Available (SomePodsNotReady)"
             "Prometheus is paused - the operator does not reconcile it"}
           (summaries findings)))))

(deftest package-registers-observability-scope-and-capability-test
  (is (seq (registry/for-scope :observability)) "the package fills the :observability scan scope")
  (is (contains? (set (capabilities/registered)) :prometheus-operator)))
