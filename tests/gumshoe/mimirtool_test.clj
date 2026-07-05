;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mimirtool-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.mimirtool :as mimirtool]))

(deftest used-metrics-test
  (is (= ["node_cpu_seconds_total" "up"]
         (mimirtool/used-metrics {:metricsUsed ["up" "node_cpu_seconds_total" "up"]}))))

(deftest unused-metrics-test
  (is (= ["forgotten_metric" "orphaned_metric"]
         (mimirtool/unused-metrics {:additional_metric_counts
                                    [{:metric "orphaned_metric" :count 10}
                                     {:metric "forgotten_metric" :count 2}]}))))
