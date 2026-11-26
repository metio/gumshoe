;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.loki-unread-mode-test
  "A workload read that did not answer never becomes \"no Loki components
   found\"."
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.loki :as loki]))

(def ^:private unanswered {"workloads" nil "namespace" "loki" "rings" nil})

(deftest the-mode-check-judges-nothing-without-workloads-test
  (is (= [] (vec (loki/detect-deployment-mode unanswered)))))

(deftest an-unanswered-workload-read-yields-no-findings-test
  (let [checked (detective/mark-unread loki/detectives unanswered)]
    (is (every? :unread checked))
    (is (= [] (vec (detective/run-detectives checked unanswered))))))

(deftest an-answered-empty-workload-list-is-still-no-components-test
  (is (= ["no Loki components found in this namespace"]
         (map :summary (loki/detect-deployment-mode {"workloads" [] "namespace" "loki"})))))
