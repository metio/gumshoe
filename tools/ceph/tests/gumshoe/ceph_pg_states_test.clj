;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ceph-pg-states-test
  "Critical placement-group states are found even inside a state that starts
   with active+clean."
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.detectives.ceph :as detectives]))

(defn- findings
  [& states]
  (vec (detectives/detect-pg-problems
        {"status" {:pgmap {:pgs_by_state (vec (for [[state n] (partition 2 states)]
                                                 {:state_name state :count n}))}}})))

(deftest an-inconsistent-clean-pg-is-critical-test
  (is (= [{:severity :critical :component "pgs" :summary "2 PGs are active+clean+inconsistent"}
          {:severity :critical :component "pgs"
           :summary "1 PGs are active+clean+scrubbing+deep+inconsistent+repair"}]
         (findings "active+clean+inconsistent" 2
                   "active+clean+scrubbing+deep+inconsistent+repair" 1
                   "active+clean" 100))))

(deftest a-clean-pg-with-benign-flags-is-quiet-test
  (is (= [] (findings "active+clean" 90 "active+clean+scrubbing+deep" 5 "active+clean+snaptrim" 3))))

(deftest other-states-keep-their-severity-test
  (is (= [:critical :warning]
         (map :severity (findings "down+peering" 1 "active+remapped+backfilling" 4)))))
