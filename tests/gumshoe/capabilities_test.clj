;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.capabilities-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.capabilities :as capabilities]))

(deftest built-in-detectors-registered-test
  (testing "the core ships detectors for the common platform pieces"
    (is (every? (set (capabilities/registered))
                [:cnpg :prometheus-operator :calico]))))

(deftest detect-present-is-best-effort-test
  (testing "a detector that throws counts as absent, never breaks detection"
    (capabilities/register-detector! :explodes (fn [] (throw (ex-info "boom" {}))))
    (capabilities/register-detector! :always (fn [] true))
    (let [found (set (capabilities/detect-present))]
      (is (contains? found :always))
      (is (not (contains? found :explodes)))))
  (testing "detect-present returns a sorted vector"
    (let [found (capabilities/detect-present)]
      (is (vector? found))
      (is (= found (vec (sort found)))))))

(deftest plugin-can-register-a-detector-test
  (testing "a plugin teaches the wizard a new capability with no core change"
    (capabilities/register-detector! :my-tool (fn [] true))
    (is (contains? (set (capabilities/registered)) :my-tool))
    (is (contains? (set (capabilities/detect-present)) :my-tool))))
