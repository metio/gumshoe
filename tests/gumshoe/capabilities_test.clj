;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.capabilities-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.capabilities :as capabilities]))

(deftest detect-present-is-best-effort-test
  (testing "a detector that throws counts as absent, never breaks detection"
    (capabilities/register-detector! :explodes (fn [] (throw (ex-info "boom" {}))))
    ;; a plugin detector using an assert/:pre throws an AssertionError (an Error),
    ;; which must be caught too or it aborts the whole sweep
    (capabilities/register-detector! :asserts (fn [] (assert false "nope")))
    (capabilities/register-detector! :always (fn [] true))
    (let [found (set (capabilities/detect-present))]
      (is (contains? found :always))
      (is (not (contains? found :explodes)))
      (is (not (contains? found :asserts)))))
  (testing "detect-present returns a sorted vector"
    (let [found (capabilities/detect-present)]
      (is (vector? found))
      (is (= found (vec (sort found)))))))

(deftest plugin-can-register-a-detector-test
  (testing "a plugin teaches the wizard a new capability with no core change"
    (capabilities/register-detector! :my-tool (fn [] true))
    (is (contains? (set (capabilities/registered)) :my-tool))
    (is (contains? (set (capabilities/detect-present)) :my-tool))))
