;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.prerequisites-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [gumshoe.prerequisites :as prerequisites]))

;; The registry is a process-wide atom; reset it around each test.
(use-fixtures :each (fn [t]
                      (reset! @#'prerequisites/checks {})
                      (t)
                      (reset! @#'prerequisites/checks {})))

(defn- run-item
  "The {:ok? :label} a checklist item's thunk produces."
  [[_label thunk]]
  (thunk))

(deftest check-builds-a-passing-or-failing-item-test
  (testing "the check helper produces a checklist item that reports pass/fail"
    (let [ok (prerequisites/check "window" (constantly true) {:pass "open" :fail "closed"})
          no (prerequisites/check "window" (constantly false) {:pass "open" :fail "closed"})]
      (is (= {:ok? true :label "open"} (run-item ok)))
      (is (= {:ok? false :label "closed"} (run-item no))))))

(deftest items-only-for-declared-checks-test
  (testing "a registered check contributes items only when the book declares its key"
    (prerequisites/register-check!
     :change-window
     (fn [window _opts]
       [(prerequisites/check (str "change window: " window) (constantly true)
                             {:pass (str window " open") :fail (str window " closed")})]))
    (is (contains? (set (prerequisites/registered-checks)) :change-window))
    (testing "a book that declares it gets the item"
      (let [items (prerequisites/items {:change-window "prod"} {})]
        (is (= 1 (count items)))
        (is (= {:ok? true :label "prod open"} (run-item (first items))))))
    (testing "a book that does not declare it gets nothing"
      (is (empty? (prerequisites/items {:installed-tools ["kubectl"]} {}))))))

(deftest multiple-checks-run-in-stable-order-test
  (testing "declared checks contribute in sorted-key order regardless of registration order"
    (prerequisites/register-check! :zeta (fn [_ _] [(prerequisites/check "z" (constantly true) {})]))
    (prerequisites/register-check! :alpha (fn [_ _] [(prerequisites/check "a" (constantly true) {})]))
    (let [labels (map first (prerequisites/items {:zeta 1 :alpha 1} {}))]
      (is (= ["a" "z"] labels)))))
