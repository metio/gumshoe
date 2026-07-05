;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.mutation :as mutation]))

(deftest nothing-selected-test
  (testing "a single pick is empty only when nil"
    (is (mutation/nothing-selected? :one nil))
    (is (not (mutation/nothing-selected? :one "node-1")))
    (is (mutation/nothing-selected? :namespaced nil))
    (is (not (mutation/nothing-selected? :namespaced "ns/name"))))
  (testing "a multi pick is empty only when it has no items"
    (is (mutation/nothing-selected? :many []))
    (is (mutation/nothing-selected? :many nil))
    (is (not (mutation/nothing-selected? :many ["a" "b"])))))

(deftest items-test
  (testing "a single pick becomes a one-item list, a multi pick becomes its vector"
    (is (= ["node-1"] (mutation/items :one "node-1")))
    (is (= ["ns/name"] (mutation/items :namespaced "ns/name")))
    (is (= ["a" "b"] (mutation/items :many ["a" "b"])))))
