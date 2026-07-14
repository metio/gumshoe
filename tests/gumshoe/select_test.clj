;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.select-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.select :as select]))

(deftest nothing-selected-test
  (testing "a single pick is empty only when nil"
    (is (select/nothing-selected? :one nil))
    (is (not (select/nothing-selected? :one "node-1")))
    (is (select/nothing-selected? :namespaced nil))
    (is (not (select/nothing-selected? :namespaced "ns/name"))))
  (testing "a multi pick is empty only when it has no items"
    (is (select/nothing-selected? :many []))
    (is (select/nothing-selected? :many nil))
    (is (not (select/nothing-selected? :many ["a" "b"])))))

(deftest items-test
  (testing "a single pick becomes a one-item list, a multi pick becomes its vector"
    (is (= ["node-1"] (select/items :one "node-1")))
    (is (= ["ns/name"] (select/items :namespaced "ns/name")))
    (is (= ["a" "b"] (select/items :many ["a" "b"])))))

;; A valid provided flag value is returned by interact/choose-one without a
;; prompt, so resolve-target/pick are exercisable without a TTY.
(def ^:private one-spec
  {:mode :one :label "Node" :flag :node
   :candidates (fn [_context] ["node-1" "node-2"])
   :empty-message "every node is already cordoned"})

(deftest resolve-target-test
  (testing "a provided valid value resolves to itself with the candidate list"
    (is (= {:candidates ["node-1" "node-2"] :target "node-1"}
           (select/resolve-target one-spec {:node "node-1"} "ctx")))))

(deftest pick-test
  (testing "a provided valid pick is returned"
    (is (= "node-1" (select/pick one-spec {:node "node-1"} "ctx"))))
  (testing "no candidates yields nil (the caller sees the :empty-message)"
    (is (nil? (select/pick (assoc one-spec :candidates (fn [_] [])) {} "ctx"))))
  (testing "candidates but no pick yields nil"
    (is (nil? (select/pick one-spec {} "ctx")))))
