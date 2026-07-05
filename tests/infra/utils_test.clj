;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.utils-test
  (:require [clojure.test :refer [deftest is testing]]
            [infra.utils :as utils]))

(deftest conj-if-not-empty-test
  (testing "adds the value when the trigger collection is non-empty"
    (is (= ["kubectl" "gopass"] (utils/conj-if-not-empty ["kubectl"] ["some-secret"] "gopass"))))
  (testing "keeps the collection when the trigger collection is empty"
    (is (= ["kubectl"] (utils/conj-if-not-empty ["kubectl"] [] "gopass"))))
  (testing "does not add duplicates"
    (is (= ["gopass"] (utils/conj-if-not-empty ["gopass"] ["some-secret"] "gopass")))))

(deftest collection-contains-test
  (testing "finds a present value"
    (is (utils/collection-contains? ["a" "b"] "a")))
  (testing "misses an absent value"
    (is (not (utils/collection-contains? ["a" "b"] "c")))))
