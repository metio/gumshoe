;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tally-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.tally :as tally]))

(deftest counted-agrees-with-the-number-test
  (is (= "1 denied flow" (tally/counted 1 "denied flow" "denied flows")))
  (is (= "0 fdb rows" (tally/counted 0 "fdb row" "fdb rows")))
  (is (= "3 entries" (tally/counted 3 "entry" "entries"))))

(deftest tally-groups-in-first-seen-order-test
  (is (= [{:key :b :items [[:b 1] [:b 3]] :count 2}
          {:key :a :items [[:a 2]] :count 1}]
         (tally/tally first [[:b 1] [:a 2] [:b 3]]))))

(deftest tally-keeps-order-past-an-array-map-test
  (let [xs (map #(keyword (str "k" %)) (range 40))]
    (is (= xs (map :key (tally/tally identity (concat xs xs)))))
    (is (every? #(= 2 (:count %)) (tally/tally identity (concat xs xs))))))

(deftest tally-of-nothing-is-empty-test
  (is (empty? (tally/tally identity nil))))
