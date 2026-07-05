;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.main-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.main :as main]))

(def ^:private guided #'main/guided)

(deftest guided-menu-dispatch-test
  (testing "each menu label dispatches to its action without throwing"
    ;; The fuzzy finder returns the label STRING; the menu must match on the
    ;; string value, not the label symbol.
    (doseq [[label expected]
            [[#'main/follow-lead :investigate]
             [#'main/scan-area :detect]
             [#'main/run-book :run]]]
      (with-redefs [fuzzy/select-single (fn [_ _] @label)
                    main/investigate (fn [_] :investigate)
                    main/detect (fn [_] :detect)
                    main/run (fn [_] :run)]
        (is (= expected (guided))
            (str "label " @label " should dispatch to " expected)))))
  (testing "an escaped (nil) selection returns exit code 1"
    (with-redefs [fuzzy/select-single (fn [_ _] nil)]
      (is (= 1 (guided))))))
