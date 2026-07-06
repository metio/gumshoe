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
      (with-redefs [fuzzy/select-single (fn [& _] @label)
                    main/investigate (fn [& _] :investigate)
                    main/detect (fn [& _] :detect)
                    main/run (fn [& _] :run)]
        (is (= expected (guided))
            (str "label " @label " should dispatch to " expected)))))
  (testing "an escaped (nil) selection at the top level quits with exit code 1"
    (with-redefs [fuzzy/select-single (fn [& _] nil)]
      (is (= 1 (guided))))))

(def ^:private menu-choice #'main/menu-choice)

(deftest menu-choice-test
  (testing "choosing the Back row resolves to :back"
    (with-redefs [fuzzy/select-single (fn [_ rows & _] (last rows))]
      (is (= :back (menu-choice "p" ["a" "b"] true))
          "with back? the '⬅ Back' row is appended and selecting it means step up")))
  (testing "ESC is :back in a submenu, but nil (quit) at the top"
    (with-redefs [fuzzy/select-single (fn [& _] nil)]
      (is (= :back (menu-choice "p" ["a"] true)))
      (is (nil? (menu-choice "p" ["a"] false)))))
  (testing "a normal pick passes through unchanged"
    (with-redefs [fuzzy/select-single (fn [& _] "a")]
      (is (= "a" (menu-choice "p" ["a" "b"] false))))))

(deftest guided-back-loops-test
  (testing "a submenu returning :back re-shows the top menu instead of exiting"
    (let [picks (atom [@#'main/scan-area @#'main/follow-lead])]
      (with-redefs [fuzzy/select-single (fn [& _] (let [v (first @picks)] (swap! picks rest) v))
                    main/detect (fn [& _] :back)
                    main/investigate (fn [& _] :done)]
        (is (= :done (guided))
            "Back from the scan submenu loops to the top, where the next pick runs")))))
