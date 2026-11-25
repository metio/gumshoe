;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ui.charm-test
  (:require [charm.message :as msg]
            [charm.program :as program]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.ui.charm :as charm]))

(defn- press [key & mods] (apply msg/key-press key mods))

(defn- play
  "The final state of a program started at `state` and fed `keys` through
   charm/step with `component-update`, stopping at the first key that quits,
   as program/run does."
  [component-update state keys]
  (reduce (fn [s k]
            (let [[s' command] (charm/step component-update s k)]
              (if (= program/quit-cmd command) (reduced s') s')))
          state keys))

(deftest a-bare-enter-at-the-confirmation-declines
  (is (= "no" (first charm/confirm-choices)))
  (is (false? (charm/confirmed? (play charm/list-update (charm/list-state charm/confirm-choices)
                                      [(press :enter)])))))

(deftest the-confirmation-approves-only-an-explicit-yes
  (is (true? (charm/confirmed? (play charm/list-update (charm/list-state charm/confirm-choices)
                                     [(press :down) (press :enter)]))))
  (testing "a cancel after moving to yes declines"
    (is (false? (charm/confirmed? (play charm/list-update (charm/list-state charm/confirm-choices)
                                        [(press :down) (press :escape)]))))
    (is (false? (charm/confirmed? (play charm/list-update (charm/list-state charm/confirm-choices)
                                        [(press :down) (press "c" :ctrl true)]))))))

(deftest a-pick-returns-the-value-under-the-cursor
  (let [state (charm/list-state ["a" "b" "c"])]
    (is (= "a" (charm/picked (play charm/list-update state [(press :enter)]))))
    (is (= "c" (charm/picked (play charm/list-update state [(press :down) (press "j") (press :enter)]))))
    (is (= "b" (charm/picked (play charm/list-update state [(press :end) (press :up) (press :enter)]))))
    (is (nil? (charm/picked (play charm/list-update state [(press :down) (press :escape)]))))
    (testing "keys after Enter are not read"
      (is (= "a" (charm/picked (play charm/list-update state [(press :enter) (press :down)])))))))

(deftest a-multi-pick-returns-the-marked-values-in-list-order
  (let [state (charm/multi-state ["a" "b" "c"])]
    (is (= [] (charm/picked-many (play charm/multi-update state [(press :enter)]))))
    (is (= ["a" "c"] (charm/picked-many (play charm/multi-update state
                                               [(press :end) (press :space) (press :home) (press :space) (press :enter)]))))
    (is (= ["b"] (charm/picked-many (play charm/multi-update state
                                           [(press :down) (press :space) (press :down) (press :space) (press :space) (press :enter)])))
        "a second Space unmarks")
    (is (nil? (charm/picked-many (play charm/multi-update state [(press :space) (press :escape)]))))))

(deftest the-multi-view-marks-each-value
  (let [view (charm/multi-view (play charm/multi-update (charm/multi-state ["a" "b"]) [(press :space)]))]
    (is (re-find #"\[x\] a" view))
    (is (re-find #"\[ \] b" view))))

(deftest text-is-read-from-the-input
  (let [state (charm/text-update (charm/text-update (charm/text-state) (press "h")) (press "i"))]
    (is (= "hi" (charm/entered state)))
    (is (nil? (charm/entered (assoc state charm/cancelled true))))
    (is (nil? (charm/entered (charm/text-state))) "an empty input is no answer")))

(deftest only-keypresses-reach-the-component
  (let [seen (atom [])
        update (fn [s m] (swap! seen conj m) s)]
    (charm/step update {} (msg/window-size 80 24))
    (charm/step update {} (press "x"))
    (is (= [(press "x")] @seen))))
