;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(deftest run-on-uses-the-given-target-test
  (testing "run-on! runs the spec against the provided target, skipping selection"
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  interact/confirm! (constantly true)]
      (let [seen (atom nil)
            spec {:select {:mode :one :label "Namespace"}
                  :confirm {:action "do a thing"}
                  :effect (fn [{:keys [target]}] (reset! seen target) [])}]
        (is (true? (mutation/run-on! spec "ns-1" {} nil)))
        (is (= "ns-1" @seen) "the effect must see the target passed to run-on!")))))

(deftest run-selects-then-runs-on-the-pick-test
  (testing "run! resolves the subject from the flags, then runs on it"
    (with-redefs [kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  interact/confirm! (constantly true)]
      (let [seen (atom nil)
            spec {:select {:mode :one :label "Namespace" :flag :namespace
                           :candidates (fn [_context] ["ns-1" "ns-2"])}
                  :confirm {:action "do a thing"}
                  :effect (fn [{:keys [target]}] (reset! seen target) [])}]
        (is (true? (mutation/run! spec {:namespace "ns-2"} nil)))
        (is (= "ns-2" @seen) "the provided flag picks ns-2 without a prompt")))))

(deftest run-nothing-to-select-succeeds-quietly-test
  (testing "run! returns true (nothing to do) when there are no candidates"
    (with-redefs [kubectl/current-context (constantly "ctx")]
      (let [ran (atom false)
            spec {:select {:mode :one :label "Namespace" :flag :namespace
                           :candidates (fn [_context] [])}
                  :empty-message "every node is already cordoned"
                  :confirm {:action "x"}
                  :effect (fn [_] (reset! ran true) [])}]
        (is (true? (mutation/run! spec {} nil)))
        (is (false? @ran) "no candidates means the effect is never built or run")))))
