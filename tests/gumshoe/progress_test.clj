;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.progress-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.progress :as progress]))

;; Tests run off a terminal, so the plain (non-animated) path is exercised - the
;; behaviour that matters: results and control flow, not the cursor choreography.

(deftest watch-futures!-test
  (testing "returns the futures' values in order once all complete"
    (is (= [1 2 3] (progress/watch-futures! [["a" (future 1)] ["b" (future 2)] ["c" (future 3)]]))))
  (testing "an empty task list is fine"
    (is (= [] (progress/watch-futures! [])))))

(deftest checklist-test
  (testing "all steps succeeding returns true and runs every step"
    (let [ran (atom [])]
      (is (true? (progress/checklist [["one" (fn [] (swap! ran conj 1) true)]
                                      ["two" (fn [] (swap! ran conj 2) true)]])))
      (is (= [1 2] @ran))))
  (testing "a failing step returns false and stops the rest"
    (let [ran (atom [])]
      (is (false? (progress/checklist [["one" (fn [] (swap! ran conj 1) true)]
                                       ["two" (fn [] (swap! ran conj 2) false)]
                                       ["three" (fn [] (swap! ran conj 3) true)]])))
      (is (= [1 2] @ran) "the step after the failure never runs")))
  (testing "a step that throws counts as failed, not a crash"
    (is (false? (progress/checklist [["boom" (fn [] (throw (ex-info "nope" {})))]]))))
  (testing "with stop-on-failure? false, every step runs and the result is whether all passed"
    (let [ran (atom [])]
      (is (false? (progress/checklist [["one" (fn [] (swap! ran conj 1) true)]
                                       ["two" (fn [] (swap! ran conj 2) false)]
                                       ["three" (fn [] (swap! ran conj 3) true)]]
                                      {:stop-on-failure? false})))
      (is (= [1 2 3] @ran) "a failure does not stop the rest - a prerequisites gate shows every problem")))
  (testing "a thunk returning {:ok? ...} is honoured (the :label is for display only)"
    (is (true? (progress/checklist [["probe" (fn [] {:ok? true :label "kubectl (v1.36)"})]])))
    (is (false? (progress/checklist [["probe" (fn [] {:ok? false :label "kubectl missing"})]])))))

(deftest animation-follows-stderr
  (testing "the repaint goes to stderr, so a redirected stderr gets no cursor movement"
    (is (false? (progress/animate? {:stderr-terminal? false :stdout-terminal? true :colors-enabled? true})))
    (is (false? (progress/animate? {:stderr-terminal? false :stdout-terminal? false :colors-enabled? true}))
        "FORCE_COLOR in CI asks for colour, not for repaints into a log"))
  (testing "a terminal on stderr animates even when stdout is captured"
    (is (true? (progress/animate? {:stderr-terminal? true :stdout-terminal? false :colors-enabled? false})))
    (is (true? (progress/animate? {:stderr-terminal? true :stdout-terminal? true :colors-enabled? true}))))
  (testing "NO_COLOR and --no-color stop it"
    (is (false? (progress/animate? {:stderr-terminal? true :stdout-terminal? false :no-color? true})))
    (is (false? (progress/animate? {:stderr-terminal? true :stdout-terminal? true :colors-enabled? false})))))

(deftest a-redirected-stderr-gets-no-cursor-escapes
  (let [err (java.io.StringWriter.)]
    (with-redefs-fn {#'progress/terminal? (fn [fd] (= 1 fd))
                     #'gumshoe.stdout/colors-enabled? (constantly true)}
      #(binding [*err* err]
         (progress/watch-futures! [["a" (future (Thread/sleep 200) 1)]])
         (progress/checklist [["b" (fn [] (Thread/sleep 200) true)]])))
    (is (not (re-find #"\u001b\[\d+A" (str err))) "no cursor-up repaints")
    (is (not (re-find #"\u001b\[K" (str err))) "no line clears")))
