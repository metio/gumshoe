;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.verify-test
  (:require [clojure.string]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.verify :as verify]))

(deftest eventually-test
  (testing "passes immediately when the check holds"
    (is (verify/eventually {:description "holds" :timeout 0 :check (constantly true)})))
  (testing "fails when the check never holds within the timeout"
    (is (not (verify/eventually {:description "never" :timeout 0 :check (constantly false)}))))
  (testing "retries until the check holds"
    (let [attempts (atom 0)]
      (is (verify/eventually {:description "third time lucky"
                              :timeout 10
                              :interval 0
                              :check #(<= 3 (swap! attempts inc))}))
      (is (= 3 @attempts))))
  (testing "an exception inside the check counts as not-yet-verified"
    (is (not (verify/eventually {:description "throws" :timeout 0
                                 :check #(throw (ex-info "boom" {}))})))))

(deftest malformed-post-check-test
  (testing "a post-check without :description or :check is refused, never 'verified'"
    (is (not (verify/eventually {:check (constantly true)})))
    (is (not (verify/eventually {:description "no check function"})))
    (is (not (verify/eventually {:description "check is not a function" :check true})))))

(deftest soft-check-test
  (testing "a soft check that never passes times out as best-effort success, not failure"
    (is (verify/eventually {:description "soft" :timeout 0 :soft? true :check (constantly false)})))
  (testing "a soft check still returns true immediately when it passes"
    (is (verify/eventually {:description "soft-ok" :timeout 0 :soft? true :check (constantly true)})))
  (testing "a hard check that never passes still fails"
    (is (not (verify/eventually {:description "hard" :timeout 0 :check (constantly false)})))))

(deftest watch-surfaces-signals-test
  (testing "the watcher runs during the wait and new signals are gathered without failing the check"
    (let [samples (atom 0)
          watch (fn [] (swap! samples inc) ["a Warning event appeared"])]
      ;; a soft check that never passes, with a tiny interval, exercises the watch path
      (is (verify/eventually {:description "watched" :timeout 1 :interval 0 :soft? true
                              :check (constantly false) :watch watch}))
      (is (pos? @samples)))))

(deftest watch-primes-baseline-test
  (testing "the pre-existing signal backlog is not reported as if it appeared during the wait"
    (let [calls (atom 0)
          watch (fn []
                  (swap! calls inc)
                  ;; the pre-existing signal is present from the baseline sample on;
                  ;; a genuinely new one only shows up after a couple of intervals
                  (if (>= @calls 3)
                    ["pre-existing warning" "a NEW resizer error"]
                    ["pre-existing warning"]))
          err (java.io.StringWriter.)]
      (binding [*err* err]
        (verify/eventually {:description "watched" :timeout 2 :interval 0 :soft? true
                            :check (constantly false) :watch watch}))
      (let [out (str err)]
        (is (clojure.string/includes? out "a NEW resizer error")
            "a signal that appears during the wait is surfaced")
        (is (not (clojure.string/includes? out "while waiting — pre-existing warning"))
            "the pre-existing backlog is not surfaced as a during-wait signal")))))

(deftest all-test
  (testing "every check must hold"
    (is (verify/all [{:description "a" :timeout 0 :check (constantly true)}
                     {:description "b" :timeout 0 :check (constantly true)}]))
    (is (not (verify/all [{:description "a" :timeout 0 :check (constantly true)}
                          {:description "b" :timeout 0 :check (constantly false)}])))))
