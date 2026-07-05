;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.effect-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]))

(deftest constructors-test
  (testing "effects are plain data an operator (and a test) can read"
    (is (= [:kubectl "prod" "cordon" "node-1"] (effect/kubectl "prod" "cordon" "node-1")))
    (is (= [:ssh {:host "h"} "ceph" "-s"] (effect/ssh {:host "h"} "ceph" "-s")))
    (is (= [:note "hello"] (effect/note "hello"))))
  (testing "plan collects effects and drops the nils, so optional pieces compose"
    (is (= [[:kubectl "p" "cordon" "n"] [:note "done"]]
           (effect/plan (effect/kubectl "p" "cordon" "n")
                        (when false (effect/note "skipped"))
                        (effect/note "done"))))))

(deftest plans-compose-test
  (testing "two plans concatenate into one bigger plan"
    (is (= [[:kubectl "p" "a"] [:kubectl "p" "b"]]
           (into (effect/plan (effect/kubectl "p" "a"))
                 (effect/plan (effect/kubectl "p" "b")))))))

(deftest describe-test
  (testing "dry-run wording hides the context noise and reads like the CLI"
    (is (= "kubectl cordon node-1" (effect/describe (effect/kubectl "prod" "cordon" "node-1"))))
    (is (= "ssh mgr-1 -- ceph -s" (effect/describe (effect/ssh {:host "mgr-1"} "ceph" "-s"))))
    (is (= "# taking a backup" (effect/describe (effect/note "taking a backup"))))))

(deftest collect-is-the-test-interpreter
  (testing "collect returns the exact effects a book would emit - the mutation, asserted, no cluster"
    (let [cordon-plan (effect/plan (effect/kubectl "prod" "cordon" "node-7"))]
      (is (= [[:kubectl "prod" "cordon" "node-7"]] (effect/collect cordon-plan))))))

(deftest run-control-flow-test
  (testing "run! succeeds when every effect succeeds"
    (is (true? (effect/run! [(effect/note "a") (effect/note "b")]))))
  (testing "run! stops and fails at the first bad effect"
    (is (false? (effect/run! [(effect/note "ok") [:bogus-op "x"] (effect/note "never")]))))
  (testing "dry-run always succeeds and runs nothing"
    (is (true? (effect/dry-run [(effect/kubectl "p" "delete" "everything")])))))

(deftest custom-effect-type-describes-and-performs-test
  (let [ran (atom nil)]
    (effect/register-effect-type! :spy
                                  {:describe (fn [args] (str "spy on " (first args)))
                                   :perform (fn [args] (reset! ran (first args)) true)})
    (testing "describe uses the plugin's renderer, so --dry-run and the preview stay honest"
      (is (= "spy on target" (effect/describe [:spy "target"])))
      (is (true? (effect/dry-run [[:spy "target"]])) "dry-run never performs")
      (is (nil? @ran) "dry-run did not perform"))
    (testing "run! dispatches to the plugin's perform"
      (is (true? (effect/run! [[:spy "target"]])))
      (is (= "target" @ran)))
    (reset! @#'effect/effect-types {})))

(deftest custom-effect-type-requires-both-handlers-test
  (is (thrown? Throwable (effect/register-effect-type! :bad {:perform (fn [_] true)}))
      ":describe is mandatory - a custom effect can not act without a preview")
  (is (thrown? Throwable (effect/register-effect-type! :bad {:describe (fn [_] "x")}))))
