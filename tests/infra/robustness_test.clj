;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.robustness-test
  "Locks in the failsafe behaviors of the foundation, so a future change can
   never silently reintroduce a crash-on-unexpected-input."
  (:require [clojure.test :refer [deftest is testing]]
            [infra.detective :as detective]
            [infra.http :as http]
            [infra.shell :as shell]))

(deftest shell-never-throws-on-missing-binary
  (testing "a missing binary is a failed result, not an exception"
    (let [result (shell/execute "this-binary-truly-does-not-exist-xyz")]
      (is (= 127 (:exit result)))
      (is (= "" (:out result)))
      (is (string? (:err result)))))
  (testing "the derived helpers stay safe too"
    (is (= "" (shell/stdout-of "this-binary-truly-does-not-exist-xyz")))
    (is (= 127 (shell/exit-code-of "this-binary-truly-does-not-exist-xyz")))
    (is (= 127 (shell/run-with-output "this-binary-truly-does-not-exist-xyz")))))

(deftest http-never-throws-on-bad-host
  (testing "an unresolvable host is an unreachable result, not an exception"
    (let [response (http/fetch "http://this-host-does-not-resolve.invalid/")]
      (is (false? (:reachable response)))
      (is (string? (:error response)))
      (is (not (http/ok? response))))))

(deftest a-throwing-detective-does-not-sink-the-investigation
  (testing "one broken detective yields a warning; the others still report"
    (let [detectives [{:name "boom" :requires [] :detect (fn [_] (throw (ex-info "kaboom" {})))}
                      {:name "fine" :requires []
                       :detect (fn [_] [{:severity :info :component "x" :summary "ok"}])}]
          findings (detective/run-detectives detectives {})]
      (is (= 2 (count findings)))
      (is (contains? (set (map :summary findings)) "ok"))
      (is (contains? (set (map :summary findings)) "detective failed to run - investigate its input"))
      (is (= "boom" (:detective (first (filter #(= :warning (:severity %)) findings)))))))
  (testing "a LAZILY throwing detective (the real shape - for-comprehensions) is isolated too"
    (let [detectives [{:name "lazy-boom" :requires []
                       :detect (fn [_] (for [x [1]] (throw (ex-info "lazy kaboom" {:x x}))))}
                      {:name "fine" :requires []
                       :detect (fn [_] [{:severity :info :component "x" :summary "ok"}])}]
          findings (detective/run-detectives detectives {})]
      (is (= 2 (count findings)))
      (is (contains? (set (map :summary findings)) "ok"))
      (is (= "lazy-boom" (:detective (first (filter #(= :warning (:severity %)) findings))))))))
