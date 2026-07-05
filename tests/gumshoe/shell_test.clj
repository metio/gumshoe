;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.shell-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.shell :as shell]))

(deftest capture-basics-test
  (testing "a normal command returns its output and a zero exit"
    (let [r (shell/execute "echo" "hello")]
      (is (= 0 (:exit r)))
      (is (= "hello" (clojure.string/trim (:out r))))))
  (testing "stdout-of trims, exit-code-of reports the code"
    (is (= "hello" (shell/stdout-of "echo" "hello")))
    (is (= 0 (shell/exit-code-of "true")))
    (is (= 1 (shell/exit-code-of "false"))))
  (testing "a missing binary is a non-zero exit, never a thrown stack trace"
    (is (= 127 (:exit (shell/execute "this-binary-does-not-exist-xyz"))))))

(deftest capture-always-returns-test
  (testing "a captured command that would hang is stopped at the deadline"
    (binding [shell/*timeout-ms* 500]
      (let [r (shell/execute "sleep" "30")]
        (is (= 124 (:exit r)))
        (is (clojure.string/includes? (:err r) "did not finish")))))
  (testing "a command that finishes within the deadline is untouched"
    (binding [shell/*timeout-ms* 5000]
      (is (= 0 (:exit (shell/execute "echo" "quick"))))))
  (testing "a nil bound runs unbounded (the escape hatch for long captures)"
    (binding [shell/*timeout-ms* nil]
      (is (= 0 (:exit (shell/execute "echo" "unbounded")))))))
