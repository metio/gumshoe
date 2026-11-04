;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.shell-test
  (:require [babashka.process :as process]
            [clojure.test :refer [deftest is testing]]
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

(deftest pipe-to-feeds-stdin-and-reports-the-exit-test
  (testing "the input reaches the command"
    (is (zero? (shell/pipe-to! "hello" "sh" "-c" "read line; test \"$line\" = hello"))))
  (testing "a command that rejects the input reports non-zero"
    (is (pos? (shell/pipe-to! "nope" "sh" "-c" "read line; test \"$line\" = hello"))))
  (testing "a missing binary is an exit code, never a thrown stack trace"
    (is (pos? (shell/pipe-to! "x" "this-binary-does-not-exist-xyz")))))

(deftest pipe-to-does-not-hand-our-stdout-to-a-daemon-test
  (testing "a reader of our stdout sees EOF even when the child outlives us"
    ;; A clipboard tool forks a server that lives on after the parent exits.
    ;; Inheriting our stdout hands that server the write end of `gumshoe | tee`,
    ;; which then never sees EOF. Run it for real in a child whose output we
    ;; read to the end: the read returns only if nothing else holds the pipe.
    (let [script (str "(require '[gumshoe.shell :as shell])"
                      "(shell/pipe-to! \"x\" \"sh\" \"-c\" \"(sleep 20) & exit 0\")"
                      "(println \"parent-done\")")
          proc (process/process {:out :string :err :string :continue true}
                                "bb" "-cp" (System/getProperty "java.class.path") "-e" script)
          result (deref (future @proc) 15000 ::timeout)]
      (when (= ::timeout result) (process/destroy-tree proc))
      (is (not= ::timeout result)
          "reading the child's stdout to EOF must not wait on the daemon it spawned")
      (is (clojure.string/includes? (str (:out result)) "parent-done")))))

(deftest pipe-to-gives-up-on-a-command-that-never-exits-test
  (let [started (System/currentTimeMillis)
        code (shell/pipe-to! "x" "sleep" "30")
        elapsed (- (System/currentTimeMillis) started)]
    (is (pos? code))
    (is (< elapsed 10000) "the deadline bounds the wait rather than freezing the caller")))
