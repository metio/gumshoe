;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.hooks-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gumshoe.hooks :as hooks]))

;; The registry is a process-wide atom; reset it around each test so one test's
;; hooks never fire in another.
(use-fixtures :each (fn [t]
                      (reset! @#'hooks/post-hooks [])
                      (t)
                      (reset! @#'hooks/post-hooks [])))

(deftest runs-registered-hooks-with-context-test
  (testing "every registered hook is called with the run context"
    (let [seen (atom [])]
      (hooks/register-post-hook! (fn [ctx] (swap! seen conj (:outcome ctx))))
      (hooks/register-post-hook! (fn [ctx] (swap! seen conj (:description ctx))))
      (hooks/run-post-hooks! {:outcome :ok :description "drain a node"})
      (is (= [:ok "drain a node"] @seen)))))

(deftest a-throwing-hook-is-warned-not-fatal-test
  (testing "a hook that throws is warned about and the others still run"
    (let [ran (atom false)
          err (java.io.StringWriter.)]
      (hooks/register-post-hook! (fn [_] (throw (ex-info "boom" {}))))
      (hooks/register-post-hook! (fn [_] (reset! ran true)))
      (binding [*err* err]
        (hooks/run-post-hooks! {:outcome :failed}))
      (is (true? @ran) "a later hook still runs after an earlier one throws")
      (is (str/includes? (str err) "post-hook failed")))))

(deftest no-hooks-is-a-no-op-test
  (testing "with nothing registered, running hooks does nothing and never throws"
    (is (nil? (hooks/run-post-hooks! {:outcome :ok})))
    (is (zero? (hooks/registered-count)))))
