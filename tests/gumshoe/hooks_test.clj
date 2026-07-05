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
                      (reset! @#'hooks/pre-hooks [])
                      (t)
                      (reset! @#'hooks/post-hooks [])
                      (reset! @#'hooks/pre-hooks [])))

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

(deftest pre-hooks-allow-by-default-test
  (testing "with no pre-hooks, or all allowing, the run is allowed"
    (is (:allowed? (hooks/run-pre-hooks! {:change? true})))
    (hooks/register-pre-hook! (constantly true))
    (is (:allowed? (hooks/run-pre-hooks! {:change? true})))))

(deftest a-pre-hook-can-veto-with-a-reason-test
  (testing "returning {:allow? false :reason} blocks the run and surfaces the reason"
    (hooks/register-pre-hook! (fn [{:keys [change?]}]
                                (if change? {:allow? false :reason "change freeze"} true)))
    (let [blocked (hooks/run-pre-hooks! {:change? true})
          allowed (hooks/run-pre-hooks! {:change? false})]
      (is (false? (:allowed? blocked)))
      (is (= "change freeze" (:reason blocked)))
      (is (:allowed? allowed) "a read-only run is not blocked by this gate")))
  (testing "a bare false also vetoes, with a default reason"
    (reset! @#'hooks/pre-hooks [])
    (hooks/register-pre-hook! (constantly false))
    (is (false? (:allowed? (hooks/run-pre-hooks! {}))))))

(deftest a-throwing-pre-hook-fails-open-test
  (testing "a broken gate warns and allows, so it can never block emergency response"
    (let [err (java.io.StringWriter.)]
      (hooks/register-pre-hook! (fn [_] (throw (ex-info "boom" {}))))
      (binding [*err* err]
        (is (:allowed? (hooks/run-pre-hooks! {:change? true}))))
      (is (clojure.string/includes? (str err) "pre-hook failed")))))
