;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.select-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.select :as select]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(deftest nothing-selected-test
  (testing "a single pick is empty only when nil"
    (is (select/nothing-selected? :one nil))
    (is (not (select/nothing-selected? :one "node-1")))
    (is (select/nothing-selected? :namespaced nil))
    (is (not (select/nothing-selected? :namespaced "ns/name"))))
  (testing "a multi pick is empty only when it has no items"
    (is (select/nothing-selected? :many []))
    (is (select/nothing-selected? :many nil))
    (is (not (select/nothing-selected? :many ["a" "b"])))))

(deftest items-test
  (testing "a single pick becomes a one-item list, a multi pick becomes its vector"
    (is (= ["node-1"] (select/items :one "node-1")))
    (is (= ["ns/name"] (select/items :namespaced "ns/name")))
    (is (= ["a" "b"] (select/items :many ["a" "b"])))))

;; A valid provided flag value is returned by interact/choose-one without a
;; prompt, so resolve-target/pick are exercisable without a TTY.
(def ^:private one-spec
  {:mode :one :label "Node" :flag :node
   :candidates (fn [_context] ["node-1" "node-2"])
   :empty-message "every node is already cordoned"})

(deftest resolve-target-test
  (testing "a provided valid value resolves to itself with the candidate list"
    (is (= {:candidates ["node-1" "node-2"] :target "node-1"}
           (select/resolve-target one-spec {:node "node-1"} "ctx")))))

(deftest pick-test
  (testing "a provided valid pick is returned"
    (is (= "node-1" (select/pick one-spec {:node "node-1"} "ctx"))))
  (testing "no candidates yields nil (the caller sees the :empty-message)"
    (is (nil? (select/pick (assoc one-spec :candidates (fn [_] [])) {} "ctx"))))
  (testing "candidates but no pick yields nil"
    (is (nil? (select/pick one-spec {} "ctx")))))

(defn- printed
  "[result stderr-without-colors] of f."
  [f]
  (let [err (java.io.StringWriter.)
        result (binding [*err* err] (f))]
    [result (stdout/strip-colors (str err))]))

(deftest pick-reports-what-left-it-empty-handed
  (testing "an empty candidate list prints the :empty-message as a success by default"
    (let [[result err] (printed #(select/pick (assoc one-spec :candidates (fn [_] [])) {} "ctx"))]
      (is (nil? result))
      (is (str/includes? err "✅ every node is already cordoned"))))
  (testing "with :on-empty :fail it prints the :empty-message as an error"
    (let [[result err] (printed #(select/pick (assoc one-spec :candidates (fn [_] []) :on-empty :fail) {} "ctx"))]
      (is (nil? result))
      (is (str/includes? err "❌ every node is already cordoned"))))
  (testing "a candidate read that did not answer is reported as such, never as an empty list"
    (let [[result err] (printed
                        #(with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "i/o timeout"})]
                           (select/pick (assoc one-spec
                                               :candidates (fn [context] (kubectl/names-of (kubectl/get-all context "nodes")))
                                               :on-empty :fail)
                                        {:node "node-1"} "ctx")))]
      (is (nil? result))
      (is (str/includes? err "could not list the Node candidates - kubectl get did not answer: i/o timeout"))
      (is (not (str/includes? err "cordoned"))))))
