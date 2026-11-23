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

(deftest help-describes-the-mutation-from-its-spec
  (is (= ["What it does:"
          "  picks one Namespace (--namespace, or interactively)"
          "  asks to confirm: delete it (destructive - type yes)"
          "  checks again right before acting that what was confirmed still holds"
          "  has no post-check"
          ""
          "Needs:"
          "  tools: kubectl"]
         (mutation/details {:select {:mode :one :label "Namespace" :flag :namespace}
                            :confirm {:action "delete it" :destructive? true}
                            :precondition (constantly true)
                            :prerequisites {:installed-tools ["kubectl"]}}))))

(deftest an-unreadable-cluster-is-not-nothing-to-do
  (let [spec {:select {:mode :one :label "Node" :candidates #(map kubectl/name-of (kubectl/items-of (kubectl/get-all % "nodes")))}
              :empty-message "every node is already cordoned"}
        out (java.io.StringWriter.)]
    (with-redefs [kubectl/current-context (constantly "ctx")
                  gumshoe.shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server: dial tcp: i/o timeout"})]
      (binding [*out* out *err* out]
        (is (false? (mutation/run! spec {} {})))))
    (is (not (clojure.string/includes? (str out) "every node is already cordoned")))
    (testing "an answered read with nothing in it is still nothing to do"
      (with-redefs [kubectl/current-context (constantly "ctx")
                    gumshoe.shell/execute (fn [& _] {:exit 0 :out "{\"items\":[]}" :err ""})]
        (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
          (is (true? (mutation/run! spec {} {}))))))
    (testing "a type the cluster does not serve has no candidates"
      (with-redefs [kubectl/current-context (constantly "ctx")
                    gumshoe.shell/execute (fn [& _] {:exit 1 :out "" :err "error: the server doesn't have a resource type \"widgets\""})]
        (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
          (is (true? (mutation/run! spec {} {}))))))))
