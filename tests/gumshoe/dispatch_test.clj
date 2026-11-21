;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dispatch-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.dispatch :as dispatch]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]))

(defn- route
  [release ran]
  {:playbook {:select {:mode :one :label "Namespace"}
              :confirm {:action (str "adopt a " release)}
              :steps [{:options {:release {:desc "The release" :alias :r :default release :coerce :string}}
                       :prerequisites {:installed-tools ["kubectl" release]}
                       :select {:mode :one :label "Namespace"}
                       :confirm {:action (str "install " release)}
                       :effect (fn [{:keys [target opts]}]
                                 (swap! ran conj [target (:release opts)])
                                 [[:note "install"]])}]}
   :note (str release " note")})

(defn- dispatcher
  [ran]
  {:select {:mode :one :label "Namespace" :flag :namespace
            :candidates (constantly ["ns-moodle" "ns-wiki" "ns-plain"])}
   :classify (fn [_ctx target] ({"ns-moodle" :moodle "ns-wiki" :hedgedoc "ns-other" :nextcloud} target))
   :routes {:moodle (route "moodle" ran) :hedgedoc (route "hedgedoc" ran)}})

(defmacro ^:private quietly
  [& body]
  `(binding [*err* (java.io.StringWriter.) *out* (java.io.StringWriter.)]
     (with-redefs [kubectl/current-context (constantly "ctx")
                   kubectl/current-cluster (constantly "cluster")
                   interact/confirm! (constantly true)]
       ~@body)))

(deftest the-dispatcher-offers-every-route-without-their-defaults
  (let [options (dispatch/all-options (dispatcher (atom [])))]
    (is (= {:desc "The release" :alias :r :coerce :string} (:release options)))))

(deftest the-dispatcher-needs-what-every-route-needs
  (is (= ["kubectl" "moodle" "hedgedoc"]
         (:installed-tools (dispatch/all-prerequisites (dispatcher (atom [])))))))

(deftest the-subject-runs-its-route-with-that-route-s-defaults
  (let [ran (atom [])]
    (quietly
     (is (true? (dispatch/run! (dispatcher ran) {:namespace "ns-wiki"} nil)))
     (is (true? (dispatch/run! (dispatcher ran) {:namespace "ns-moodle" :release "mine"} nil))))
    (is (= [["ns-wiki" "hedgedoc"] ["ns-moodle" "mine"]] @ran))))

(deftest a-subject-without-a-route-runs-nothing
  (let [ran (atom [])]
    (quietly
     (testing "it can not say what it is"
       (is (false? (dispatch/run! (dispatcher ran) {:namespace "ns-plain"} nil))))
     (testing "it says, and nothing routes there"
       (is (false? (dispatch/run-on! (dispatcher ran) "ns-other" {} nil)))))
    (is (empty? @ran))))

(deftest a-lone-mutation-spec-is-a-route
  (let [spec {:select {:mode :one} :confirm {:action "x"} :effect (constantly [])}]
    (is (= [spec] (:steps (dispatch/playbook-of {:playbook spec}))))))

(deftest help-lists-the-routes
  (is (= ["What it does:"
          "  picks one Namespace (--namespace, or interactively) and runs the route for what it is:"
          "  hedgedoc - adopt a hedgedoc (hedgedoc note)"
          "  moodle - adopt a moodle (moodle note)"
          "  each route's --help lists its steps"]
         (take 5 (dispatch/details (dispatcher (atom [])))))))
