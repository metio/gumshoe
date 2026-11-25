;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.concurrent-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation])
  (:import (java.util.concurrent CountDownLatch TimeUnit)))

(defn- meeting
  "A thunk factory whose thunks each wait for all n of them to have
   started, true when they met within a few seconds. Thunks run one after
   another never meet: the first waits alone and returns false."
  [n]
  (let [latch (CountDownLatch. n)]
    (fn []
      (.countDown latch)
      (.await latch 5 TimeUnit/SECONDS))))

(deftest values-keep-the-order-given
  (is (= [1 2 3] (concurrent/all [#(do 1) #(do 2) #(do 3)])))
  (is (= [] (concurrent/all [])))
  (is (= [:one] (concurrent/all [#(do :one)])))
  (is (= {:a 1 :b 2} (concurrent/values {:a #(do 1) :b #(do 2)}))))

(deftest thunks-run-at-the-same-time
  (let [meet (meeting 3)]
    (is (= [true true true] (concurrent/all [meet meet meet]))
        "every thunk was running while the others were")))

(deftest the-caller-s-bindings-hold-in-every-thunk
  (let [cache (kubectl/read-cache)
        seen (binding [kubectl/*strict-reads* true
                       kubectl/*read-cache* cache
                       mutation/*phase* :judge]
               (concurrent/all (repeat 2 (fn [] [kubectl/*strict-reads* kubectl/*read-cache* mutation/*phase*]))))]
    (is (= [[true cache :judge] [true cache :judge]] seen)))
  (testing "and none outside them"
    (is (= [[false nil nil] [false nil nil]]
           (concurrent/all (repeat 2 (fn [] [kubectl/*strict-reads* kubectl/*read-cache* mutation/*phase*])))))))

(deftest a-failure-is-rethrown-as-thrown-after-every-thunk-settled
  (let [finished (atom false)
        thrown (try
                 (concurrent/all [#(throw (ex-info "kubectl get did not answer" {::kubectl/unread true}))
                                  #(do (Thread/sleep 100) (reset! finished true))])
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
    (is (= "kubectl get did not answer" (ex-message thrown)))
    (is (::kubectl/unread (ex-data thrown)) "the ex-data a caller dispatches on survives")
    (is (true? @finished) "the slower thunk finished before the call returned"))
  (testing "the first failure in the order given wins"
    (is (= "first" (try (concurrent/all [#(do (Thread/sleep 50) (throw (ex-info "first" {})))
                                         #(throw (ex-info "second" {}))])
                        (catch clojure.lang.ExceptionInfo e (ex-message e)))))))
