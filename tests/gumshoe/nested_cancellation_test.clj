;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.nested-cancellation-test
  "Work abandoned at a deadline stops, also the work it started on other
   threads: a post-check attempt's collector, and the fan-out of a collector
   through gumshoe.concurrent."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.http :as http]
            [gumshoe.loopback-server :as loopback]
            [gumshoe.shell :as shell]
            [gumshoe.verify :as verify])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- now [] (System/currentTimeMillis))

(defn- unique-key [] (str "nested-cancellation-" (random-uuid)))

(defn- slow-commands!
  "Runs `sleep 1` count times, logging each finished command with its time."
  [log tag count]
  (dotimes [i count]
    (shell/execute "sleep" "1")
    (swap! log conj [tag i (now)])))

(defn- finished-after [log t]
  (filterv (fn [[_ _ at]] (> at t)) @log))

(deftest an-abandoned-attempt-stops-the-collector-it-started
  (let [log (atom [])
        key (unique-key)
        _ (registry/register-collector! key (fn [_ _] (slow-commands! log :collector 4) {:ok true}))
        detectives [{:name "d" :requires [key] :detect (fn [_] [])}]
        result (verify/bounded-attempt #(detective/read-evidence "ctx" "c" detectives) 1500)
        abandoned (now)]
    (is (= "the check did not return within 1s" result))
    (Thread/sleep 3500)
    (testing "the command running at the deadline was stopped, and no further one ran"
      (is (= [] (finished-after log abandoned)) (pr-str @log)))
    (is (<= (count @log) 1) (pr-str @log))))

(deftest an-abandoned-attempt-stops-a-collector-fan-out
  (let [log (atom [])
        key (unique-key)
        _ (registry/register-collector!
           key (fn [_ _] (concurrent/map-all #(slow-commands! log % 4) [:a :b]) {:ok true}))
        detectives [{:name "d" :requires [key] :detect (fn [_] [])}]
        result (verify/bounded-attempt #(detective/read-evidence "ctx" "c" detectives) 1500)
        abandoned (now)]
    (is (= "the check did not return within 1s" result))
    (Thread/sleep 3500)
    (is (= [] (finished-after log abandoned)) (pr-str @log))))

(deftest a-collector-timeout-stops-its-fan-out
  (let [log (atom [])
        key (unique-key)
        _ (registry/register-collector!
           key (fn [_ _] (concurrent/map-all #(slow-commands! log % 4) [:a :b]) {:ok true}))
        detectives [{:name "d" :requires [key] :detect (fn [_] [])}]
        thrown (binding [detective/*collector-timeout-ms* 1500]
                 (try (detective/read-evidence "ctx" "c" detectives) nil
                      (catch clojure.lang.ExceptionInfo e e)))
        abandoned (now)]
    (is (some? thrown))
    (Thread/sleep 3500)
    (is (= [] (finished-after log abandoned)) (pr-str @log))))

(deftest an-interrupted-caller-cancels-every-thunk-of-all
  (let [started (CountDownLatch. 2)
        events (atom [])
        thunk (fn [tag]
                (fn []
                  (.countDown started)
                  (try
                    (loop [] (Thread/sleep 50) (swap! events conj [tag :tick]) (recur))
                    (catch InterruptedException _
                      (swap! events conj [tag :interrupted])))))
        outcome (promise)
        caller (Thread. ^Runnable
                (fn []
                  (deliver outcome
                           (try (concurrent/all [(thunk :a) (thunk :b)]) :returned
                                (catch InterruptedException _
                                  {:interrupt-flag (.isInterrupted (Thread/currentThread))})))))]
    (.start caller)
    (is (.await started 5 TimeUnit/SECONDS))
    (.interrupt caller)
    (is (= {:interrupt-flag true} (deref outcome 5000 :hung)))
    (Thread/sleep 300)
    (let [seen @events]
      (Thread/sleep 300)
      (testing "both thunks were interrupted and then stopped"
        (is (= #{[:a :interrupted] [:b :interrupted]} (set (filter #(= :interrupted (second %)) seen))))
        (is (= seen @events) "no thunk ticked after it was cancelled")))))

(deftest all-still-settles-every-thunk-before-rethrowing-a-failure
  (let [finished (atom #{})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"boom"
                          (concurrent/all [#(throw (ex-info "boom" {}))
                                           #(do (Thread/sleep 200) (swap! finished conj :slow))])))
    (is (= #{:slow} @finished))))

(deftest map-all-keeps-order-and-honours-parallelism
  (let [running (atom 0)
        peak (atom 0)
        f (fn [x]
            (let [n (swap! running inc)]
              (swap! peak max n)
              (Thread/sleep 50)
              (swap! running dec)
              (* 10 x)))]
    (is (= [10 20 30 40 50 60] (concurrent/map-all f [1 2 3 4 5 6] {:parallelism 2})))
    (is (<= @peak 2))
    (is (= [] (concurrent/map-all f [])))
    (is (= [70] (concurrent/map-all f [7])))))

(deftest map-all-conveys-bindings
  (let [v (concurrent/map-all (fn [_] detective/*collector-timeout-ms*) [1 2 3])]
    (is (= [300000 300000 300000] v))
    (is (= [7 7] (binding [detective/*collector-timeout-ms* 7]
                   (concurrent/map-all (fn [_] detective/*collector-timeout-ms*) [1 2]))))))

(deftest an-interrupted-http-read-answers-and-keeps-the-interrupt
  (let [{:keys [url close]} (loopback/serve! "HTTP/1.1 200 OK\r\nContent-Length: 100000\r\n\r\n" ["a"] 0)
        outcome (promise)
        caller (Thread. ^Runnable
                (fn []
                  (let [response (http/fetch url {} {:timeout-ms 20000 :deadline-ms 20000})]
                    (deliver outcome {:reachable (:reachable response)
                                      :interrupt-flag (.isInterrupted (Thread/currentThread))}))))]
    (try
      (.start caller)
      (Thread/sleep 500)
      (.interrupt caller)
      (testing "the read ends at the interrupt, and the caller is still marked cancelled"
        (is (= {:reachable false :interrupt-flag true} (deref outcome 5000 :hung))))
      (finally (close)))))
