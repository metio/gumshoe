;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.bounded-waits-test
  "Books whose changes and post-checks must end on their own: a delete whose
   object a finalizer holds, or an HTTP call to a server that stopped
   answering, fails the bounded post-check instead of hanging the book."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.loopback-server :as loopback]
            [gumshoe.mutation :as mutation]
            [gumshoe.runbook :as runbook]))

(defn- load-spec
  [path]
  (with-redefs [mutation/book identity] (load-file path)))

(defn- load-book!
  [path]
  (binding [runbook/*loading* true] (load-file path)))

(defn- plan-of [effect-fn ctx]
  (effect/collect (effect/as-plan (effect-fn ctx))))

(deftest recreating-orders-does-not-wait-on-their-finalizer
  (load-book! "runbooks/kubernetes/orders/recreate.clj")
  (let [recreate-effect (requiring-resolve 'runbooks.kubernetes.orders.recreate/recreate-effect)]
    (is (= [[:kubectl "ctx" "--namespace=shop" "delete" "orders.acme.cert-manager.io" "o-1" "--ignore-not-found" "--interactive=false" "--wait=false"]]
           (effect/collect (effect/as-plan (recreate-effect "ctx" ["shop/o-1"])))))))

(deftest deleting-an-unused-claim-does-not-wait-on-pvc-protection
  (let [{:keys [effect]} (load-spec "runbooks/kubernetes/volumes/delete_unused.clj")]
    (is (= [[:kubectl "ctx" "--namespace=shop" "delete" "persistentvolumeclaim" "data" "--ignore-not-found" "--interactive=false" "--wait=false"]]
           (plan-of effect {:context "ctx" :target "shop/data"})))))

(defn- quietly [f]
  (binding [*out* (java.io.StringWriter.) *err* (java.io.StringWriter.)]
    (f)))

(defn- elapsed-ms [f]
  (let [started (System/nanoTime)
        result (f)]
    [result (quot (- (System/nanoTime) started) 1000000)]))

(deftest deleting-a-metric-bounds-its-http-calls
  (load-book! "tools/prometheus/runbooks/delete_metric.clj")
  (let [delete-series! (requiring-resolve 'runbooks.prometheus.delete-metric/delete-series!)
        series-gone? (requiring-resolve 'runbooks.prometheus.delete-metric/series-gone?)
        probe-timeouts (requiring-resolve 'runbooks.prometheus.delete-metric/series-probe-timeouts)]
    (testing "a delete that never answers fails after its timeout"
      (let [{:keys [port close]} (loopback/silent!)]
        (try
          (let [[deleted? ms] (elapsed-ms #(quietly (fn [] (delete-series! port "up" 1))))]
            (is (false? deleted?))
            (is (< ms 8000)))
          (finally (close)))))
    (testing "a series probe whose body drips is not gone, and ends within its deadline"
      (let [{:keys [port close]} (loopback/serve! "HTTP/1.1 200 OK\r\nContent-Length: 100000\r\n\r\n"
                                                  (cons "{\"status\":\"success\",\"data\":[]" (repeat 1000 " ")) 200)]
        (try
          (with-redefs-fn {probe-timeouts {:timeout-ms 1000 :deadline-ms 1500}}
            (fn []
              (let [[gone? ms] (elapsed-ms #(series-gone? port "up"))]
                (is (false? gone?))
                (is (< ms 5000)))))
          (finally (close)))))
    (testing "an answered empty series list is gone, a non-empty one is not"
      (let [body "{\"status\":\"success\",\"data\":[]}"
            {:keys [port close]} (loopback/serve! (str "HTTP/1.1 200 OK\r\nContent-Length: " (count body) "\r\n\r\n")
                                                  [body] 0)]
        (try (is (true? (series-gone? port "up"))) (finally (close))))
      (let [body "{\"status\":\"success\",\"data\":[{\"__name__\":\"up\"}]}"
            {:keys [port close]} (loopback/serve! (str "HTTP/1.1 200 OK\r\nContent-Length: " (count body) "\r\n\r\n")
                                                  [body] 0)]
        (try (is (false? (series-gone? port "up"))) (finally (close)))))
    (testing "the book exposes the delete timeout with a generous default"
      (let [{:keys [options]} (load-book! "tools/prometheus/runbooks/delete_metric.clj")]
        (is (<= 300 (get-in options [:delete-timeout :default])))
        (is (not (str/blank? (get-in options [:delete-timeout :desc]))))))))
