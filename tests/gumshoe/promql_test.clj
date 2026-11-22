;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.promql-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.promql :as promql]
            [org.httpkit.server :as server])
  (:import (java.net URLDecoder)
           (java.nio.charset StandardCharsets)
           (java.time Instant)))

(defn- params [url]
  (into {} (for [kv (str/split (second (str/split url #"\?" 2)) #"&")
                 :let [[k v] (str/split kv #"=" 2)]]
             [k (URLDecoder/decode ^String v StandardCharsets/UTF_8)])))

(deftest endpoints-per-flavour
  (testing "prometheus queries /api/v1 and adds nothing"
    (let [url (promql/query-url (promql/endpoint {:url "https://p.example.org/"}) "up" {})]
      (is (str/starts-with? url "https://p.example.org/api/v1/query?"))
      (is (= {"query" "up"} (params url)))))
  (testing "thanos deduplicates and refuses a partial response"
    (is (= {"query" "up" "dedup" "true" "partial_response" "false"}
           (params (promql/query-url (promql/endpoint {:url "https://t" :flavor :thanos}) "up" {})))))
  (testing "mimir lives below /prometheus and names its tenant in a header"
    (let [ep (promql/endpoint {:url "https://m" :flavor :mimir :tenant "team-a"})]
      (is (str/starts-with? (promql/query-url ep "up" {}) "https://m/prometheus/api/v1/query?"))
      (is (= {"X-Scope-OrgID" "team-a"} (:headers ep)))))
  (testing "a tenant means nothing outside mimir"
    (is (= {} (:headers (promql/endpoint {:url "https://p" :tenant "team-a"})))))
  (testing "basic auth needs both halves"
    (is (= {"Authorization" "Basic dTpw"} (:headers (promql/endpoint {:url "https://p" :username "u" :password "p"}))))
    (is (= {} (:headers (promql/endpoint {:url "https://p" :username "u"})))))
  (testing "an unknown flavour is refused"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown PromQL flavour" (promql/endpoint {:url "x" :flavor :victoria})))))

(deftest query-parameters-are-encoded
  (let [ep (promql/endpoint {:url "https://p"})
        promql "count by (node) (up{job=~\"node|x\"})"]
    (is (= promql (get (params (promql/query-url ep promql {})) "query")))
    (is (= "1700000000" (get (params (promql/query-url ep "up" {:time (Instant/ofEpochSecond 1700000000)})) "time")))
    (is (= {"query" "up" "start" "10" "end" "20" "step" "5s"}
           (params (promql/query-range-url ep "up" {:start 10 :end 20 :step "5s"}))))))

(defn- answer [json & {:keys [status] :or {status 200}}]
  {:reachable true :status status :json json})

(deftest parsing-answers
  (testing "a vector becomes series with string label names and double values"
    (is (= {:ok true :type :vector
            :series [{:labels {"__name__" "up" "instance" "a:9100"} :value 1.0 :at (Instant/ofEpochMilli 1700000000500)}]}
           (promql/parse (answer {:status "success"
                                  :data {:resultType "vector"
                                         :result [{:metric {:__name__ "up" :instance "a:9100"} :value [1700000000.5 "1"]}]}})))))
  (testing "a matrix keeps every sample"
    (is (= [{:at (Instant/ofEpochSecond 1) :value 1.0} {:at (Instant/ofEpochSecond 2) :value 2.0}]
           (-> (promql/parse (answer {:status "success"
                                      :data {:resultType "matrix" :result [{:metric {} :values [[1 "1"] [2 "2"]]}]}}))
               :series first :samples))))
  (testing "a scalar is one sample"
    (is (= [{:at (Instant/ofEpochSecond 3) :value 42.0}]
           (:series (promql/parse (answer {:status "success" :data {:resultType "scalar" :result [3 "42"]}}))))))
  (testing "NaN and the infinities are values, not failures"
    (let [values (map :value (:series (promql/parse (answer {:status "success"
                                                             :data {:resultType "vector"
                                                                    :result [{:metric {} :value [1 "NaN"]}
                                                                             {:metric {} :value [1 "+Inf"]}
                                                                             {:metric {} :value [1 "-Inf"]}]}}))))]
      (is (Double/isNaN (first values)))
      (is (= [Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY] (rest values)))))
  (testing "warnings travel with the answer"
    (is (= ["partial"] (:warnings (promql/parse (answer {:status "success" :warnings ["partial"]
                                                         :data {:resultType "vector" :result []}}))))))
  (testing "failures are maps with the reason"
    (is (= {:ok false :error "bad_data: parse error" :http-status 400}
           (promql/parse (answer {:status "error" :errorType "bad_data" :error "parse error"} :status 400))))
    (is (= {:ok false :error "HTTP 502" :http-status 502} (promql/parse (answer nil :status 502))))
    (is (= {:ok false :error "unreachable: refused"} (promql/parse {:reachable false :error "refused"})))
    (is (= {:ok false :error "the response is not a Prometheus API answer"} (promql/parse (answer {:hello "world"}))))
    (is (false? (:ok (promql/parse (answer {:status "success" :data {:resultType "histogram" :result []}})))))))

(deftest by-label-takes-the-larger-of-duplicates
  (is (= {"a" 2.0 "b" 1.0}
         (promql/by-label [{:labels {"node" "a"} :value 1.0}
                           {:labels {"node" "a"} :value 2.0}
                           {:labels {"node" "b"} :value 1.0}
                           {:labels {} :value 9.0}]
                          "node"))))

(deftest label-selectors-escape-values
  (is (= "cluster=\"a\\\"b\",job=\"node\"" (promql/label-selector {"job" "node" "cluster" "a\"b"}))))

(deftest a-query-reaches-the-server-with-flavour-and-headers
  (let [seen (atom nil)
        stop (server/run-server (fn [req]
                                  (reset! seen {:uri (:uri req) :query (:query-string req)
                                                :tenant (get-in req [:headers "x-scope-orgid"])})
                                  {:status 200 :headers {"Content-Type" "application/json"}
                                   :body (json/generate-string {:status "success"
                                                                :data {:resultType "vector"
                                                                       :result [{:metric {:node "n1"} :value [1 "8"]}]}})})
                                {:port 0 :legacy-return-value? false})
        port (server/server-port stop)]
    (try
      (let [ep (promql/endpoint {:url (str "http://127.0.0.1:" port) :flavor :mimir :tenant "t1"})
            result (promql/query! ep "count(up)")]
        (is (= {"n1" 8.0} (promql/by-label (:series result) "node")))
        (is (= "/prometheus/api/v1/query" (:uri @seen)))
        (is (= "t1" (:tenant @seen)))
        (is (= {"query" "count(up)"} (params (str "?" (:query @seen))))))
      (finally (server/server-stop! stop)))))
