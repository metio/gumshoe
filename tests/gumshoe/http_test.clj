;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.http-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.http :as http]
            [gumshoe.loopback-server :refer [recording! response serve!]])
  (:import [java.io ByteArrayInputStream PipedInputStream PipedOutputStream]
           [java.net URLDecoder]))

(deftest http-ok-test
  (is (http/ok? {:reachable true :status 200}))
  (is (http/ok? {:reachable true :status 204}))
  (is (not (http/ok? {:reachable true :status 503})))
  (is (not (http/ok? {:reachable false :error "connection refused"}))))

(defn- elapsed-ms [f]
  (let [started (System/nanoTime)
        result (f)]
    [result (quot (- (System/nanoTime) started) 1000000)]))

(deftest fetch-reads-a-whole-body
  (let [{:keys [url close]} (serve! "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 9\r\n\r\n"
                                    ["{\"a\":", "\"b\"}"] 0)]
    (try
      (let [response (http/fetch url)]
        (is (http/ok? response))
        (is (= {:a "b"} (:json response))))
      (finally (close)))))

(deftest fetch-gives-up-on-a-body-that-drips
  (testing "a server that answers the headers and then trickles the body is unreachable once the deadline passes"
    (let [{:keys [url close]} (serve! "HTTP/1.1 200 OK\r\nContent-Length: 100000\r\n\r\n"
                                      (repeat 1000 "x") 200)]
      (try
        (let [[response ms] (elapsed-ms #(http/fetch url {} {:deadline-ms 1500}))]
          (is (< ms 5000) "fetch returns close to its deadline")
          (is (false? (:reachable response)))
          (is (= 200 (:status response)))
          (is (str/includes? (:error response) "did not arrive"))
          (is (not (http/ok? response))))
        (finally (close))))))

(deftest fetch-gives-up-on-a-body-past-max-bytes
  (let [{:keys [url close]} (serve! "HTTP/1.1 200 OK\r\nContent-Length: 4096\r\n\r\n"
                                    [(apply str (repeat 4096 "x"))] 0)]
    (try
      (let [response (http/fetch url {} {:max-bytes 1024})]
        (is (false? (:reachable response)))
        (is (str/includes? (:error response) "exceeded 1024 bytes")))
      (finally (close)))))

(deftest fetch-lines-bounds-an-error-body-that-never-ends
  (testing "a 503 whose body keeps the connection open still returns within the deadline"
    (let [{:keys [url close]} (serve! "HTTP/1.1 503 Service Unavailable\r\nContent-Length: 100000\r\n\r\n"
                                      (repeat 1000 "x") 200)]
      (try
        (let [[response ms] (elapsed-ms #(http/fetch-lines! url {:deadline-ms 1500}))]
          (is (< ms 5000))
          (is (true? (:reachable response)))
          (is (= 503 (:status response)))
          (is (nil? (:body response)))
          (is (not (http/ok? response))))
        (finally (close))))))

(deftest fetch-lines-keeps-a-short-error-body
  (let [{:keys [url close]} (serve! "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 4\r\nConnection: close\r\n\r\n"
                                    ["oops"] 0)]
    (try
      (let [response (http/fetch-lines! url {:deadline-ms 3000})]
        (is (= 500 (:status response)))
        (is (= "oops" (:body response))))
      (finally (close)))))

(deftest fetch-lines-returns-what-a-stream-sent-by-the-deadline
  (let [{:keys [url close]} (serve! "HTTP/1.1 200 OK\r\nContent-Length: 100000\r\n\r\n"
                                    ["one\n" "two\n"] 0)]
    (try
      (let [[response ms] (elapsed-ms #(http/fetch-lines! url {:deadline-ms 1500}))]
        (is (< ms 5000))
        (is (= ["one" "two"] (:lines response))))
      (finally (close)))))

(deftest read-body-interrupts-a-blocked-read
  (let [blocking (PipedInputStream. (PipedOutputStream.))
        [result ms] (elapsed-ms #(http/read-body blocking {:deadline-ms 200 :max-bytes 10}))]
    (is (< ms 2000))
    (is (str/includes? (:error result) "did not arrive within 200 ms")))
  (is (= {:body "héllo"}
         (http/read-body (ByteArrayInputStream. (.getBytes "héllo" "UTF-8"))
                         {:deadline-ms 1000 :max-bytes 100}))))

(defn- query-of
  "The decoded query parameters of a request target."
  [target]
  (into {} (for [pair (str/split (or (second (str/split target #"\?" 2)) "") #"&")
                 :when (seq pair)
                 :let [[k v] (str/split pair #"=" 2)]]
             [(URLDecoder/decode k "UTF-8") (URLDecoder/decode (or v "") "UTF-8")])))

(deftest request-sends-accept-json-the-caller-headers-and-the-query
  (let [{:keys [url requests close]} (recording! (fn [_] (response "200 OK" "{\"a\":1}")))]
    (try
      (let [answer (http/request :get (str url "api/v1/series")
                                 {:headers {"X-Scope-OrgID" "tenant"}
                                  :query-params {"match[]" "up{job=\"a b\"}"}})
            {:keys [method target headers]} (first @requests)]
        (is (= {:reachable true :status 200 :body "{\"a\":1}" :json {:a 1}} answer))
        (is (= "GET" method))
        (is (str/starts-with? target "/api/v1/series?"))
        (is (= {"match[]" "up{job=\"a b\"}"} (query-of target)))
        (is (= "application/json" (get headers "accept")))
        (is (= "tenant" (get headers "x-scope-orgid"))))
      (finally (close)))))

(deftest request-posts-to-a-loopback-port
  (let [{:keys [url requests close]} (recording! (fn [_] (response "204 No Content" "")))]
    (try
      (let [answer (http/request :post (str url "api/v1/admin/tsdb/delete_series")
                                 {:query-params {"match[]" "metric_name"}})]
        (is (= 204 (:status answer)))
        (is (true? (:reachable answer)))
        (is (= "POST" (:method (first @requests))))
        (is (= {"match[]" "metric_name"} (query-of (:target (first @requests))))))
      (finally (close)))))

(deftest a-server-error-is-an-answer
  (let [{:keys [url close]} (recording! (fn [_] (response "503 Service Unavailable" "down")))]
    (try
      (let [answer (http/fetch url)]
        (is (= {:reachable true :status 503 :body "down" :json nil} answer))
        (is (not (http/ok? answer))))
      (finally (close)))))

(deftest a-redirect-on-the-same-origin-is-followed
  (let [{:keys [url requests close]} (recording! (fn [{:keys [target]}]
                                                   (if (= "/old" target)
                                                     (response "302 Found" {"Location" "/new"} "")
                                                     (response "200 OK" "[1]"))))]
    (try
      (let [answer (http/fetch (str url "old"))]
        (is (= {:reachable true :status 200 :body "[1]" :json [1]} answer))
        (is (= ["/old" "/new"] (map :target @requests))))
      (finally (close)))))

(deftest a-refused-connection-is-unreachable
  (let [{:keys [port close]} (recording! (fn [_] (response "200 OK" "")))]
    (close)
    (let [answer (http/fetch (str "http://127.0.0.1:" port "/"))]
      (is (false? (:reachable answer)))
      (is (string? (:error answer)))
      (is (not (http/ok? answer))))))

