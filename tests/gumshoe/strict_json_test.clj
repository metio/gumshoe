;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.strict-json-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.http :as http]
            [gumshoe.json :as json]
            [gumshoe.loopback-server :refer [recording! response]]))

(deftest whole-documents-parse
  (is (= [{:a 1} {:b [2 3]}] (json/parse-strict "[{\"a\":1},{\"b\":[2,3]}]")))
  (is (vector? (json/parse-strict "[1,2]")) "a top-level array arrives whole, never as a lazy seq")
  (is (= {:a 1} (json/parse-strict "  {\"a\":1}\n")))
  (is (nil? (json/parse-strict "")))
  (is (nil? (json/parse-strict nil)))
  (is (nil? (json/parse-strict "null"))))

(deftest a-truncated-array-throws-inside-the-call
  (testing "the failure surfaces in parse-strict, not later where the value is used"
    (is (thrown? Exception (json/parse-strict "[{\"time\":\"2026-01-01T00:00:00Z\"}, {\"ti")))
    (is (thrown? Exception (json/parse-strict "[1,2")))
    (is (thrown? Exception (json/parse-strict "{\"a\":[1,{\"b\":2}")))))

(deftest anything-after-the-value-throws
  (is (thrown? Exception (json/parse-strict "[1] trailing")))
  (is (thrown? Exception (json/parse-strict "[1] null")))
  (is (thrown? Exception (json/parse-strict "{\"a\":1}{\"b\":2}")))
  (is (thrown? Exception (json/parse-strict "repository abc opened (version 2)\n[]"))))

(deftest parse-or-nil-never-throws
  (is (nil? (json/parse-or-nil "[1,2")))
  (is (nil? (json/parse-or-nil "[1] x")))
  (is (= [1 2] (json/parse-or-nil "[1,2]"))))

(defn- answered
  [body]
  (let [{:keys [url close]} (recording! (fn [_] (response "200 OK" body)))]
    (try
      (http/request :get (str url "api") {})
      (finally (close)))))

(deftest http-json-is-nil-unless-the-whole-body-parses
  (is (= [{:a 1}] (:json (answered "[{\"a\":1}]"))))
  (let [truncated (answered "[{\"a\":1},{\"a\"")]
    (is (true? (:reachable truncated)))
    (is (= "[{\"a\":1},{\"a\"" (:body truncated)) "the body is kept as it came")
    (is (nil? (:json truncated)) "a truncated array is no JSON, not a lazy seq that throws later"))
  (is (nil? (:json (answered "{\"a\":1} trailing")))))

