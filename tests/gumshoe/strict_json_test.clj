;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.strict-json-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.http :as http]
            [gumshoe.json :as json]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.shell :as shell]
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

(defn- nested
  "A JSON object nesting depth levels of objects and arrays, as a CRD's
   openAPIV3Schema does, with a value at the bottom."
  [depth]
  (str (str/join (take (dec depth) (cycle ["{\"properties\":" "["])))
       "{\"type\":\"string\"}"
       (str/join (reverse (take (dec depth) (cycle ["}" "]"]))))))

(deftest nested-builds-the-depth-it-names
  (is (= {:type "string"} (json/parse-strict (nested 1))))
  (is (= [{:type "string"}] (:properties (json/parse-strict (nested 3)))))
  (is (thrown? Exception (json/parse-strict (nested 65))) "65 levels pass the default of 64 by one"))

(deftest a-key-named-twice-throws
  (is (thrown? Exception (json/parse-strict "{\"a\":1,\"a\":2}")))
  (is (nil? (json/parse-or-nil "{\"a\":1,\"a\":2}")))
  (is (nil? (kubectl/parse-json "{\"kind\":\"Pod\",\"kind\":\"Secret\"}"))
      "two readers keeping different duplicates would read different objects"))

(deftest a-raw-control-character-in-a-string-throws
  (is (thrown? Exception (json/parse-strict (str "{\"a\":\"x" (char 7) "y\"}"))))
  (is (= {:a (str "x" (char 7) "y")} (json/parse-strict "{\"a\":\"x\\u0007y\"}"))
      "the escaped form reads"))

(deftest limits-name-their-source
  (is (= shell/max-out-bytes (:max-bytes json/command-output))
      "a command's JSON can be no larger than the stdout gumshoe.shell keeps")
  (is (= shell/max-out-bytes (:max-bytes json/kubectl-output)))
  (is (= 128 (:max-depth json/kubectl-output))))

(deftest kubectl-json-nests-as-deep-as-a-crd-listing
  (testing "the deepest CRD listing measured needed 60 levels; kubectl's limit is 128"
    (is (some? (kubectl/parse-json (nested 60))))
    (is (some? (kubectl/parse-json (nested 128))))
    (is (nil? (kubectl/parse-json (nested 129))) "one level past the limit is no evidence"))
  (testing "negative control: under the command-output limit the same listing fails"
    (is (nil? (json/parse-or-nil (nested 65))))
    (is (some? (json/parse-or-nil (nested 65) json/kubectl-output)))))

(deftest kubectl-parse-json-keeps-its-contract
  (is (nil? (kubectl/parse-json nil)))
  (is (nil? (kubectl/parse-json "")))
  (is (nil? (kubectl/parse-json "null")))
  (is (nil? (kubectl/parse-json "{\"items\":[")))
  (is (= {:kind "List" :items [{:metadata {:name "a"}}]}
         (kubectl/parse-json "{\"kind\":\"List\",\"items\":[{\"metadata\":{\"name\":\"a\"}}]}"))))
