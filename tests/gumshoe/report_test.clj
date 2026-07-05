;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.report-test
  (:require [babashka.cli :as cli]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gumshoe.detective :as detective]
            [gumshoe.report :as report]))

(use-fixtures :each (fn [t] (reset! @#'report/formats {}) (t) (reset! @#'report/formats {})))

(def ^:private detectives [{:name "d" :description "checks a thing"}])
(def ^:private findings [{:severity :critical :component "ns/x" :summary "boom"}])

(deftest a-plugin-format-is-selected-by-output-test
  (testing "--output <name> routes the report data through the registered renderer"
    (report/register-format! "count-only"
                             (fn [{:keys [findings summary]}]
                               (println "FINDINGS" (count findings) "CRIT" (:critical summary))))
    (let [out (with-out-str (detective/report! detectives findings "count-only"))]
      (is (str/includes? out "FINDINGS 1"))
      (is (str/includes? out "CRIT 1") "the summary is computed and passed to the format"))))

(deftest registered-lists-builtins-and-plugins-test
  (report/register-format! "sarif" (fn [_] nil))
  (is (= ["edn" "json" "sarif" "text"] (report/registered))
      "the three built-ins plus any registered, sorted"))

(deftest output-option-accepts-plugin-formats-test
  (testing "--output validates against the live registry, so a plugin format parses"
    (report/register-format! "sarif" (fn [_] nil))
    (is (= "sarif" (:output (cli/parse-opts ["--output" "sarif"] {:spec detective/output-option}))))
    (is (= "json" (:output (cli/parse-opts ["--output" "json"] {:spec detective/output-option})))))
  (testing "an unregistered format is still rejected"
    (is (thrown? Exception
                 (cli/parse-opts ["--output" "nope"] {:spec detective/output-option})))))

(deftest an-unknown-format-returns-health-not-crash-test
  (testing "report! still returns healthy? for a plugin format"
    (report/register-format! "noop" (fn [_] nil))
    (is (false? (detective/report! detectives findings "noop")) "a critical finding is unhealthy")
    (is (true? (detective/report! detectives [] "noop")) "no findings is healthy")))
