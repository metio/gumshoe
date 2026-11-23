;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.report-test
  (:require [babashka.cli :as cli]
            [babashka.fs]
            [babashka.process]
            [cheshire.core]
            [clojure.edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gumshoe.detective :as detective]
            [gumshoe.report :as report]
            [gumshoe.summary]))

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

(deftest extra-data-joins-the-machine-formats-test
  (let [extra {:hosts [{:host "w1" :cores 8}] :findings "must not win"}
        json-out (cheshire.core/parse-string (with-out-str (detective/report! detectives findings "json" extra)) true)
        edn-out (clojure.edn/read-string (with-out-str (detective/report! detectives findings "edn" extra)))]
    (is (= [{:host "w1" :cores 8}] (:hosts json-out) (:hosts edn-out)))
    (is (= "boom" (-> json-out :findings first :summary)) "the report's own keys win over extra")
    (is (= findings (:findings edn-out))))
  (testing "the text report leaves extra out"
    (is (not (str/includes? (with-out-str (detective/report! detectives [] "text" {:hosts [{:host "w1-extra"}]})) "w1-extra")))))

(deftest a-plugin-format-from-env-edn-parses-in-a-real-run
  (let [dir (str (babashka.fs/create-temp-dir {:prefix "gumshoe-plugin-format"}))]
    (spit (str dir "/probe_fmt_plugin.clj")
          "(ns probe-fmt-plugin (:require [gumshoe.report :as report])) (report/register-format! \"probe-fmt\" (fn [_] nil))")
    (spit (str dir "/env.edn") "{:plugins [probe-fmt-plugin]}")
    (let [script (str "(require '[gumshoe.runbook :as runbook] '[gumshoe.detective :as detective])"
                      "(binding [*command-line-args* [\"--output\" \"probe-fmt\"]]"
                      "  (runbook/execute! {:description \"t\" :options detective/output-option :announce? false"
                      "                     :action (fn [opts _] (println \"ran with\" (:output opts)) true)}))")
          result @(babashka.process/process {:out :string :err :string :continue true
                                             :extra-env {"GUMSHOE_ENV" (str dir "/env.edn")}}
                                            "bb" "-cp" (str (System/getProperty "java.class.path") ":" dir) "-e" script)]
      (is (clojure.string/includes? (:out result) "ran with probe-fmt") (:err result)))))

(deftest every-report-captures-its-findings-for-the-banner
  (reset! @#'gumshoe.summary/captured [])
  (binding [*out* (java.io.StringWriter.)]
    (detective/report! detectives findings "json"))
  (is (gumshoe.summary/captured?) "a book rendering through report! gets the findings banner and the share offer"))
