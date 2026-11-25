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
            [gumshoe.detectives.disruption :as disruption]
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

(deftest a-book-option-reaches-the-detectives-as-evidence
  (let [seen (atom nil)]
    (with-redefs [detective/collect-evidence! (fn [_ _] {:now (java.time.Instant/now)})
                  detective/report-and-offer! (fn [_ _ _ _] true)]
      (detective/investigate! "ctx" [{:name "probe" :requires [] :detect (fn [evidence] (reset! seen evidence) [])}]
                              {:output "json" :hpa-grace-minutes 3}))
    (is (= {:hpa-grace-minutes 3} (get @seen "options")))))

(deftest the-hpa-grace-option-parses-and-defaults
  (let [spec (merge detective/output-option disruption/hpa-grace-option)]
    (is (= 10 (:hpa-grace-minutes (cli/parse-opts [] {:spec spec}))))
    (is (= 3 (:hpa-grace-minutes (cli/parse-opts ["--hpa-grace-minutes" "3"] {:spec spec}))))
    (is (thrown? Exception (cli/parse-opts ["--hpa-grace-minutes" "-1"] {:spec spec})))))

(defn- failing-writer
  "A PrintWriter over a stream that fails every write (an unconnected pipe),
   as stdout does on a full disk or a closed pipe."
  []
  (java.io.PrintWriter. (java.io.PipedOutputStream.)))

(deftest a-report-that-never-reached-stdout-is-not-a-pass-test
  (doseq [output ["json" "edn" "text"]]
    (testing output
      (let [err (java.io.StringWriter.)
            healthy (binding [*out* (failing-writer) *err* err]
                      (detective/report! detectives [] output))]
        (is (false? healthy) "a healthy scan whose report is missing never exits 0")
        (is (str/includes? (str err) "the report could not be written to stdout") (str err)))))
  (testing "a report that was written keeps its verdict"
    (let [out (java.io.StringWriter.)]
      (is (true? (binding [*out* (java.io.PrintWriter. out) *err* (java.io.StringWriter.)]
                   (detective/report! detectives [] "json"))))
      (is (str/includes? (str out) "\"findings\"")))))

(deftest every-java-time-value-is-written-test
  (testing "cheshire encodes instants and dates; every other java.time value is written as its string"
    (doseq [v [(java.time.LocalDate/of 2026 11 25)
               (java.time.LocalDateTime/of 2026 11 25 1 2 3)
               (java.time.ZonedDateTime/of 2026 11 25 1 2 3 0 (java.time.ZoneId/of "Europe/Berlin"))
               (java.time.OffsetDateTime/of 2026 11 25 1 2 3 0 java.time.ZoneOffset/UTC)
               (java.time.LocalTime/of 1 2 3)
               (java.time.Year/of 2026)]]
      (let [data (cheshire.core/parse-string (detective/json-report {:findings [{:since v}]}) true)]
        (is (= (str v) (:since (first (:findings data)))) (str (class v))))))
  (testing "an instant beside them keeps cheshire's own encoding"
    (let [data (cheshire.core/parse-string
                (detective/json-report {:at (java.time.Instant/parse "2026-11-25T00:00:00Z")
                                        :day (java.time.LocalDate/of 2026 11 25)})
                true)]
      (is (= "2026-11-25T00:00:00Z" (:at data)))
      (is (= "2026-11-25" (:day data)))))
  (testing "a book's :extra holding one does not cost the findings"
    (let [out (with-out-str
                (binding [*err* (java.io.StringWriter.)]
                  (detective/report! detectives findings "json" {:day (java.time.LocalDate/of 2026 11 25)})))]
      (is (= "boom" (:summary (first (:findings (cheshire.core/parse-string out true)))))))))

(defn- absolute-classpath []
  (->> (str/split (System/getProperty "java.class.path") #":")
       (map #(str (babashka.fs/absolutize %)))
       (str/join ":")))

(defn- scan-script
  "A scan as a book runs it: runbook/execute! around an action that reports
   one warning, so a written report exits 0."
  [output]
  (str "(require '[gumshoe.runbook :as runbook] '[gumshoe.detective :as d])"
       "(binding [*command-line-args* []]"
       "  (runbook/execute! {:description \"reports a warning\" :announce? false"
       "                     :action (fn [_ _] (d/report! [{:name \"d\" :description \"checks\"}]"
       "                                                  [{:severity :warning :component \"ns/x\" :summary \"meh\"}]"
       "                                                  " (pr-str output) "))}))"))

(defn- run-scan
  "Runs the scan in a fresh bb with its stdout sent to `out` (a babashka.process
   :out value) and returns the finished process."
  [output out]
  (babashka.fs/with-temp-dir [dir {}]
    (let [p (babashka.process/process
             {:dir (str dir) :out out :err :string :continue true
              :extra-env {"GUMSHOE_ENV" (str (babashka.fs/path dir "env.edn")) "NO_COLOR" "1"}}
             "bb" "-cp" (absolute-classpath) "-e" (scan-script output))]
      (when (= :pipe out)
        ;; the reader is gone before anything is written: a closed pipe
        (.close ^java.io.InputStream (:out p)))
      @p)))

(deftest a-real-stdout-that-loses-the-report-fails-the-scan-test
  (doseq [output ["json" "edn" "text"]]
    (testing (str output " to a full disk")
      (let [{:keys [exit err]} (run-scan output (java.io.File. "/dev/full"))]
        (is (not= 0 exit) err)
        (is (str/includes? err "the report could not be written to stdout") err)))
    (testing (str output " to a closed pipe")
      (let [{:keys [exit err]} (run-scan output :pipe)]
        (is (not= 0 exit) err)
        (is (str/includes? err "the report could not be written to stdout") err)))
    (testing (str output " to a working stdout keeps its verdict")
      (let [{:keys [exit err]} (run-scan output :string)]
        (is (= 0 exit) err)
        (is (not (str/includes? err "could not be written")) err)))))
