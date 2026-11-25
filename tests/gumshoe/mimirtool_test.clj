;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mimirtool-test
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.mimirtool :as mimirtool]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(deftest used-metrics-test
  (is (= ["node_cpu_seconds_total" "up"]
         (mimirtool/used-metrics {:metricsUsed ["up" "node_cpu_seconds_total" "up"]}))))

(deftest unused-metrics-test
  (is (= ["forgotten_metric" "orphaned_metric"]
         (mimirtool/unused-metrics {:additional_metric_counts
                                    [{:metric "orphaned_metric" :count 10}
                                     {:metric "forgotten_metric" :count 2}]}))))

;; The shapes below follow what mimirtool 3.2.6 wrote, offline, for a
;; dashboard with one query it could not parse and for a rule file.

(def ^:private grafana-analysis
  {:metricsUsed ["good_metric"]
   :dashboards [{:slug "" :uid "abc" :title "Shop" :metrics ["good_metric"]
                 :parse_errors ["query=rate(broken_metric[5m]) +: 1:26: parse error: unexpected end of input"]}
                {:slug "" :uid "def" :title "Clean" :metrics ["up"] :parse_errors []}]})

(def ^:private ruler-analysis
  {:metricsUsed ["up"]
   :ruleGroups [{:name "g1" :metrics ["up"] :parse_errors nil}]})

(deftest parse-errors-test
  (is (= [{:source "dashboard Shop (uid abc)"
           :errors ["query=rate(broken_metric[5m]) +: 1:26: parse error: unexpected end of input"]}]
         (mimirtool/parse-errors grafana-analysis)))
  (is (= [] (mimirtool/parse-errors ruler-analysis)))
  (is (= [{:source "rule group g1" :errors ["bad"]}]
         (mimirtool/parse-errors {:ruleGroups [{:name "g1" :parse_errors ["bad"]}]})))
  (is (= [] (mimirtool/parse-errors nil))))

(deftest rule-files-include-subdirectories
  (fs/with-temp-dir [dir {}]
    (fs/create-dirs (fs/path dir "sub" "deeper"))
    (spit (str (fs/path dir "top.yaml")) "groups: []")
    (spit (str (fs/path dir "sub" "nested.yml")) "groups: []")
    (spit (str (fs/path dir "sub" "deeper" "deep.yaml")) "groups: []")
    (spit (str (fs/path dir "notes.txt")) "")
    (is (= ["deep.yaml" "nested.yml" "top.yaml"]
           (sort (map #(str (fs/file-name %)) (mimirtool/rule-files (str dir))))))))

;; list_unused_metrics, run whole against a recorded mimirtool

(binding [runbook/*loading* true]
  (def ^:private unused-book (load-file "tools/prometheus/runbooks/list_unused_metrics.clj")))

(defn- run-unused
  "{:result :out :analyses <the analyses run>} of list_unused_metrics with
   mimirtool writing grafana for its grafana analysis."
  [grafana]
  (fs/with-temp-dir [rules {}]
    (spit (str (fs/path rules "a.yaml")) "groups: []")
    (let [ran (atom [])
          out (java.io.StringWriter.)
          answers {"rule-file" ruler-analysis
                   "grafana" grafana
                   "prometheus" {:additional_metric_counts [{:metric "broken_metric" :count 3}
                                                            {:metric "orphan_metric" :count 1}]}}]
      (with-redefs [shell/execute-env (fn [_env _tool _analyze kind & args]
                                        (swap! ran conj kind)
                                        (let [output (subs (last args) (count "--output="))]
                                          (spit output (json/generate-string (get answers kind)))
                                          {:exit 0 :out "" :err ""}))
                    stdout/print-command (fn [& _])]
        (let [result (binding [*out* out *err* out]
                       ((:action unused-book) {:directory (str rules) :grafana "http://grafana" :token "t"
                                               :prometheus "http://prometheus" :prometheus-username "u"
                                               :prometheus-password "p"}
                        {}))]
          {:result result :out (stdout/strip-colors (str out)) :analyses @ran})))))

(deftest unused-metrics-are-refused-when-a-dashboard-query-did-not-parse
  (let [{:keys [result out analyses]} (run-unused grafana-analysis)]
    (is (false? result))
    (is (str/includes? out "dashboard Shop (uid abc)"))
    (is (str/includes? out "broken_metric[5m]"))
    (is (not (str/includes? out "orphan_metric")) "no candidate for deletion is listed")
    (is (= ["rule-file" "grafana"] analyses) "the TSDB is not analysed against incomplete usage")))

(deftest unused-metrics-are-listed-when-every-query-parsed
  (let [{:keys [result out]} (run-unused (update grafana-analysis :dashboards #(mapv (fn [d] (assoc d :parse_errors [])) %)))]
    (is (true? result))
    (testing "the prometheus analysis is listed as it came back"
      (is (str/includes? out "broken_metric"))
      (is (str/includes? out "orphan_metric")))))
