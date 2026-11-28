;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mimirtool
  "mimirtool analyze wrappers: every analysis writes its JSON result into a
   working directory and returns the parsed data."
  (:require [babashka.fs :as fs]
            [briko.json :as briko-json]
            [gumshoe.json :as json]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(defn grafana-file [directory] (str (fs/path directory "metrics-in-grafana.json")))
(defn ruler-file [directory] (str (fs/path directory "metrics-in-ruler.json")))
(defn prometheus-file [directory] (str (fs/path directory "prometheus-metrics.json")))

(defn- read-json
  "The JSON file mimirtool wrote, with keyword keys, within
   gumshoe.json/command-output; nil when it is missing or holds no value."
  [file]
  (when (fs/exists? file)
    (try
      (briko-json/parse-file file (assoc json/command-output :keys :keyword))
      (catch clojure.lang.ExceptionInfo e
        (when-not (= :briko.json/no-value (:type (ex-data e)))
          (throw e))))))

(defn- analyze!
  "Runs one mimirtool analysis. Credentials travel via env, never via
   process arguments."
  [output env & args]
  (apply stdout/print-command "mimirtool" "analyze" args)
  (let [result (apply shell/execute-env env
                      "mimirtool" "analyze" (concat args [(str "--output=" output)]))]
    (if (zero? (:exit result))
      (read-json output)
      (do (stdout/error (format "mimirtool analyze failed:\n%s" (:err result)))
          nil))))

(defn rule-files
  "Every rule file (*.yaml, *.yml) in directory and its subdirectories,
   sorted."
  [directory]
  (vec (sort (map str (fs/glob directory "**.{yaml,yml}")))))

(defn analyze-rules!
  "Metrics used by the rule files in the given directory and its
   subdirectories."
  [directory output-dir]
  (let [files (rule-files directory)]
    (if (empty? files)
      (do (stdout/error (format "no rule files (*.yaml) found in %s" directory)) nil)
      (apply analyze! (ruler-file output-dir) {} "rule-file" files))))

(defn analyze-grafana!
  "Metrics used by the dashboards of a Grafana instance."
  [address token output-dir]
  (analyze! (grafana-file output-dir)
            {"GRAFANA_API_KEY" token}
            "grafana" (str "--address=" address)))

(defn analyze-prometheus!
  "Metrics in Prometheus that neither dashboards nor rules use. Requires the
   grafana and ruler analyses to exist in output-dir."
  [address username password output-dir]
  (analyze! (prometheus-file output-dir)
            {"MIMIR_API_USER" username
             "MIMIR_API_KEY" password}
            "prometheus"
            (str "--address=" address)
            (str "--grafana-metrics-file=" (grafana-file output-dir))
            (str "--ruler-metrics-file=" (ruler-file output-dir))))

(defn used-metrics
  "The metricsUsed list of a grafana or ruler analysis."
  [analysis]
  (vec (sort (distinct (:metricsUsed analysis)))))

(defn unused-metrics
  "The metrics a prometheus analysis found in the TSDB but nowhere else."
  [analysis]
  (vec (sort (distinct (keep :metric (:additional_metric_counts analysis))))))

(defn parse-errors
  "Every query a grafana or ruler analysis could not parse, as
   [{:source <the dashboard or rule group> :errors [<message> ..]}]. mimirtool
   leaves the metrics of such a query out of metricsUsed and still exits 0,
   so an analysis with any of these under-reports what is used."
  [analysis]
  (vec (concat
        (for [{:keys [title uid parse_errors]} (:dashboards analysis)
              :when (seq parse_errors)]
          {:source (format "dashboard %s (uid %s)" title uid) :errors (vec parse_errors)})
        (for [{:keys [name parse_errors]} (:ruleGroups analysis)
              :when (seq parse_errors)]
          {:source (format "rule group %s" name) :errors (vec parse_errors)}))))
