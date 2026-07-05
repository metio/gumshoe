;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mimirtool
  "mimirtool analyze wrappers: every analysis writes its JSON result into a
   working directory and returns the parsed data."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(defn grafana-file [directory] (str (fs/path directory "metrics-in-grafana.json")))
(defn ruler-file [directory] (str (fs/path directory "metrics-in-ruler.json")))
(defn prometheus-file [directory] (str (fs/path directory "prometheus-metrics.json")))

(defn- read-json
  [file]
  (when (fs/exists? file)
    (json/parse-string (slurp file) true)))

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

(defn analyze-rules!
  "Metrics used by the rule files in the given directory."
  [directory output-dir]
  (let [files (mapv str (fs/glob directory "*.{yaml,yml}"))]
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
