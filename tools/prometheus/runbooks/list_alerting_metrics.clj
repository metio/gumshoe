;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.prometheus.list-alerting-metrics
  "Lists every metric used by the Prometheus alerting rule files in a
   directory."
  (:require [babashka.fs :as fs]
            [gumshoe.config :as config]
            [gumshoe.mimirtool :as mimirtool]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(defn rules-directory
  "The --directory flag, or the directory configured in env.edn."
  [provided]
  (or provided
      (config/env-value {} [:prometheus :rules-directory])))

(def options
  {:directory {:desc "Directory with Prometheus rule files; defaults to env.edn :prometheus :rules-directory"
               :alias :d
               :coerce :string}})

(def prerequisites
  {:installed-tools ["mimirtool"]})

(defn- list-alerting-metrics
  [opts _ctx]
  (let [directory (rules-directory (:directory opts))]
    (cond
      (nil? directory)
      (do (stdout/error "use --directory or set env.edn :prometheus :rules-directory") false)

      (not (fs/directory? directory))
      (do (stdout/error (format "%s is not a directory" directory)) false)

      :else
      (fs/with-temp-dir [workdir {}]
        (if-let [analysis (mimirtool/analyze-rules! directory (str workdir))]
          (do (run! println (mimirtool/used-metrics analysis))
              true)
          false)))))

(runbook/execute!
 {:description "Lists every metric used by the Prometheus alerting rule files in a directory"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action list-alerting-metrics})
