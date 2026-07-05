;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.prometheus.list-unused-metrics
  "Lists every metric in a Prometheus TSDB that neither Grafana dashboards
   nor alerting rules use - candidates for deletion."
  (:require [babashka.fs :as fs]
            [infra.config :as config]
            [infra.secrets :as secrets]
            [infra.mimirtool :as mimirtool]
            [infra.runbook :as runbook]
            [infra.shell :as shell]
            [infra.stdout :as stdout]))

(def options
  {:directory {:desc "Directory with Prometheus rule files; defaults to env.edn :prometheus :rules-directory"
               :alias :d
               :coerce :string}
   :grafana {:desc "The Grafana base URL (e.g. https://grafana.example.org); defaults to env.edn :grafana :url"
             :alias :g
             :default (config/env-value {} [:grafana :url])
             :coerce :string}
   :token {:desc "The Grafana service account token - resolved via gopass when omitted"
           :alias :t
           :coerce :string}
   :prometheus {:desc "The Prometheus base URL (e.g. https://prometheus.example.org); defaults to env.edn :prometheus :url"
                :alias :p
                :default (config/env-value {} [:prometheus :url])
                :coerce :string}
   :prometheus-username {:desc "The Prometheus basic auth username - resolved via gopass when omitted"
                         :alias :u
                         :coerce :string}
   :prometheus-password {:desc "The Prometheus basic auth password - resolved via gopass when omitted"
                         :alias :w
                         :coerce :string}})

(def prerequisites
  {:installed-tools ["mimirtool"]})

(defn- gopass-credential
  [host field provided]
  (or provided
      (when-let [secret (secrets/find-path host)]
        (if (= :password field)
          (secrets/password secret)
          (secrets/field secret "login")))))

(defn- rules-directory
  [provided]
  (or provided
      (config/env-value {} [:prometheus :rules-directory])))

(defn- list-unused-metrics
  [opts _ctx]
  (let [directory (rules-directory (:directory opts))
        grafana-token (gopass-credential (:grafana opts) :password (:token opts))
        prometheus-user (gopass-credential (:prometheus opts) :login (:prometheus-username opts))
        prometheus-pass (gopass-credential (:prometheus opts) :password (:prometheus-password opts))]
    (cond
      (nil? directory)
      (do (stdout/error "use --directory or set env.edn :prometheus :rules-directory") false)

      (not (fs/directory? directory))
      (do (stdout/error (format "%s is not a directory" directory)) false)

      (empty? grafana-token)
      (do (stdout/error "no Grafana token given and none found in gopass, use --token") false)

      (or (empty? prometheus-user) (empty? prometheus-pass))
      (do (stdout/error "no Prometheus credentials given and none found in gopass, use --prometheus-username/--prometheus-password") false)

      :else
      (fs/with-temp-dir [workdir {}]
        (let [workdir (str workdir)]
          (if (and (mimirtool/analyze-rules! directory workdir)
                   (mimirtool/analyze-grafana! (:grafana opts) grafana-token workdir))
            (if-let [analysis (mimirtool/analyze-prometheus! (:prometheus opts)
                                                             prometheus-user prometheus-pass
                                                             workdir)]
              (do (run! println (mimirtool/unused-metrics analysis))
                  true)
              false)
            false))))))

(runbook/execute!
 {:description "Lists every metric in a Prometheus TSDB that neither dashboards nor rules use"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action list-unused-metrics})
