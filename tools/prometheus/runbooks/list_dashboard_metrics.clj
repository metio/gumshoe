;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.prometheus.list-dashboard-metrics
  "Lists every metric used by the dashboards of a Grafana instance."
  (:require [babashka.fs :as fs]
            [gumshoe.config :as config]
            [gumshoe.secrets :as secrets]
            [gumshoe.mimirtool :as mimirtool]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(def options
  {:grafana {:desc "The Grafana base URL (e.g. https://grafana.example.org); defaults to env.edn :grafana :url"
             :alias :g
             :default (config/env-value {} [:grafana :url])
             :coerce :string}
   :token {:desc "The Grafana service account token - resolved via gopass when omitted"
           :alias :t
           :coerce :string}})

(def prerequisites
  {:installed-tools ["mimirtool"]})

(defn- resolve-token
  [base-url provided]
  (or provided
      (when-let [secret (secrets/find-path base-url)]
        (secrets/password secret))))

(defn- list-dashboard-metrics
  [opts _ctx]
  (let [base-url (:grafana opts)
        token (resolve-token base-url (:token opts))]
    (if (empty? token)
      (do (stdout/error "no Grafana token given and none found in gopass, use --token") false)
      (fs/with-temp-dir [workdir {}]
        (if-let [analysis (mimirtool/analyze-grafana! base-url token (str workdir))]
          (do (run! println (mimirtool/used-metrics analysis))
              true)
          false)))))

(runbook/execute!
 {:description "Lists every metric used by the dashboards of a Grafana instance"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action list-dashboard-metrics})
