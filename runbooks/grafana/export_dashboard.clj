;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.grafana.export-dashboard
  "Exports a single dashboard as JSON via Grafana's app-platform API. The
   dashboard model goes to stdout, so it pipes and redirects cleanly."
  (:require [cheshire.core :as json]
            [infra.config :as config]
            [infra.secrets :as secrets]
            [infra.grafana :as grafana]
            [infra.interact :as interact]
            [infra.runbook :as runbook]
            [infra.shell :as shell]
            [infra.stdout :as stdout]))

(def options
  {:folder {:desc "The folder containing the dashboard - interactive selection when omitted"
            :alias :f
            :coerce :string}
   :dashboard {:desc "The dashboard to export - interactive selection when omitted"
               :alias :d
               :coerce :string}
   :grafana {:desc "The Grafana base URL (e.g. https://grafana.example.org); defaults to env.edn :grafana :url"
             :alias :g
             :default (config/env-value {} [:grafana :url])
             :coerce :string}
   :namespace {:desc "The Grafana org namespace ('default' is org 1)"
               :alias :n
               :default "default"
               :coerce :string}
   :token {:desc "The Grafana service account token - resolved via gopass when omitted"
           :alias :t
           :coerce :string}})

(def prerequisites
  {:installed-tools ["fzf"]})

(defn- resolve-token
  [base-url provided]
  (or provided
      (when-let [secret (secrets/find-path base-url)]
        (secrets/password secret))))

(defn- export-dashboard
  [opts _ctx]
  (let [base-url (:grafana opts)
        namespace (:namespace opts)
        token (resolve-token base-url (:token opts))]
    (cond
      (empty? token)
      (do (stdout/error "no Grafana token given and none found in gopass, use --token") false)

      (not (grafana/logged-in? base-url token namespace))
      (do (stdout/error (format "cannot list folders on %s - is the token valid?" base-url)) false)

      :else
      (let [all-folders (grafana/folders base-url token namespace)
            folder-title (interact/choose-one "Folder" (grafana/titles all-folders) (:folder opts))
            folder (grafana/find-by-title all-folders folder-title)]
        (if (nil? folder)
          (do (stdout/error "no folder selected") false)
          (let [candidates (grafana/in-folder (grafana/dashboards base-url token namespace)
                                              (grafana/uid-of folder))
                dashboard-title (interact/choose-one "Dashboard" (grafana/titles candidates) (:dashboard opts))
                dashboard (grafana/find-by-title candidates dashboard-title)]
            (if (nil? dashboard)
              (do (stdout/error "no dashboard selected") false)
              (let [resource (grafana/dashboard base-url token namespace (grafana/uid-of dashboard))]
                (if (nil? resource)
                  (do (stdout/error (format "could not fetch dashboard %s" (grafana/uid-of dashboard))) false)
                  (do (println (json/generate-string (:spec resource) {:pretty true}))
                      (stdout/ok (format "dashboard '%s' exported" dashboard-title))
                      true))))))))))

(runbook/execute!
 {:description "Exports a single dashboard as JSON via Grafana's app-platform API"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action export-dashboard})
