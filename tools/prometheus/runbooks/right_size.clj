;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.prometheus.right-size
  "Compares every workload container's CPU and memory requests with its usage
   in Prometheus and recommends requests. Read-only: it prints the commands
   that would apply a recommendation, and runs none of them."
  (:require [gumshoe.config :as config]
            [gumshoe.http :as http]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.rightsizing :as rightsizing]
            [gumshoe.runbook :as runbook]
            [gumshoe.secrets :as secrets]
            [gumshoe.stdout :as stdout])
  (:import (java.net URLEncoder)
           (java.nio.charset StandardCharsets)
           (java.util Base64)))

(def options
  {:prometheus {:desc "The Prometheus base URL (e.g. https://prometheus.example.org); defaults to env.edn :prometheus :url"
                :alias :p
                :default (config/env-value {} [:prometheus :url])
                :coerce :string}
   :prometheus-username {:desc "The Prometheus basic auth username - resolved from the secret store when omitted"
                         :alias :u
                         :coerce :string}
   :prometheus-password {:desc "The Prometheus basic auth password - resolved from the secret store when omitted"
                         :alias :w
                         :coerce :string}
   :namespace {:desc "Only this namespace"
               :alias :n
               :coerce :string}
   :window {:desc "How far back usage is read, as a PromQL duration"
            :default "7d"
            :coerce :string}
   :all {:desc "Also list containers whose requests fit"
         :coerce :boolean}})

(def prerequisites
  {:installed-tools ["kubectl"]
   :cluster-capabilities []
   :kubectl-can-get ["pods"]})

(defn- credential
  [url field provided]
  (or provided
      (when-let [secret (secrets/find-path url)]
        (if (= :password field) (secrets/password secret) (secrets/field secret "login")))))

(defn- query
  [url headers promql]
  (let [response (http/fetch (str url "/api/v1/query?query=" (URLEncoder/encode ^String promql StandardCharsets/UTF_8)) headers)]
    (when-not (http/ok? response)
      (throw (ex-info (format "Prometheus did not answer the query: %s"
                              (or (:error response) (str "HTTP " (:status response) " " (get-in response [:json :error]))))
                      {})))
    (rightsizing/samples (:json response))))

(def ^:private verdict-labels
  {:missing (stdout/yellow "unset") :under (stdout/red "under") :over (stdout/yellow "over") :fits (stdout/green "fits")})

(defn- right-size
  [{:keys [prometheus namespace window all] :as opts} _ctx]
  (when-not prometheus
    (throw (ex-info "no Prometheus URL - pass --prometheus or set :prometheus :url in env.edn" {})))
  (let [username (credential prometheus :login (:prometheus-username opts))
        password (credential prometheus :password (:prometheus-password opts))
        headers (if (and username password)
                  {"Authorization" (str "Basic " (.encodeToString (Base64/getEncoder) (.getBytes (str username ":" password) StandardCharsets/UTF_8)))}
                  {})
        cpu (query prometheus headers (rightsizing/cpu-query window namespace))
        memory (query prometheus headers (rightsizing/memory-query window namespace))
        pods (if namespace
               (kubectl/get-namespaced (kubectl/current-context) namespace "pods")
               (kubectl/get-all (kubectl/current-context) "pods"))
        rows (map rightsizing/judge (rightsizing/usage-rows pods cpu memory))
        shown (if all rows (filter rightsizing/actionable? rows))]
    (if (empty? shown)
      (stdout/ok (if (seq rows)
                   (format "every request of %d containers fits its usage over %s" (count rows) window)
                   (format "Prometheus has no container usage over %s that matches a running pod" window)))
      (do
        (stdout/print-table
         [["WORKLOAD" #(str (:namespace %) "/" (:workload %))]
          ["CONTAINER" :container]
          ["PODS" :pods]
          ["CPU REQ" #(rightsizing/format-cpu (:cpu-request %))]
          ["P95" #(rightsizing/format-cpu (:cpu %))]
          ["→" #(rightsizing/format-cpu (:cpu-recommendation %))]
          ["" #(verdict-labels (:cpu-verdict %) "")]
          ["MEM REQ" #(rightsizing/format-memory (:memory-request %))]
          ["PEAK" #(rightsizing/format-memory (:memory %))]
          ["→ " #(rightsizing/format-memory (:memory-recommendation %))]
          [" " #(verdict-labels (:memory-verdict %) "")]]
         shown)
        (stdout/print-section-marker)
        (stdout/err-println (format "usage over %s: CPU at p95, memory at its peak, plus %d%% headroom" window
                                    (long (Math/round (* 100 (- rightsizing/headroom 1))))))
        (doseq [line (keep rightsizing/patch-line (filter rightsizing/actionable? shown))]
          (println line))))
    true))

(runbook/execute!
 {:description "Recommends container CPU and memory requests from Prometheus usage"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action right-size})
