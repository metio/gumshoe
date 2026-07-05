;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.prometheus.delete-metric
  "Deletes every series of a metric from a Prometheus TSDB."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [infra.watch :as watch]
            [infra.flow :as flow]
            [infra.interact :as interact]
            [infra.kubectl :as kubectl]
            [infra.announce :as announce]
            [infra.runbook :as runbook]
            [infra.stdout :as stdout]
            [infra.verify :as verify]))

(def options
  {:metric {:desc "The metric to delete"
            :alias :m
            :require true
            :coerce :string}
   :namespace {:desc "The namespace of the Prometheus service - interactive selection when omitted"
               :alias :n
               :coerce :string}
   :service {:desc "The name of the Prometheus service - interactive selection when omitted"
             :alias :s
             :coerce :string}
   :port {:desc "The local port used for the port-forward"
          :alias :p
          :default 46372
          :coerce :long}})

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["services"]})

(defn- delete-series!
  [local-port metric]
  (try
    (let [response (http/post (format "http://localhost:%s/api/v1/admin/tsdb/delete_series" local-port)
                              {:query-params {"match[]" metric}
                               :throw false})]
      (if (contains? #{200 204} (:status response))
        (do (stdout/ok (format "metric %s deleted" metric)) true)
        (do (stdout/error "metric not deleted, status:" (:status response) (:body response)) false)))
    (catch Exception e
      (stdout/error "could not reach Prometheus through the port-forward:" (ex-message e))
      false)))

(defn- series-gone?
  [local-port metric]
  (try
    (let [response (http/get (format "http://localhost:%s/api/v1/series" local-port)
                             {:query-params {"match[]" metric}
                              :throw false})]
      (and (= 200 (:status response))
           (empty? (:data (json/parse-string (:body response) true)))))
    (catch Exception _ false)))

(defn- delete-metric
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        services (kubectl/get-selected context "services" "app=prometheus")
        target (interact/choose-namespaced "Prometheus" (kubectl/namespaces-names services)
                                           (:namespace opts) (:service opts))
        metric (:metric opts)]
    (if (nil? target)
      (do (stdout/error "no Prometheus service selected") false)
      (let [{:keys [namespace name]} (kubectl/split-namespace-name target)
            remote-port (kubectl/service-port (kubectl/find-item services target) "http")]
        (cond
          (nil? remote-port)
          (do (stdout/error (format "service %s has no port named 'http'" target)) false)

          :else
          (flow/change!
           {:confirmation {:action (format "delete every series of metric '%s' from the TSDB" metric)
                           :target (format "%s (%s)" target cluster)
                           :items [metric]
                           :destructive? true}
            :announce! #(announce/announce! cluster announcement-data
                                                         (format "Delete metric %s from %s" metric target))
            :execute! #(kubectl/with-port-forward {:context context
                                                   :namespace namespace
                                                   :service name
                                                   :local-port (:port opts)
                                                   :remote-port remote-port}
                         (fn []
                           (and (delete-series! (:port opts) metric)
                                (do (stdout/print-section "🔎 Post-check")
                                    ;; best-effort: /api/v1/series can lag behind the delete until
                                    ;; tombstone cleanup, and the delete already returned success.
                                    ;; We watch the Prometheus namespace's events while confirming.
                                    (verify/all
                                     [{:description (format "no series of '%s' are queryable anymore" metric)
                                       :timeout 60 :soft? true
                                       :watch (watch/namespace-warning-events context namespace)
                                       :check (fn [] (series-gone? (:port opts) metric))}])))))}))))))

(runbook/execute!
 {:description "Deletes every series of a metric from a Prometheus TSDB"
  :options options
  :prerequisites prerequisites
  :action delete-metric})
