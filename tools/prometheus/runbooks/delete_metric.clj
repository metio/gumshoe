;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.prometheus.delete-metric
  "Deletes every series of a metric from a Prometheus TSDB."
  (:require [gumshoe.http :as http]
            [gumshoe.watch :as watch]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.announce :as announce]
            [gumshoe.prometheus :as prometheus]
            [gumshoe.runbook :as runbook]
            [gumshoe.select :as select]
            [gumshoe.stdout :as stdout]
            [gumshoe.verify :as verify]))

(def service-select
  "The Prometheus service to ask. Its candidates and empty message depend on
   --selector, so the action adds them."
  {:mode :namespaced :label "Prometheus" :namespace-flag :namespace :name-flag :service
   :on-empty :fail})

(def options
  (select/options service-select
                  {:metric {:desc "The metric to delete"
                            :alias :m
                            :require true
                            :coerce :string}
                   :namespace {:desc "The namespace of the Prometheus service - interactive selection when omitted"}
                   :service {:desc "The name of the Prometheus service - interactive selection when omitted"}
                   :selector {:desc "Label selector used to find Prometheus services"
                              :alias :l
                              :default prometheus/governing-service-selector
                              :coerce :string}
                   :port-name {:desc "The service port serving the Prometheus API"
                               :default prometheus/default-port-name
                               :coerce :string}
                   :port kubectl/local-port-spec
                   :delete-timeout {:desc "Seconds the delete_series call may take - a large TSDB can need minutes"
                                    :default 600
                                    :coerce :long}}))

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["services"]})

(def series-probe-timeouts
  "How long one /api/v1/series probe of the post-check may take. The check
   runs inside verify/eventually, which judges its timeout only between
   attempts, so an attempt must end on its own."
  {:timeout-ms 10000 :deadline-ms 15000})

(defn delete-series!
  "Asks Prometheus on local-port to delete every series of metric, giving the
   call timeout-seconds before it counts as failed."
  [local-port metric timeout-seconds]
  (let [ms (* 1000 timeout-seconds)
        response (http/request :post (format "http://127.0.0.1:%s/api/v1/admin/tsdb/delete_series" local-port)
                               {:query-params {"match[]" metric}
                                :timeout-ms ms
                                :deadline-ms (+ ms 10000)})]
    (cond
      (not (:reachable response))
      (do (stdout/error "could not reach Prometheus through the port-forward:" (:error response)) false)

      (contains? #{200 204} (:status response))
      (do (stdout/ok (format "metric %s deleted" metric)) true)

      :else
      (do (stdout/error "metric not deleted, status:" (:status response) (:body response)) false))))

(defn series-gone?
  "True only when Prometheus on local-port answered that no series of metric
   remains."
  [local-port metric]
  (let [response (http/request :get (format "http://127.0.0.1:%s/api/v1/series" local-port)
                               (assoc series-probe-timeouts :query-params {"match[]" metric}))]
    (boolean (and (http/ok? response)
                  (= "success" (get-in response [:json :status]))
                  (empty? (get-in response [:json :data]))))))

(defn- delete-metric
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        services (delay (kubectl/get-selected context "services" (:selector opts)))
        target (select/pick (assoc service-select
                                   :candidates (fn [_] (kubectl/namespaces-names @services))
                                   :empty-message (format "no service matches %s - check --selector" (:selector opts)))
                            opts context)
        metric (:metric opts)]
    (if (nil? target)
      false
      (let [{:keys [namespace name]} (kubectl/namespace-name-map target)
            remote-port (prometheus/api-port (kubectl/find-item @services target) (:port-name opts))
            local-port (delay (kubectl/local-port-or-free (:port opts)))]
        (cond
          (nil? remote-port)
          (do (stdout/error (format "service %s has no port named '%s' - check --port-name" target (:port-name opts))) false)

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
                                                   :local-port @local-port
                                                   :remote-port remote-port}
                         (fn []
                           (and (delete-series! @local-port metric (:delete-timeout opts))
                                (do (stdout/print-section "🔎 Post-check")
                                    ;; best-effort: /api/v1/series can lag behind the delete until
                                    ;; tombstone cleanup, and the delete already returned success.
                                    ;; We watch the Prometheus namespace's events while confirming.
                                    (verify/all
                                     [{:description (format "no series of '%s' are queryable anymore" metric)
                                       :timeout 60 :soft? true
                                       :watch (watch/namespace-warning-events context namespace)
                                       :check (fn [] (and (kubectl/port-forward-alive?)
                                                          (series-gone? @local-port metric)))}])))))}))))))

(runbook/execute!
 {:description "Deletes every series of a metric from a Prometheus TSDB"
  :options options
  :prerequisites prerequisites
  :action delete-metric})
