;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.prometheus.delete-metric
  "Deletes every series of a metric from every TSDB of one Prometheus: each
   replica and each shard keeps its own, so the book deletes on every pod of
   the chosen Prometheus behind the chosen service, through a port-forward
   to that pod, checks each one, and reports per pod. The run is verified
   only when every pod deleted the metric and its check held."
  (:require [clojure.string :as str]
            [gumshoe.http :as http]
            [gumshoe.interact :as interact]
            [gumshoe.outcome :as outcome]
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
                   :prometheus {:desc "The Prometheus resource whose pods the metric is deleted from - interactive selection among those in the service's namespace when omitted"
                                :coerce :string}
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
   :kubectl-can-get ["services" "pods" prometheus/prometheus-type]})

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

(defn- delete-on-pod!
  "Forwards a local port to pod and deletes metric there, then checks that
   the pod answers no series of it, recording each outcome with record!."
  [context namespace opts metric {:keys [pod port]} record!]
  (let [local-port (kubectl/local-port-or-free (:port opts))]
    (kubectl/with-port-forward {:context context
                                :namespace namespace
                                :pod pod
                                :local-port local-port
                                :remote-port port}
      (fn []
        (stdout/print-section (format "🗑  %s" pod))
        (when (delete-series! local-port metric (:delete-timeout opts))
          (record! :deleted? true)
          ;; best-effort: /api/v1/series can lag behind the delete until
          ;; tombstone cleanup, and the delete already returned success.
          ;; We watch the Prometheus namespace's events while confirming.
          (let [held (verify/eventually
                      {:description (format "no series of '%s' are queryable on %s anymore" metric pod)
                       :timeout 60 :soft? true
                       :watch (watch/namespace-warning-events context namespace)
                       :check (fn [] (let [gone (and (kubectl/port-forward-alive?)
                                                     (series-gone? local-port metric))]
                                       (when gone (record! :confirmed? true))
                                       gone))})]
            (record! :verified? held)
            held))))))

(defn report-lines
  "One line per pod of a deletion's results, saying what it did."
  [results]
  (for [{:keys [pod deleted? verified? confirmed?]} results]
    (format "  %s: %s" pod
            (cond
              (not deleted?) "NOT deleted - the metric is still there"
              confirmed? "deleted, no series left"
              verified? "deleted, NOT confirmed (best effort) - its series API still listed the metric"
              :else "deleted, but its check did not complete"))))

(defn failure-guidance
  "The recovery printed when not every pod deleted the metric."
  [metric results]
  (let [kept (map :pod (remove :deleted? results))]
    (str (format "Metric '%s' was deleted on %d of %d Prometheus pods." metric
                 (count (filter :deleted? results)) (count results))
         (when (seq kept)
           (format "\nPod(s) %s still hold it, and queries load-balanced across the replicas keep returning it." (str/join ", " kept)))
         "\nFix the cause named above and run the book again: deleting on a pod that has nothing left is harmless.")))

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
      (let [{:keys [namespace]} (kubectl/namespace-name-map target)
            service (kubectl/find-item @services target)
            prometheuses (kubectl/get-namespaced context namespace prometheus/prometheus-type)
            names (kubectl/names-of prometheuses)
            ;; the one Prometheus of a namespace needs no question
            chosen (when prometheuses
                     (if (and (nil? (:prometheus opts)) (= 1 (count names)))
                       (first names)
                       (interact/choose-one "Prometheus" names (:prometheus opts))))
            {:keys [targets refusal]} (when chosen
                                        (prometheus/delete-targets
                                         {:service service
                                          :port-name (:port-name opts)
                                          :prometheus (str namespace "/" chosen)
                                          :instance chosen
                                          :pods (kubectl/get-namespaced-selected context namespace "pods"
                                                                                 (prometheus/pods-selector chosen))}))]
        (cond
          (nil? (prometheus/api-port service (:port-name opts)))
          (do (stdout/error (format "service %s has no port named '%s' - check --port-name" target (:port-name opts))) false)

          (nil? prometheuses)
          (do (stdout/error (format "could not read the Prometheus resources of namespace %s" namespace)) false)

          (nil? chosen)
          (do (stdout/error (format "no Prometheus chosen in namespace %s" namespace)) false)

          refusal
          (do (stdout/error refusal) false)

          :else
          (flow/change!
           {:confirmation {:action (format "delete every series of metric '%s' from each of the %d pod(s) of Prometheus %s/%s - every replica and shard"
                                           metric (count targets) namespace chosen)
                           :target (format "%s/%s (%s)" namespace chosen cluster)
                           :items (mapv #(format "delete %s on pod %s" metric (:pod %)) targets)
                           :destructive? true}
            :announce! #(announce/announce! cluster announcement-data
                                            (format "Delete metric %s from Prometheus %s/%s" metric namespace chosen))
            :execute! #(let [{:keys [results ok?]} (prometheus/delete-on-every-pod!
                                                    targets
                                                    (fn [pod-target record!]
                                                      (delete-on-pod! context namespace opts metric pod-target record!)))]
                         (stdout/print-section "🔎 Every pod")
                         (doseq [line (report-lines results)] (stdout/err-println line))
                         (outcome/verified! ok?)
                         (when-not ok? (outcome/recovery! (failure-guidance metric results) {}))
                         ok?)}))))))

(runbook/execute!
 {:description "Deletes every series of a metric from every replica and shard of a Prometheus"
  :options options
  :prerequisites prerequisites
  :action delete-metric})
