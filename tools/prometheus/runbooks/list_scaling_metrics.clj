;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.prometheus.list-scaling-metrics
  "Lists every metric used by HorizontalPodAutoscalers for scaling decisions."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(def prerequisites
  {:installed-tools ["kubectl"]
   :cluster-capabilities []
   :kubectl-can-get ["horizontalpodautoscalers"]})

(defn- list-scaling-metrics
  [_opts _ctx]
  (let [{:keys [doc unread]} (kubectl/read-all (kubectl/current-context) "horizontalpodautoscalers")]
    (if unread
      (do (stdout/error (str "could not read the horizontalpodautoscalers of the cluster - " unread)) false)
      (let [metrics (kubectl/hpa-scaling-metrics doc)]
        (if (empty? metrics)
          (stdout/ok "no HorizontalPodAutoscaler uses any metrics")
          (run! println metrics))
        true))))

(runbook/execute!
 {:description "Lists every metric used by HorizontalPodAutoscalers for scaling decisions"
  :prerequisites prerequisites
  :announce? false
  :action list-scaling-metrics})
