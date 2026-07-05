;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.prometheus.list-scaling-metrics
  "Lists every metric used by HorizontalPodAutoscalers for scaling decisions."
  (:require [infra.kubectl :as kubectl]
            [infra.runbook :as runbook]
            [infra.stdout :as stdout]))

(def prerequisites
  {:installed-tools ["kubectl"]
   :cluster-capabilities []
   :kubectl-can-get ["horizontalpodautoscalers"]})

(defn- list-scaling-metrics
  [_opts _ctx]
  (let [metrics (kubectl/hpa-scaling-metrics (kubectl/get-all (kubectl/current-context) "horizontalpodautoscalers"))]
    (if (empty? metrics)
      (stdout/ok "no HorizontalPodAutoscaler uses any metrics")
      (run! println metrics))
    true))

(runbook/execute!
 {:description "Lists every metric used by HorizontalPodAutoscalers for scaling decisions"
  :prerequisites prerequisites
  :announce? false
  :action list-scaling-metrics})
