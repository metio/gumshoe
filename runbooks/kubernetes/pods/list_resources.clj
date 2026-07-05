;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.pods.list-resources
  "Lists the resource requests and limits of every pod."
  (:require [infra.kubectl :as kubectl]
            [infra.runbook :as runbook]
            [infra.shell :as shell]
            [infra.stdout :as stdout]))

(def ^:private columns
  (str "Namespace:metadata.namespace"
       ",Name:metadata.name"
       ",Memory-Request:spec.containers[*].resources.requests.memory"
       ",Memory-Limit:spec.containers[*].resources.limits.memory"
       ",CPU-Request:spec.containers[*].resources.requests.cpu"
       ",CPU-Limit:spec.containers[*].resources.limits.cpu"))

(def prerequisites
  {:installed-tools ["kubectl"]
   :cluster-capabilities []
   :kubectl-can-get ["pods"]})

(defn- list-resources
  [_opts _ctx]
  (let [context (kubectl/current-context)
        args ["kubectl" (str "--context=" context) "get" "pods" "--all-namespaces"
              (str "--output=custom-columns=" columns)]]
    (apply stdout/print-command args)
    (stdout/print-section-marker)
    (zero? (apply shell/run-with-output args))))

(runbook/execute!
 {:description "Lists the resource requests and limits of every pod"
  :prerequisites prerequisites
  :announce? false
  :action list-resources})
