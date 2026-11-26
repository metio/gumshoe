;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.pods.list-resources
  "Lists the resource requests and limits of every container of every pod,
   init containers and native sidecars included, since both count towards
   what the scheduler reserves for the pod. A pod that sets pod-level
   resources (spec.resources) shows them on a row of role pod."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

(def ^:private columns
  [["Namespace" :namespace]
   ["Pod" :pod]
   ["Container" :container]
   ["Role" (comp name :role)]
   ["Memory-Request" #(or (:memory-request %) "<none>")]
   ["Memory-Limit" #(or (:memory-limit %) "<none>")]
   ["CPU-Request" #(or (:cpu-request %) "<none>")]
   ["CPU-Limit" #(or (:cpu-limit %) "<none>")]])

(def prerequisites
  {:installed-tools ["kubectl"]
   :cluster-capabilities []
   :kubectl-can-get ["pods"]})

(defn- list-resources
  [_opts _ctx]
  (let [{:keys [doc unread]} (kubectl/read-all (kubectl/current-context) "pods")]
    (if unread
      (do (stdout/error (str "could not read the pods of the cluster - " unread)) false)
      (let [rows (kubectl/container-resource-rows doc)]
        (stdout/print-section-marker)
        (if (seq rows)
          (println (stdout/table columns rows))
          (stdout/err-println "no pod runs in the cluster"))
        true))))

(runbook/execute!
 {:description "Lists the resource requests and limits of every pod"
  :prerequisites prerequisites
  :announce? false
  :action list-resources})
