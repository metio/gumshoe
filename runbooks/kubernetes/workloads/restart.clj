;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.workloads.restart
  "Restarts the pods of a Deployment, StatefulSet or DaemonSet through a rollout."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.workloads :as workloads]))

(defn- fetch
  [context {:keys [type namespace name]}]
  (kubectl/get-namespaced-resource context namespace type name))

(defn- candidates
  [context]
  (workloads/picks (for [[type kind] workloads/types] [kind (kubectl/get-all context type)])
                   (fn [kind workload] (nil? (workloads/restart-refusal kind workload)))))

(mutation/book
 {:description "Restarts the pods of a Deployment, StatefulSet or DaemonSet through a rollout"
  :options {:pick {:desc "\"<Kind> <namespace>/<name>\" - interactive selection when omitted"
                   :alias :p :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get (mapv first workloads/types)
                  :kubectl-can-patch (mapv first workloads/types)}
  :select {:mode :one :label "Workload" :flag :pick :candidates candidates}
  :empty-message "no workload runs pods that a rollout could restart"
  :confirm {:action (str "restart every pod of the workload, one batch at a time as its update strategy allows - "
                         "a workload with a single replica is unavailable while its pod restarts")}
  :derive (fn [{:keys [target]}] {:pick (workloads/parse-pick target)})
  :precondition (fn [{:keys [context pick]}]
                  (let [why (if pick
                              (workloads/restart-refusal (:kind pick) (fetch context pick))
                              "the pick is not \"<Kind> <namespace>/<name>\"")]
                    (when why (println (str "refusing: " why)))
                    (nil? why)))
  :announce (fn [{:keys [target]}] (format "Restart %s" target))
  :effect (fn [{:keys [context pick]}]
            (effect/plan (apply effect/kubectl context (workloads/restart-args pick))))
  :verify (fn [{:keys [context pick target]}]
            [{:description (format "%s rolled out" target)
              :timeout 600 :interval 10
              :check (fn [] (workloads/rolled-out? (:kind pick) (fetch context pick)))}])})
