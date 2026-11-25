;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.workloads.restart
  "Restarts the pods of a Deployment, StatefulSet or DaemonSet through a rollout."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]
            [gumshoe.workloads :as workloads]))

(defn- candidates
  [context]
  (workloads/picks (for [[type kind] workloads/types] [kind (kubectl/get-all context type)])
                   (fn [kind workload] (nil? (workloads/restart-refusal kind workload)))))

(def selection
  {:mode :one :label "Workload" :flag :pick :candidates candidates})

(mutation/book
 {:description "Restarts the pods of a Deployment, StatefulSet or DaemonSet through a rollout"
  :options (select/options selection {:pick {:desc "\"<Kind> <namespace>/<name>\" - interactive selection when omitted"}})
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get (mapv first workloads/types)
                  :kubectl-can-patch (mapv first workloads/types)}
  :select selection
  :empty-message "no workload runs pods that a rollout could restart"
  :confirm {:action (str "restart every pod of the workload, one batch at a time as its update strategy allows - "
                         "a workload with a single replica is unavailable while its pod restarts")
            :destructive? false}
  :subject {:parse workloads/parse-pick :invalid "the pick is not \"<Kind> <namespace>/<name>\""}
  :refusal (fn [{:keys [ref object]}] (workloads/restart-refusal (:kind ref) object))
  :announce (fn [{:keys [target]}] (format "Restart %s" target))
  :effect (fn [{:keys [context ref]}]
            (if ref
              (effect/plan (apply effect/kubectl context (workloads/restart-args ref)))
              (effect/plan (effect/note "nothing to restart - the precondition says why"))))
  :verify (fn [{:keys [ref target] :as ctx}]
            [{:description (format "%s rolled out" target)
              :timeout 600 :interval 10
              :check (fn [] (workloads/rolled-out? (:kind ref) (mutation/read-subject ctx)))}])})
