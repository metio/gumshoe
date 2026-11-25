;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.workloads.rollback
  "Rolls a Deployment, StatefulSet or DaemonSet back to its previous revision,
   or to a named one."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]
            [gumshoe.workloads :as workloads]))

(defn- history
  [context {:keys [kind namespace]}]
  (kubectl/get-namespaced context namespace (workloads/history-types kind)))

(defn- candidates
  [context]
  (let [histories (into {} (for [type (distinct (vals workloads/history-types))] [type (kubectl/get-all context type)]))]
    (workloads/picks (for [[type kind] workloads/types] [kind (kubectl/get-all context type)])
                     (fn [kind workload]
                       (nil? (workloads/rollback-refusal kind workload (histories (workloads/history-types kind)) nil))))))

(def selection
  {:mode :one :label "Workload" :flag :pick :candidates candidates})

(mutation/book
 {:description "Rolls a Deployment, StatefulSet or DaemonSet back to its previous revision, or to a named one"
  :options (select/options selection {:pick {:desc "\"<Kind> <namespace>/<name>\" - interactive selection when omitted"}
                                      :to-revision {:desc "The revision to go back to - the previous one when omitted" :coerce :long}})
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get (into (mapv first workloads/types) ["replicasets" "controllerrevisions"])
                  :kubectl-can-patch (mapv first workloads/types)}
  :select selection
  :empty-message "no workload has an earlier revision to go back to"
  :confirm {:action "roll a workload back" :destructive? true}
  :subject {:parse workloads/parse-pick :invalid "the pick is not \"<Kind> <namespace>/<name>\""}
  :derive (fn [{:keys [context ref object opts]}]
            (let [past (when ref (history context ref))
                  to (:to-revision opts)]
              {:history past :to to
               :target-revision (when object (workloads/rollback-target (:kind ref) object past to))}))
  :items (fn [{:keys [ref object history to target-revision target]}]
           (if target-revision
             (workloads/rollback-items (:kind ref) object history to)
             [(format "roll %s back" target)]))
  :refusal (fn [{:keys [context ref object to]}]
             (workloads/rollback-refusal (:kind ref) object (history context ref) to))
  :announce (fn [{:keys [target target-revision]}] (format "Roll %s back to revision %s" target (:revision target-revision)))
  :effect (fn [{:keys [context ref target-revision]}]
            (if target-revision
              (effect/plan (apply effect/kubectl context (workloads/rollback-args ref (:revision target-revision))))
              (effect/plan (effect/note "nothing to roll back to - the precondition says why"))))
  :verify (fn [{:keys [ref target target-revision] :as ctx}]
            [{:description (format "%s runs revision %s's images and rolled out" target (:revision target-revision))
              :timeout 600 :interval 10
              :check (fn [] (workloads/rolled-back? (:kind ref) (mutation/read-subject ctx) target-revision))}])})
