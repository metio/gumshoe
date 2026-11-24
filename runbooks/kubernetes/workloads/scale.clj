;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.workloads.scale
  "Scales a Deployment or StatefulSet to a replica count."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.workloads :as workloads]))

(defn- hpas
  [context {:keys [namespace]}]
  (kubectl/get-namespaced-result context namespace "horizontalpodautoscalers"))

(defn- candidates
  [context]
  (workloads/picks (for [[type kind] workloads/types :when (workloads/scalable-kinds kind)]
                     [kind (kubectl/get-all context type)])
                   (constantly true)))

(mutation/book
 {:description "Scales a Deployment or StatefulSet to a replica count"
  :options {:pick {:desc "\"<Kind> <namespace>/<name>\" - interactive selection when omitted"
                   :alias :p :coerce :string}
            :replicas {:desc "The replica count to scale to" :alias :r :coerce :long}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["deployments" "statefulsets" "horizontalpodautoscalers"]
                  :kubectl-can-patch ["deployments" "statefulsets"]}
  :select {:mode :one :label "Workload" :flag :pick :candidates candidates}
  :empty-message "no Deployment or StatefulSet to scale"
  :confirm {:action "scale a workload"}
  :subject {:parse workloads/parse-pick
            :invalid "the pick is not \"<Kind> <namespace>/<name>\" of a Deployment or StatefulSet"}
  :derive (fn [{:keys [opts]}] {:replicas (:replicas opts)})
  :items (fn [{:keys [ref object replicas]}]
           (if (and object (integer? replicas))
             (workloads/scale-items (:kind ref) object replicas)
             ["scale a workload"]))
  :refusal (fn [{:keys [context ref object replicas]}]
             (workloads/scale-refusal-given (:kind ref) object (hpas context ref) replicas))
  :announce (fn [{:keys [target replicas]}] (format "Scale %s to %s replica(s)" target replicas))
  :effect (fn [{:keys [context ref replicas]}]
            (effect/plan (apply effect/kubectl context (workloads/scale-args ref replicas))))
  :verify (fn [{:keys [target replicas] :as ctx}]
            [{:description (format "%s runs %s ready replica(s)" target replicas)
              :timeout 600 :interval 10
              :check (fn [] (workloads/scaled? (mutation/read-subject ctx) replicas))}])})
