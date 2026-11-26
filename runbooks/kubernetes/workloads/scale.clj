;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.workloads.scale
  "Scales a Deployment or StatefulSet to a replica count."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]
            [gumshoe.workloads :as workloads]))

(defn- hpas
  [context {:keys [namespace]}]
  (kubectl/get-namespaced-result context namespace "horizontalpodautoscalers"))

(def selection
  {:mode :one :label "Workload" :flag :pick :candidates workloads/scalable-picks})

(mutation/book
 {:description "Scales a Deployment or StatefulSet to a replica count"
  :options (select/options selection {:pick {:desc "\"<Kind> <namespace>/<name>\" - interactive selection when omitted"}
                                      :replicas {:desc "The replica count to scale to" :alias :r :coerce :long}})
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["deployments" "statefulsets" "horizontalpodautoscalers"]
                  ;; kubectl scale writes through the scale subresource, which
                  ;; RBAC authorises apart from the workload itself
                  :kubectl-can-patch ["deployments/scale" "statefulsets/scale"]}
  :select selection
  :empty-message "no Deployment or StatefulSet to scale"
  :confirm {:action "scale a workload" :destructive? false}
  :subject {:parse workloads/parse-pick
            :invalid "the pick is not \"<Kind> <namespace>/<name>\" of a Deployment or StatefulSet"}
  :derive (fn [{:keys [opts]}] {:replicas (:replicas opts)})
  :items (fn [{:keys [ref object replicas]}]
           (if (and object (integer? replicas))
             (workloads/scale-items (:kind ref) object replicas)
             ["scale a workload"]))
  :refusal (fn [{:keys [context ref object replicas]}]
             (workloads/scale-refusal-given (:kind ref) object (hpas context ref) replicas))
  ;; a workload already running that many ready replicas is skipped as done
  :done true
  :announce (fn [{:keys [target replicas]}] (format "Scale %s to %s replica(s)" target replicas))
  :effect (fn [{:keys [context ref replicas]}]
            (effect/plan (apply effect/kubectl context (workloads/scale-args ref replicas))))
  :verify (fn [{:keys [target replicas] :as ctx}]
            [{:description (format "%s runs %s ready replica(s)" target replicas)
              :timeout 600 :interval 10
              :check (fn [] (workloads/scaled? (mutation/read-subject ctx) replicas))}])})
