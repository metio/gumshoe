;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.workloads.scale
  "Scales a Deployment or StatefulSet to a replica count."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.workloads :as workloads]))

(defn- fetch
  [context {:keys [type namespace name]}]
  (kubectl/get-namespaced-resource context namespace type name))

(defn- hpas
  [context {:keys [namespace]}]
  (kubectl/get-namespaced context namespace "horizontalpodautoscalers"))

(defn- candidates
  [context]
  (workloads/picks (for [[type kind] workloads/types :when (workloads/scalable-kinds kind)]
                     [kind (kubectl/get-all context type)])
                   (constantly true)))

(defn- refusal
  [context pick replicas]
  (if pick
    (workloads/scale-refusal (:kind pick) (fetch context pick) (hpas context pick) replicas)
    "the pick is not \"<Kind> <namespace>/<name>\" of a Deployment or StatefulSet"))

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
  :derive (fn [{:keys [context target opts]}]
            (let [pick (workloads/parse-pick target)]
              {:pick pick :replicas (:replicas opts) :before (when pick (fetch context pick))}))
  :items (fn [{:keys [pick before replicas]}]
           (if (and pick before (integer? replicas))
             (workloads/scale-items (:kind pick) before replicas)
             ["scale a workload"]))
  :pin :pick
  :precondition (fn [{:keys [context pick replicas]}]
                  (let [why (refusal context pick replicas)]
                    (when why (println (str "refusing: " why)))
                    (nil? why)))
  :announce (fn [{:keys [target replicas]}] (format "Scale %s to %s replica(s)" target replicas))
  :effect (fn [{:keys [context pick replicas]}]
            (effect/plan (apply effect/kubectl context (workloads/scale-args pick replicas))))
  :verify (fn [{:keys [context pick target replicas]}]
            [{:description (format "%s runs %s ready replica(s)" target replicas)
              :timeout 600 :interval 10
              :check (fn [] (workloads/scaled? (fetch context pick) replicas))}])})
