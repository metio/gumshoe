;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.namespaces.remove-finalizers
  "Removes the kubernetes finalizer from a namespace stuck in Terminating."
  (:require [cheshire.core :as json]
            [gumshoe.cleanup :as cleanup]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn terminating-namespaces
  [context]
  (kubectl/terminating-namespaces (kubectl/get-all context "namespaces")))

(defn finalize-effect
  "The plan that writes `resource`, the namespace as the operator confirmed
   it, back through the finalize subresource without its kubernetes
   finalizer. The body carries the confirmed resourceVersion, so the API
   server refuses the write when the namespace changed since. Without a
   resource the plan writes nothing."
  [context namespace resource]
  (if-not resource
    (effect/plan (effect/note (format "namespace %s was not found; nothing is written back" namespace)))
    (let [finalized (kubectl/remove-kubernetes-finalizer resource)
          path (format "/api/v1/namespaces/%s/finalize" namespace)]
      (effect/plan (effect/kubectl-stdin context (json/encode finalized)
                                         "replace" "--raw" path "--filename" "-")))))

(mutation/book
 {:description "Removes the kubernetes finalizer from a namespace stuck in Terminating"
  :options {:namespace {:desc "The stuck namespace - interactive selection when omitted"
                        :alias :n :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["namespaces"]}
  :select {:mode :one :label "Namespace" :flag :namespace :candidates terminating-namespaces}
  :empty-message "no namespace is stuck in Terminating"
  :confirm {:action "force namespace termination - every resource left in it is deleted"
            :destructive? true}
  :subject {:type "namespaces"}
  :refusal (fn [{:keys [target object]}] (cleanup/namespace-refusal target object))
  :announce (fn [{:keys [target]}] (format "Remove finalizers from namespace %s" target))
  :effect (fn [{:keys [context target object]}] (finalize-effect context target object))
  :verify (fn [{:keys [context target]}]
            [{:description (format "namespace %s is gone" target)
              :timeout 120 :interval 10 :verifies-absence? true
              :check (fn [] (kubectl/confirmed-gone? context nil "namespaces" target))}])})
