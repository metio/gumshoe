;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.namespaces.remove-finalizers
  "Removes the kubernetes finalizer from a namespace stuck in Terminating."
  (:require [cheshire.core :as json]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn terminating-namespaces
  [context]
  (kubectl/terminating-namespaces (kubectl/get-all context "namespaces")))

(defn finalize-effect
  "Reads the namespace, drops its kubernetes finalizer, and returns the plan
   that writes it back through the finalize subresource."
  [context namespace]
  (let [resource (kubectl/get-cluster-resource context "namespaces" namespace)
        finalized (kubectl/remove-kubernetes-finalizer resource)
        path (format "/api/v1/namespaces/%s/finalize" namespace)]
    (effect/plan (effect/kubectl-stdin context (json/encode finalized)
                                       "replace" "--raw" path "--filename" "-"))))

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
  :announce (fn [{:keys [target]}] (format "Remove finalizers from namespace %s" target))
  :effect (fn [{:keys [context target]}] (finalize-effect context target))
  :verify (fn [{:keys [context target]}]
            [{:description (format "namespace %s is gone" target)
              :timeout 120 :interval 10
              :check (fn [] (nil? (kubectl/get-cluster-resource context "namespaces" target)))}])})
