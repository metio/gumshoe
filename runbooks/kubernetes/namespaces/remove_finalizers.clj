;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.namespaces.remove-finalizers
  "Removes the kubernetes finalizer from a namespace stuck in Terminating.

   The finalize subresource removes the Namespace without deleting what is
   left in it, so the book shows the namespace's deletion conditions and the
   objects it still holds, and refuses while objects may remain unless the
   operator passes --orphan-remaining."
  (:require [cheshire.core :as json]
            [gumshoe.cleanup :as cleanup]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]))

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

(def selection
  {:mode :one :label "Namespace" :flag :namespace :candidates terminating-namespaces})

(mutation/book
 {:description "Removes the kubernetes finalizer from a namespace stuck in Terminating"
  :options (select/options selection {:namespace {:desc "The stuck namespace - interactive selection when omitted"}
                                      :orphan-remaining {:desc "Finalize even though objects may remain in the namespace, leaving them in etcd without cleanup"
                                                         :coerce :boolean}})
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["namespaces"]
                  :kubectl-can-update ["namespaces/finalize"]}
  :select selection
  :empty-message "no namespace is stuck in Terminating"
  :confirm {:action (str "force-remove the namespace - objects still in it are NOT deleted: they stay in etcd "
                         "without cleanup, and a namespace created later under this name exposes them again")
            :destructive? true}
  :subject {:type "namespaces"}
  :reads {:contents (fn [{:keys [context target]}] (kubectl/namespace-objects context target))}
  :items (fn [{:keys [target object contents]}] (cleanup/namespace-items target object contents))
  :refusal (fn [{:keys [target object contents opts]}]
             (or (cleanup/namespace-refusal target object)
                 (cleanup/orphan-refusal target object contents (:orphan-remaining opts))))
  :announce (fn [{:keys [target]}] (format "Remove finalizers from namespace %s" target))
  :effect (fn [{:keys [context target object]}] (finalize-effect context target object))
  :verify (fn [{:keys [context target]}]
            [{:description (format "namespace %s is gone" target)
              :timeout 120 :interval 10 :verifies-absence? true
              :check (fn [] (kubectl/confirmed-gone? context nil "namespaces" target))}])})
