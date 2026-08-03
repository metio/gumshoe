;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.jaas.reconcile
  "Requests an immediate re-evaluation of a JsonnetSnippet by stamping the
   reconcile.fluxcd.io/requestedAt annotation - the trigger `flux reconcile`
   uses, which JaaS honours on its own kinds."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.tools.jaas :as jaas]))

(def reconcile-annotation "reconcile.fluxcd.io/requestedAt")

(defn snippets
  [context]
  (kubectl/namespaces-names (kubectl/get-all context jaas/jsonnetsnippet-type)))

(defn handled-check
  "The operator writes the token it acted on to status.lastHandledReconcileAt, so
   that field going to our token proves the request was picked up. Readiness is
   deliberately not the check: a snippet can be re-evaluated and still fail, and
   that failure is a result to read, not a post-check to hang on."
  [context namespace name target token]
  {:description (format "JsonnetSnippet %s handled the reconcile request" target)
   :timeout 120 :interval 5
   :check (fn []
            (= token (-> (kubectl/get-namespaced-resource context namespace
                                                          jaas/jsonnetsnippet-type name)
                         :status :lastHandledReconcileAt)))})

(mutation/book
 {:description "Requests an immediate re-evaluation of a JsonnetSnippet"
  :options {:namespace {:desc "The namespace of the JsonnetSnippet - interactive selection when omitted"
                        :alias :n :coerce :string}
            :name {:desc "The name of the JsonnetSnippet - interactive selection when omitted"
                   :alias :s :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities [:jaas]
                  :kubectl-can-get [jaas/jsonnetsnippet-type]
                  :kubectl-can-patch [jaas/jsonnetsnippet-type]}
  :select {:mode :namespaced :label "JsonnetSnippet" :namespace-flag :namespace :name-flag :name
           :candidates snippets}
  ;; The token is derived once, in :derive, so the effect that stamps it and the
  ;; post-check that waits for it are looking at the same value.
  :derive (fn [_] {:token (str (java.time.Instant/now))})
  :confirm {:action "request a JsonnetSnippet re-evaluation"}
  :announce (fn [{:keys [target]}] (format "Reconcile JsonnetSnippet %s" target))
  :effect (fn [{:keys [context target token]}]
            (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
              (effect/plan
               (effect/kubectl context (str "--namespace=" namespace)
                               "annotate" jaas/jsonnetsnippet-type name
                               (str reconcile-annotation "=" token)
                               "--overwrite"))))
  :verify (fn [{:keys [context target token]}]
            (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
              [(handled-check context namespace name target token)]))})
