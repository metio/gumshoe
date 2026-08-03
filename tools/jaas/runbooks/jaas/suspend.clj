;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.jaas.suspend
  "Suspends a JsonnetSnippet, so the operator stops re-evaluating and publishing
   it. The remediation for a snippet that is churning - a hot loop against a
   moving source, an evaluation that keeps timing out - while you fix the cause."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.tools.jaas :as jaas]))

(defn snippets
  [context]
  (kubectl/namespaces-names (kubectl/get-all context jaas/jsonnetsnippet-type)))

(defn suspended-check
  [context namespace name target]
  {:description (format "JsonnetSnippet %s reports itself suspended" target)
   :timeout 60 :interval 5
   :check (fn [] (= "Suspended" (jaas/ready-reason context namespace name)))})

(mutation/book
 {:description "Suspends a JsonnetSnippet so the operator stops re-evaluating it"
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
  :confirm {:action "suspend a JsonnetSnippet"}
  :announce (fn [{:keys [target]}] (format "Suspend JsonnetSnippet %s" target))
  :effect (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
              (effect/plan
               ;; Suspending pauses writes; it does not withdraw what is already
               ;; published, so consumers keep fetching the last good artifact.
               (effect/note "consumers keep serving the last published artifact while suspended")
               (effect/kubectl context (str "--namespace=" namespace)
                               "patch" jaas/jsonnetsnippet-type name
                               "--type=merge" "--patch" "{\"spec\":{\"suspend\":true}}"))))
  :verify (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
              [(suspended-check context namespace name target)]))})
