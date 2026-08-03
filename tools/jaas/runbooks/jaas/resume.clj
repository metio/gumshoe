;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.jaas.resume
  "Resumes a suspended JsonnetSnippet, so the operator evaluates and publishes it
   again."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.tools.jaas :as jaas]))

(defn suspended-snippets
  "Only the suspended ones - resuming a snippet that is already running is a
   no-op, so offering the whole list would just be noise."
  [context]
  (->> (kubectl/items-of (kubectl/get-all context jaas/jsonnetsnippet-type))
       (filter #(-> % :spec :suspend))
       (mapv kubectl/namespace-name-of)))

(defn resumed-check
  [context namespace name target]
  {:description (format "JsonnetSnippet %s is no longer suspended" target)
   :timeout 60 :interval 5
   :check (fn [] (not= "Suspended" (jaas/ready-reason context namespace name)))})

(mutation/book
 {:description "Resumes a suspended JsonnetSnippet"
  :options {:namespace {:desc "The namespace of the JsonnetSnippet - interactive selection when omitted"
                        :alias :n :coerce :string}
            :name {:desc "The name of the JsonnetSnippet - interactive selection when omitted"
                   :alias :s :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities [:jaas]
                  :kubectl-can-get [jaas/jsonnetsnippet-type]
                  :kubectl-can-patch [jaas/jsonnetsnippet-type]}
  :select {:mode :namespaced :label "suspended JsonnetSnippet"
           :namespace-flag :namespace :name-flag :name
           :candidates suspended-snippets}
  :confirm {:action "resume a JsonnetSnippet"}
  :announce (fn [{:keys [target]}] (format "Resume JsonnetSnippet %s" target))
  :effect (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
              (effect/plan
               (effect/kubectl context (str "--namespace=" namespace)
                               "patch" jaas/jsonnetsnippet-type name
                               "--type=merge" "--patch" "{\"spec\":{\"suspend\":false}}"))))
  :verify (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
              [(resumed-check context namespace name target)]))})
