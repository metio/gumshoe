;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.argocd.sync
  "Syncs an Argo CD Application to its source and waits for the operation."
  (:require [gumshoe.argocd-actions :as actions]
            [gumshoe.detectives.argocd :as argocd]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(mutation/book
 {:description "Syncs an Argo CD Application to its source and waits for the operation"
  :options {:application {:desc "\"<namespace>/<name>\" - interactive selection when omitted" :alias :a :coerce :string}
            :prune {:desc "Delete resources that were removed from Git" :coerce :boolean}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get [argocd/application-type]
                  :kubectl-can-patch [argocd/application-type]}
  :select {:mode :one :label "Application" :flag :application
           :candidates (fn [context]
                         (vec (for [app (sort-by kubectl/namespace-name-of (kubectl/items-of (kubectl/get-all context argocd/application-type)))
                                    :when (nil? (actions/sync-refusal app))]
                                (kubectl/namespace-name-of app))))}
  :empty-message "every Application is running an operation already"
  :confirm {:action "sync an Application - the cluster is changed to match its source" :destructive? true}
  :subject {:type argocd/application-type :parse actions/parse-pick :invalid "the pick is not \"<namespace>/<name>\""}
  ;; taken before the change, so the operation it starts is newer
  :derive (fn [_] {:requested (java.time.Instant/now)})
  :items (fn [{:keys [object target opts]}]
           (if object (actions/sync-items object (boolean (:prune opts))) [(str "sync " target)]))
  :refusal (fn [{:keys [object]}] (actions/sync-refusal object))
  :announce (fn [{:keys [target]}] (format "Sync Application %s" target))
  :effect (fn [{:keys [context ref opts]}]
            (effect/plan (effect/kubectl context (str "--namespace=" (:namespace ref)) "patch" argocd/application-type (:name ref)
                                         "--type=merge" (str "--patch=" (actions/sync-patch "gumshoe" (boolean (:prune opts)))))))
  :verify (fn [{:keys [target requested] :as ctx}]
            [{:description (format "%s synced" target)
              :timeout 900 :interval 10
              :abort #(first (actions/sync-signals (mutation/read-subject ctx) requested))
              :check #(actions/synced? (mutation/read-subject ctx) requested)}])})
