;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.argocd.sync
  "Syncs an Argo CD Application to its source and waits for the operation."
  (:require [gumshoe.argocd-actions :as actions]
            [gumshoe.detectives.argocd :as argocd]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn- fetch
  [context {:keys [namespace name]}]
  (kubectl/get-namespaced-resource context namespace argocd/application-type name))

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
  :derive (fn [{:keys [context target]}]
            (let [pick (actions/parse-pick target)]
              ;; taken before the change, so the operation it starts is newer
              {:pick pick :application (when pick (fetch context pick)) :requested (java.time.Instant/now)}))
  :items (fn [{:keys [application target opts]}]
           (if application (actions/sync-items application (boolean (:prune opts))) [(str "sync " target)]))
  :pin (fn [{:keys [pick]}] (when pick {:type argocd/application-type :namespace (:namespace pick) :name (:name pick)}))
  :precondition (fn [{:keys [context pick opts]}]
                  (let [app (when pick (fetch context pick))
                        why (if pick (actions/sync-refusal app) "the pick is not \"<namespace>/<name>\"")]
                    (when why (println (str "refusing: " why)))
                    (nil? why)))
  :announce (fn [{:keys [target]}] (format "Sync Application %s" target))
  :effect (fn [{:keys [context pick opts]}]
            (effect/plan (effect/kubectl context (str "--namespace=" (:namespace pick)) "patch" argocd/application-type (:name pick)
                                         "--type=merge" (str "--patch=" (actions/sync-patch "gumshoe" (boolean (:prune opts)))))))
  :verify (fn [{:keys [context pick target requested]}]
            [{:description (format "%s synced" target)
              :timeout 900 :interval 10
              :abort #(first (actions/sync-signals (fetch context pick) requested))
              :check #(actions/synced? (fetch context pick) requested)}])})
