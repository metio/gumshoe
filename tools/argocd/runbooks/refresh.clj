;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.argocd.refresh
  "Refreshes an Argo CD Application, comparing it with its source again."
  (:require [gumshoe.argocd-actions :as actions]
            [gumshoe.detectives.argocd :as argocd]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn- fetch
  [context {:keys [namespace name]}]
  (kubectl/get-namespaced-resource context namespace argocd/application-type name))

(defn- refresh-type [opts] (if (:hard opts) "hard" "normal"))
(mutation/book
 {:description "Refreshes an Argo CD Application, comparing it with its source again"
  :options {:application {:desc "\"<namespace>/<name>\" - interactive selection when omitted" :alias :a :coerce :string}
            :hard {:desc "Regenerate the manifests instead of reading the manifest cache" :coerce :boolean}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get [argocd/application-type]
                  :kubectl-can-patch [argocd/application-type]}
  :select {:mode :one :label "Application" :flag :application
           :candidates (fn [context]
                         (vec (for [app (sort-by kubectl/namespace-name-of (kubectl/items-of (kubectl/get-all context argocd/application-type)))
                                    :when (nil? (actions/refresh-refusal app "normal"))]
                                (kubectl/namespace-name-of app))))}
  :empty-message "every Application has a refresh pending"
  :confirm {:action "refresh an Application"}
  :derive (fn [{:keys [context target]}]
            (let [pick (actions/parse-pick target)]
              {:pick pick :application (when pick (fetch context pick))}))
  :items (fn [{:keys [application target opts]}]
           (if application (actions/refresh-items application (refresh-type opts)) [(str "refresh " target)]))
  :pin (fn [{:keys [pick]}] (when pick {:type argocd/application-type :namespace (:namespace pick) :name (:name pick)}))
  :precondition (fn [{:keys [context pick opts]}]
                  (let [app (when pick (fetch context pick))
                        why (if pick (actions/refresh-refusal app (refresh-type opts)) "the pick is not \"<namespace>/<name>\"")]
                    (when why (println (str "refusing: " why)))
                    (nil? why)))
  :announce (fn [{:keys [target]}] (format "Refresh Application %s" target))
  :effect (fn [{:keys [context pick opts]}]
            (effect/plan (effect/kubectl context (str "--namespace=" (:namespace pick)) "annotate" argocd/application-type (:name pick)
                                         "--overwrite" (actions/refresh-arg (refresh-type opts)))))
  :verify (fn [{:keys [context pick target]}]
            [{:description (format "the controller refreshed %s" target)
              :timeout 300 :interval 5
              :check #(actions/refreshed? (fetch context pick))}])})
