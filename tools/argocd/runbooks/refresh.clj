;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.argocd.refresh
  "Refreshes an Argo CD Application, comparing it with its source again."
  (:require [gumshoe.argocd-actions :as actions]
            [gumshoe.detectives.argocd :as argocd]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]))

(defn- refresh-type [opts] (if (:hard opts) "hard" "normal"))
(def selection
  {:mode :one :label "Application" :flag :application
   :candidates (fn [context]
                 (vec (for [app (sort-by kubectl/namespace-name-of (kubectl/items-of (kubectl/get-all context argocd/application-type)))
                            :when (nil? (actions/refresh-refusal app "normal"))]
                        (kubectl/namespace-name-of app))))})

(mutation/book
 {:description "Refreshes an Argo CD Application, comparing it with its source again"
  :options (select/options selection {:application {:desc "\"<namespace>/<name>\" - interactive selection when omitted"}
                                      :hard {:desc "Regenerate the manifests instead of reading the manifest cache" :coerce :boolean}})
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get [argocd/application-type]
                  :kubectl-can-patch [argocd/application-type]}
  :select selection
  :empty-message "every Application has a refresh pending"
  :confirm {:action "refresh an Application" :destructive? false}
  :subject {:type argocd/application-type :parse actions/parse-pick :invalid "the pick is not \"<namespace>/<name>\""}
  :items (fn [{:keys [object target opts]}]
           (if object (actions/refresh-items object (refresh-type opts)) [(str "refresh " target)]))
  :refusal (fn [{:keys [object opts]}] (actions/refresh-refusal object (refresh-type opts)))
  :announce (fn [{:keys [target]}] (format "Refresh Application %s" target))
  :effect (fn [{:keys [context ref opts]}]
            (effect/plan (effect/kubectl context (str "--namespace=" (:namespace ref)) "annotate" argocd/application-type (:name ref)
                                         "--overwrite" (actions/refresh-arg (refresh-type opts)))))
  :verify (fn [{:keys [target] :as ctx}]
            [{:description (format "the controller refreshed %s" target)
              :timeout 300 :interval 5
              :check #(actions/refreshed? (mutation/read-subject ctx))}])})
