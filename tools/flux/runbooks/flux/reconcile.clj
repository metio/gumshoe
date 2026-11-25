;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.flux.reconcile
  "Triggers a flux reconciliation of a single HelmRelease."
  (:require [gumshoe.conditions :as conditions]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]
            [gumshoe.tools.flux :as flux]))

(defn helmreleases
  [context]
  (kubectl/namespaces-names (kubectl/get-all context flux/helmrelease-type)))

(defn reconcile-effect
  [context namespace name]
  (effect/plan (effect/cmd "flux" "reconcile" "helmrelease" name
                           (str "--context=" context)
                           (str "--namespace=" namespace))))

(defn ready-check
  [context namespace name target]
  (conditions/ready-check
   {:description (format "HelmRelease %s is Ready" target)
    :timeout 120 :interval 10
    :fetch #(kubectl/get-namespaced-resource context namespace flux/helmrelease-type name)}))

(def selection
  {:mode :namespaced :label "HelmRelease" :namespace-flag :namespace :name-flag :name
   :candidates helmreleases})

(mutation/book
 {:description "Triggers a flux reconciliation of a single HelmRelease"
  :options (select/options selection)
  :prerequisites {:installed-tools ["flux" "kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get [flux/helmrelease-type]
                  :kubectl-can-patch [flux/helmrelease-type]}
  :select selection
  :confirm {:action "trigger a flux reconciliation" :destructive? false}
  :subject {:type flux/helmrelease-type}
  :refusal (fn [{:keys [target object]}] (flux/helmrelease-refusal target object))
  :announce (fn [{:keys [target]}] (format "Reconcile HelmRelease %s" target))
  :effect (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
              (reconcile-effect context namespace name)))
  :verify (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
              [(ready-check context namespace name target)]))})
