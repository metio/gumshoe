;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.flux.reconcile
  "Triggers a flux reconciliation of a single HelmRelease."
  (:require [gumshoe.conditions :as conditions]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.tools.flux]))

(def helmrelease-type "helmreleases.helm.toolkit.fluxcd.io")

(defn helmreleases
  [context]
  (kubectl/namespaces-names (kubectl/get-all context helmrelease-type)))

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
    :fetch #(kubectl/get-namespaced-resource context namespace helmrelease-type name)}))

(mutation/book
 {:description "Triggers a flux reconciliation of a single HelmRelease"
  :options {:namespace {:desc "The namespace of the HelmRelease - interactive selection when omitted"
                        :alias :n :coerce :string}
            :name {:desc "The name of the HelmRelease - interactive selection when omitted"
                   :alias :r :coerce :string}}
  :prerequisites {:installed-tools ["flux" "kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get [helmrelease-type]
                  :kubectl-can-patch [helmrelease-type]}
  :select {:mode :namespaced :label "HelmRelease" :namespace-flag :namespace :name-flag :name
           :candidates helmreleases}
  :confirm {:action "trigger a flux reconciliation"}
  :announce (fn [{:keys [target]}] (format "Reconcile HelmRelease %s" target))
  :effect (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
              (reconcile-effect context namespace name)))
  :verify (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
              [(ready-check context namespace name target)]))})
