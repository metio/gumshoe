;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.helmreleases.reconcile
  "Triggers a flux reconciliation of a single HelmRelease."
  (:require [infra.effect :as effect]
            [infra.kubectl :as kubectl]
            [infra.mutation :as mutation]))

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
  {:description (format "HelmRelease %s is Ready" target)
   :timeout 120 :interval 10
   :check (fn []
            (->> (-> (kubectl/get-namespaced-resource context namespace helmrelease-type name)
                     :status :conditions)
                 (some #(and (= "Ready" (:type %)) (= "True" (:status %))))
                 boolean))})

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
            (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
              (reconcile-effect context namespace name)))
  :verify (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
              [(ready-check context namespace name target)]))})
