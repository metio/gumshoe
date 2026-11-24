;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.stageset.reconcile
  "Triggers a StageSet reconciliation via stagesetctl, optionally overriding the
   update window so a held revision rolls now."
  (:require [gumshoe.conditions :as conditions]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.tools.stageset :as stageset]))

(defn stagesets
  [context]
  (kubectl/namespaces-names (kubectl/get-all context stageset/stageset-type)))

(defn ready-check
  [context namespace name target]
  (conditions/ready-check
   {:description (format "StageSet %s is Ready" target)
    :timeout 120 :interval 10
    :fetch #(kubectl/get-namespaced-resource context namespace stageset/stageset-type name)}))

(mutation/book
 {:description "Triggers a StageSet reconciliation via stagesetctl"
  :options {:namespace {:desc "The namespace of the StageSet - interactive selection when omitted"
                        :alias :n :coerce :string}
            :name {:desc "The name of the StageSet - interactive selection when omitted"
                   :alias :s :coerce :string}
            :update-now {:desc "Override the update window and roll a held revision now"
                         :coerce :boolean}}
  :prerequisites {:installed-tools ["stagesetctl" "kubectl" "fzf"]
                  :cluster-capabilities [:stageset]
                  :kubectl-can-get [stageset/stageset-type]}
  :select {:mode :namespaced :label "StageSet" :namespace-flag :namespace :name-flag :name
           :candidates stagesets}
  :confirm {:action "trigger a StageSet reconciliation"}
  :announce (fn [{:keys [target]}] (format "Reconcile StageSet %s" target))
  :effect (fn [{:keys [target opts]}]
            (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
              (effect/plan
               (apply effect/cmd "stagesetctl" "reconcile" name
                      (str "--namespace=" namespace)
                      (when (:update-now opts) ["--update-now"])))))
  :verify (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
              [(ready-check context namespace name target)]))})
