;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.stageset.promote
  "Manually promotes a StageSet stage that is held on a manual gate
   (AwaitingPromotion), via stagesetctl."
  (:require [gumshoe.effect :as effect]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.tools.stageset :as stageset]))

(defn stagesets
  [context]
  (kubectl/namespaces-names (kubectl/get-all context stageset/stageset-type)))

(defn stage-names
  "The declared stage names of a StageSet, so a held stage can be picked by name."
  [context namespace name]
  (let [resource (kubectl/get-namespaced-resource context namespace stageset/stageset-type name)]
    (mapv :name (or (seq (-> resource :status :stages))
                    (-> resource :spec :stages)))))

(mutation/book
 {:description "Manually promotes a held StageSet stage via stagesetctl"
  :options {:namespace {:desc "The namespace of the StageSet - interactive selection when omitted"
                        :alias :n :coerce :string}
            :name {:desc "The name of the StageSet - interactive selection when omitted"
                   :alias :s :coerce :string}
            :stage {:desc "The stage to promote - interactive selection when omitted"
                    :coerce :string}}
  :prerequisites {:installed-tools ["stagesetctl" "kubectl" "fzf"]
                  :cluster-capabilities [:stageset]
                  :kubectl-can-get [stageset/stageset-type]}
  :select {:mode :namespaced :label "StageSet" :namespace-flag :namespace :name-flag :name
           :candidates stagesets}
  :derive (fn [{:keys [context target opts]}]
            (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
              {:stage (interact/choose-one "Stage" (stage-names context namespace name) (:stage opts))}))
  :confirm {:action "promote a StageSet stage"}
  :announce (fn [{:keys [target stage]}] (format "Promote stage %s of StageSet %s" stage target))
  :effect (fn [{:keys [target stage]}]
            (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
              (effect/plan (effect/cmd "stagesetctl" "promote" name "--stage" stage
                                       (str "--namespace=" namespace)))))})
