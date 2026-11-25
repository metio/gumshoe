;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.stageset.promote
  "Manually promotes a StageSet stage that is held on a manual gate
   (AwaitingPromotion), via stagesetctl."
  (:require [gumshoe.effect :as effect]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]
            [gumshoe.tools.stageset :as stageset]))

(defn stagesets
  [context]
  (kubectl/namespaces-names (kubectl/get-all context stageset/stageset-type)))


(def selection
  {:mode :namespaced :label "StageSet" :namespace-flag :namespace :name-flag :name
   :candidates stagesets})

(mutation/book
 {:description "Manually promotes a held StageSet stage via stagesetctl"
  :options (select/options selection {:stage {:desc "The stage to promote - interactive selection when omitted"
                                              :coerce :string}})
  :prerequisites {:installed-tools ["stagesetctl" "kubectl" "fzf"]
                  :cluster-capabilities [:stageset]
                  :kubectl-can-get [stageset/stageset-type]}
  :select selection
  :subject {:type stageset/stageset-type}
  ;; a held stage is picked by name among those the StageSet declares
  :values {:stage (fn [{:keys [object opts]}]
                    (interact/choose-one "Stage" (stageset/declared-stages object) (:stage opts)))}
  :refusal (fn [{:keys [target object stage opts]}]
             (stageset/promotion-refusal target object stage (:stage opts)))
  :confirm {:action "promote a StageSet stage" :destructive? false}
  :announce (fn [{:keys [target stage]}] (format "Promote stage %s of StageSet %s" stage target))
  :effect (fn [{:keys [ref stage]}]
            (if stage
              (effect/plan (effect/cmd "stagesetctl" "promote" (:name ref) "--stage" stage
                                       (str "--namespace=" (:namespace ref))))
              (effect/plan (effect/note "no stage was chosen; nothing is promoted"))))
  ;; stagesetctl's own --wait gives up after five minutes; so does this.
  :verify (fn [{:keys [context target stage]}]
            (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
              [{:description (format "stage %s of StageSet %s acted on the promotion" stage target)
                :timeout 300 :interval 5
                :check #(stageset/promotion-handled?
                         (kubectl/get-namespaced-resource context namespace stageset/stageset-type name)
                         stage)}]))})
