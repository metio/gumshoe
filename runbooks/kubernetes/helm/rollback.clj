;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.helm.rollback
  "Rolls a Helm release back to its newest earlier deployed revision, or to a
   named one."
  (:require [gumshoe.effect :as effect]
            [gumshoe.helm-rollback :as rollback]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn- revisions
  [context pick]
  (rollback/history (kubectl/helm-releases context) pick))

(mutation/book
 {:description "Rolls a Helm release back to its newest earlier deployed revision, or to a named one"
  :options {:release {:desc "\"<namespace>/<release>\" - interactive selection when omitted" :alias :r :coerce :string}
            :to-revision {:desc "The revision to go back to - the newest earlier deployed one when omitted" :coerce :long}}
  :prerequisites {:installed-tools ["kubectl" "helm" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["secrets"]}
  :select {:mode :one :label "Release" :flag :release
           :candidates (fn [context] (rollback/picks (kubectl/helm-releases context)))}
  :empty-message "no Helm release has an earlier deployed revision to go back to"
  :confirm {:action "roll a Helm release back" :destructive? true}
  :derive (fn [{:keys [context target opts]}]
            (let [pick (rollback/parse-pick target)
                  history (when pick (revisions context pick))]
              {:pick pick :history history :to (:to-revision opts)
               :before (:version (first history))
               :target-revision (rollback/rollback-target history (:to-revision opts))}))
  :items (fn [{:keys [pick history to target-revision target]}]
           (if target-revision (rollback/rollback-items pick history to) [(str "roll " target " back")]))
  :precondition (fn [{:keys [context pick to before]}]
                  (let [now (when pick (revisions context pick))
                        why (cond
                              (nil? pick) "the pick is not \"<namespace>/<release>\""
                              (not= before (:version (first now))) "the release changed since it was confirmed"
                              :else (rollback/rollback-refusal now to))]
                    (when why (println (str "refusing: " why)))
                    (nil? why)))
  :announce (fn [{:keys [target target-revision]}] (format "Roll Helm release %s back to revision %s" target (:version target-revision)))
  :effect (fn [{:keys [context pick target-revision]}]
            (if target-revision
              (effect/plan (apply effect/cmd (rollback/rollback-args context pick (:version target-revision))))
              (effect/plan (effect/note "nothing to roll back to - the precondition says why"))))
  :verify (fn [{:keys [context pick target before]}]
            [{:description (format "%s has a newer revision deployed" target)
              :timeout 600 :interval 10
              :check #(rollback/rolled-back? (revisions context pick) before)}])})
