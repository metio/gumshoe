;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.unfence
  "Lifts the fence on a CloudNativePG instance, or on every instance with *."
  (:require [gumshoe.cnpg.operations :as operations]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn- fetch
  [context {:keys [namespace cluster]}]
  (kubectl/get-namespaced-resource context namespace cnpg/cluster-type cluster))

(mutation/book
 {:description "Lifts the fence on a CloudNativePG instance, or on every instance with *"
  :options {:pick {:desc "\"<namespace>/<cluster> <instance>\" (by name, by number, or *) - interactive selection when omitted"
                   :alias :p :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get [cnpg/cluster-type]
                  :kubectl-can-patch [cnpg/cluster-type]}
  :select {:mode :one :label "Unfence" :flag :pick
           :candidates (fn [context] (operations/unfence-picks (kubectl/get-all context cnpg/cluster-type)))}
  :empty-message "no cluster has a fenced instance"
  :confirm {:action "lift the fence - the operator takes the instance back into service"
            :destructive? true}
  :derive (fn [{:keys [target]}] {:pick (operations/parse-pick target)})
  :precondition (fn [{:keys [context pick]}]
                  (let [why (if pick
                              (operations/unfence-refusal (fetch context pick) (:instance pick))
                              "the pick is not \"<namespace>/<cluster> <instance>\"")]
                    (when why (println (str "refusing: " why)))
                    (nil? why)))
  :announce (fn [{:keys [pick]}] (format "Unfence %s in %s/%s" (:instance pick) (:namespace pick) (:cluster pick)))
  :effect (fn [{:keys [context pick]}]
            (if-let [change (operations/fence-change :unfence (fetch context pick) (:instance pick))]
              (effect/plan (effect/kubectl context "--namespace" (:namespace pick) "annotate" cnpg/cluster-type
                                           (:cluster pick) "--overwrite" change))
              (effect/plan (effect/note "nothing can be written - the precondition says why"))))
  :verify (fn [{:keys [context pick]}]
            [{:description (format "%s is no longer fenced" (:instance pick))
              :timeout 60 :interval 5
              :check (fn [] (let [cluster (fetch context pick)] (not (contains? (or (operations/fenced cluster) #{}) (:instance pick)))))}])})
