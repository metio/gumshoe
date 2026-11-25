;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.unfence
  "Lifts the fence on a CloudNativePG instance, or on every instance with *."
  (:require [gumshoe.cnpg.operations :as operations]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.verify :as verify]))

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
  :subject {:type cnpg/cluster-type :parse operations/parse-pick
            :invalid "the pick is not \"<namespace>/<cluster> <instance>\""
            :facts operations/fenced}
  :refusal (fn [{:keys [ref object]}] (operations/unfence-refusal object (:instance ref)))
  :announce (fn [{:keys [ref]}] (format "Unfence %s in %s/%s" (:instance ref) (:namespace ref) (:cluster ref)))
  :effect (fn [{:keys [context ref object]}]
            (if-let [change (operations/fence-change :unfence object (:instance ref))]
              (effect/plan (effect/kubectl context "--namespace" (:namespace ref) "annotate" cnpg/cluster-type
                                           (:cluster ref) "--overwrite" change))
              (effect/plan (effect/note "nothing can be written - the precondition says why"))))
  :verify (fn [{:keys [ref] :as ctx}]
            [{:description (format "%s is no longer fenced" (:instance ref))
              :timeout 60 :interval 5
              ;; an annotation that does not parse says nothing about the fence
              :check (fn [] (verify/object-holds? (mutation/read-subject ctx)
                                                  #(and (some? (operations/fenced %))
                                                        (not (operations/fenced? % (:instance ref))))))}])})
