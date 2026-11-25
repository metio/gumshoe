;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.fence
  "Fences a CloudNativePG instance, or every instance with *."
  (:require [gumshoe.cnpg.operations :as operations]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]))

(def selection
  {:mode :one :label "Fence" :flag :pick
   :candidates (fn [context] (operations/fence-picks (kubectl/get-all context cnpg/cluster-type)))})

(mutation/book
 {:description "Fences a CloudNativePG instance, or every instance with *"
  :options (select/options selection {:pick {:desc "\"<namespace>/<cluster> <instance>\" (by name, by number, or *) - interactive selection when omitted"}})
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get [cnpg/cluster-type]
                  :kubectl-can-patch [cnpg/cluster-type]}
  :select selection
  :empty-message "no cluster has an instance left to fence"
  :confirm {:action "fence an instance - the operator takes it out of service and will not promote it; fencing the primary or * leaves nothing accepting writes"
            :destructive? true}
  :subject {:type cnpg/cluster-type :parse operations/parse-pick
            :invalid "the pick is not \"<namespace>/<cluster> <instance>\""
            :facts operations/fenced}
  :refusal (fn [{:keys [ref object]}] (operations/fence-refusal object (:instance ref)))
  ;; an instance already in the wanted fence state is skipped as done
  :done true
  :announce (fn [{:keys [ref]}] (format "Fence %s in %s/%s" (:instance ref) (:namespace ref) (:cluster ref)))
  :effect (fn [{:keys [context ref object]}]
            (if-let [change (operations/fence-change :fence object (:instance ref))]
              (effect/plan (effect/kubectl context "--namespace" (:namespace ref) "annotate" cnpg/cluster-type
                                           (:cluster ref) "--overwrite" change))
              (effect/plan (effect/note "nothing can be written - the precondition says why"))))
  :verify (fn [{:keys [ref] :as ctx}]
            [{:description (format "%s is fenced" (:instance ref))
              :timeout 60 :interval 5
              :check (fn [] (operations/fenced? (mutation/read-subject ctx) (:instance ref)))}])})
