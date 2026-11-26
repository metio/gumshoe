;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.switchover
  "Switches a CloudNativePG cluster over to one of its healthy replicas."
  (:require [gumshoe.cnpg.operations :as operations]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]))

(def selection
  {:mode :one :label "New primary" :flag :pick
   :candidates (fn [context] (operations/switchover-picks (kubectl/get-all context cnpg/cluster-type)))})

(mutation/book
 {:description "Switches a CloudNativePG cluster over to one of its healthy replicas"
  :options (select/options selection {:pick {:desc "\"<namespace>/<cluster> <instance>\" (the instance by name or number) - interactive selection when omitted"}})
  :prerequisites {:installed-tools ["kubectl" "kubectl-cnpg" "fzf"]
                  :cluster-capabilities []
                  ;; the plugin reads the instance's pod and writes
                  ;; status.targetPrimary through the status subresource
                  :kubectl-can-get [cnpg/cluster-type "pods"]
                  :kubectl-can-patch [operations/cluster-status]}
  :select selection
  :empty-message "no cluster has a healthy, unfenced replica to switch over to"
  :confirm {:action (str "switch the primary over - the old primary restarts as a replica, and clients "
                         "of the -rw Service lose their connections and must reconnect")
            :destructive? true}
  :subject {:type cnpg/cluster-type :parse operations/parse-pick
            :invalid "the pick is not \"<namespace>/<cluster> <instance>\""
            :facts #(select-keys (:status %) [:currentPrimary :targetPrimary])}
  :refusal (fn [{:keys [ref object]}] (operations/switchover-refusal object (:instance ref)))
  :announce (fn [{:keys [ref]}] (format "Promote %s in %s/%s" (:instance ref) (:namespace ref) (:cluster ref)))
  :effect (fn [{:keys [context ref]}]
            (effect/plan (apply effect/cmd (operations/promote-args context ref))))
  :verify (fn [{:keys [ref] :as ctx}]
            [{:description (format "%s is the primary" (:instance ref))
              :timeout 300 :interval 10
              :check (fn [] (= (:instance ref) (-> (mutation/read-subject ctx) :status :currentPrimary)))}])})
