;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.switchover
  "Switches a CloudNativePG cluster over to one of its healthy replicas."
  (:require [gumshoe.cnpg.operations :as operations]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn- fetch
  [context {:keys [namespace cluster]}]
  (kubectl/get-namespaced-resource context namespace cnpg/cluster-type cluster))

(mutation/book
 {:description "Switches a CloudNativePG cluster over to one of its healthy replicas"
  :options {:pick {:desc "\"<namespace>/<cluster> <instance>\" (the instance by name or number) - interactive selection when omitted"
                   :alias :p :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "kubectl-cnpg" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get [cnpg/cluster-type]
                  :kubectl-can-patch [cnpg/cluster-type]}
  :select {:mode :one :label "New primary" :flag :pick
           :candidates (fn [context] (operations/switchover-picks (kubectl/get-all context cnpg/cluster-type)))}
  :empty-message "no cluster has a healthy, unfenced replica to switch over to"
  :confirm {:action (str "switch the primary over - the old primary restarts as a replica, and clients "
                         "of the -rw Service lose their connections and must reconnect")
            :destructive? true}
  :derive (fn [{:keys [target]}] {:pick (operations/parse-pick target)})
  :precondition (fn [{:keys [context pick]}]
                  (let [why (if pick
                              (operations/switchover-refusal (fetch context pick) (:instance pick))
                              "the pick is not \"<namespace>/<cluster> <instance>\"")]
                    (when why (println (str "refusing: " why)))
                    (nil? why)))
  :announce (fn [{:keys [pick]}] (format "Promote %s in %s/%s" (:instance pick) (:namespace pick) (:cluster pick)))
  :effect (fn [{:keys [context pick]}]
            (effect/plan (apply effect/cmd (operations/promote-args context pick))))
  :verify (fn [{:keys [context pick]}]
            [{:description (format "%s is the primary" (:instance pick))
              :timeout 300 :interval 10
              :check (fn [] (= (:instance pick) (-> (fetch context pick) :status :currentPrimary)))}])})
