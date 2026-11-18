;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.fence
  "Fences a CloudNativePG instance, or every instance with *."
  (:require [gumshoe.cnpg.operations :as operations]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn- fetch
  [context {:keys [namespace cluster]}]
  (kubectl/get-namespaced-resource context namespace cnpg/cluster-type cluster))

(mutation/book
 {:description "Fences a CloudNativePG instance, or every instance with *"
  :options {:pick {:desc "\"<namespace>/<cluster> <instance>\" (by name, by number, or *) - interactive selection when omitted"
                   :alias :p :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get [cnpg/cluster-type]
                  :kubectl-can-patch [cnpg/cluster-type]}
  :select {:mode :one :label "Fence" :flag :pick
           :candidates (fn [context] (operations/fence-picks (kubectl/get-all context cnpg/cluster-type)))}
  :empty-message "no cluster has an instance left to fence"
  :confirm {:action "fence an instance - the operator takes it out of service and will not promote it; fencing the primary or * leaves nothing accepting writes"
            :destructive? true}
  :derive (fn [{:keys [target]}] {:pick (operations/parse-pick target)})
  :precondition (fn [{:keys [context pick]}]
                  (let [why (if pick
                              (operations/fence-refusal (fetch context pick) (:instance pick))
                              "the pick is not \"<namespace>/<cluster> <instance>\"")]
                    (when why (println (str "refusing: " why)))
                    (nil? why)))
  :announce (fn [{:keys [pick]}] (format "Fence %s in %s/%s" (:instance pick) (:namespace pick) (:cluster pick)))
  :effect (fn [{:keys [context pick]}]
            (if-let [change (operations/fence-change :fence (fetch context pick) (:instance pick))]
              (effect/plan (effect/kubectl context "--namespace" (:namespace pick) "annotate" cnpg/cluster-type
                                           (:cluster pick) "--overwrite" change))
              (effect/plan (effect/note "nothing can be written - the precondition says why"))))
  :verify (fn [{:keys [context pick]}]
            [{:description (format "%s is fenced" (:instance pick))
              :timeout 60 :interval 5
              :check (fn [] (let [cluster (fetch context pick)] (operations/fenced? cluster (:instance pick))))}])})
