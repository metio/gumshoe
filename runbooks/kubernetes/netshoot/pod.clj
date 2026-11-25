;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.netshoot.pod
  "Starts an interactive netshoot debug session next to a running pod."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(mutation/book
 {:description "Starts an interactive netshoot debug session next to a running pod"
  :options {:namespace {:desc "The namespace of the pod - interactive selection when omitted"
                        :alias :n
                        :coerce :string}
            :pod {:desc "The pod to debug - interactive selection when omitted"
                  :alias :p
                  :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "kubectl-netshoot" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["pods"]}
  :announce? false
  :no-verify "an interactive debug session: the operator works in it until they leave, and what they find there is the result, so nothing is left to check after it ends"
  :select {:mode :namespaced
           :label "Pod"
           :namespace-flag :namespace
           :name-flag :pod
           :candidates (fn [context] (kubectl/namespaces-names (kubectl/get-all context "pods")))}
  :empty-message "no pod is running to debug"
  :confirm {:action "attach a netshoot debug container to a running pod" :destructive? false}
  :effect (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
              (effect/plan (effect/kubectl-plugin context "netshoot" "debug" name "--namespace" namespace))))})
