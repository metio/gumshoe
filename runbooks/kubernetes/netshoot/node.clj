;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.netshoot.node
  "Starts an interactive netshoot debug session on a node."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]))

(def selection
  {:mode :one
   :label "Node"
   :flag :node
   :candidates (fn [context] (kubectl/names-of (kubectl/get-all context "nodes")))})

(mutation/book
 {:description "Starts an interactive netshoot debug session on a node"
  :options (select/options selection {:node {:desc "The node to debug - interactive selection when omitted"}})
  :prerequisites {:installed-tools ["kubectl" "kubectl-netshoot" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["nodes"]
                  :kubectl-can-create ["pods"]}
  :announce? false
  :no-verify "an interactive debug session: the operator works in it until they leave, and what they find there is the result, so nothing is left to check after it ends"
  :select selection
  :confirm {:action "start a privileged netshoot debug pod on a node" :destructive? false}
  :effect (fn [{:keys [context target]}]
            (effect/plan (effect/kubectl-plugin context "netshoot" "debug" (str "node/" target))))})
