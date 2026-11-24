;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.netshoot.node
  "Starts an interactive netshoot debug session on a node."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(mutation/book
 {:description "Starts an interactive netshoot debug session on a node"
  :options {:node {:desc "The node to debug - interactive selection when omitted"
                   :alias :n
                   :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "kubectl-netshoot" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["nodes"]
                  :kubectl-can-create ["pods"]}
  :announce? false
  :select {:mode :one
           :label "Node"
           :flag :node
           :candidates (fn [context] (kubectl/names-of (kubectl/get-all context "nodes")))}
  :confirm {:action "start a privileged netshoot debug pod on a node"}
  :effect (fn [{:keys [context target]}]
            (effect/plan (effect/kubectl-plugin context "netshoot" "debug" (str "node/" target))))})
