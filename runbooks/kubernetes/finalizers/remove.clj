;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.finalizers.remove
  "Removes one controller finalizer from an object stuck terminating."
  (:require [gumshoe.detectives.terminating :as terminating]
            [gumshoe.effect :as effect]
            [gumshoe.finalizers :as finalizers]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn- candidates
  [context]
  (finalizers/picks (for [[type] terminating/types] [type (kubectl/get-all context type)])))

(mutation/book
 {:description "Removes one controller finalizer from an object stuck terminating"
  :options {:pick {:desc "\"<type> <namespace>/<name> <finalizer>\" - interactive selection when omitted"
                   :alias :p :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get (mapv first terminating/types)
                  :kubectl-can-patch (mapv first terminating/types)}
  :select {:mode :one :label "Finalizer" :flag :pick :candidates candidates}
  :empty-message "no object being deleted carries a controller finalizer"
  :confirm {:action (str "remove a finalizer - the cleanup its controller would do (deleting cloud resources, "
                         "backups, DNS records) is skipped for good; do this only when that controller is gone")
            :destructive? true}
  :subject {:parse finalizers/parse-pick :invalid "the pick is not \"<type> <object> <finalizer>\""}
  :refusal (fn [{:keys [ref object]}] (finalizers/refusal object ref))
  :announce (fn [{:keys [target]}] (format "Remove finalizer from %s" target))
  :effect (fn [{:keys [context ref object]}]
            (effect/plan (apply effect/kubectl context (finalizers/patch-args object ref))))
  :verify (fn [{:keys [context ref target]}]
            [{:description (format "%s no longer waits on that finalizer" target)
              :timeout 60 :interval 5
              :check #(finalizers/released? context ref)}])})
