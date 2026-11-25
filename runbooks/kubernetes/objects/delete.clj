;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.objects.delete
  "Deletes one namespaced object after a server-side dry run accepted it."
  (:require [gumshoe.deletion :as deletion]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]
            [clojure.string :as str]))

(def selection
  {:mode :one :label "Object" :flag :pick :candidates deletion/candidates})

(mutation/book
 {:description "Deletes one namespaced object after a server-side dry run accepted it"
  :options (select/options selection {:pick {:desc "\"<type> <namespace>/<name>\" - interactive selection when omitted"}})
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get deletion/types
                  :kubectl-can-delete deletion/types}
  :select selection
  :empty-message "nothing to delete"
  :confirm {:action "delete an object" :destructive? true}
  :subject {:parse deletion/parse-pick
            :invalid (str "the pick is not \"<type> <namespace>/<name>\" of " (str/join ", " deletion/types))}
  :derive (fn [{:keys [context ref]}]
            (when ref
              {:dry-run (deletion/dry-run context ref)
               :dependents (deletion/read-dependents context ref)}))
  :items (fn [{:keys [target dry-run dependents]}]
           (if dry-run (deletion/deletion-items target dry-run dependents) [(str "delete " target)]))
  :refusal (fn [{:keys [object dry-run]}]
             (cond
               (not (:ok? dry-run)) (str "the dry run was refused: " (:message dry-run))
               (nil? object) "the object no longer exists"
               :else nil))
  :announce (fn [{:keys [target]}] (format "Delete %s" target))
  :effect (fn [{:keys [context ref]}]
            (effect/plan (apply effect/kubectl context (deletion/delete-args (or ref {})))))
  :verify (fn [{:keys [context ref target] :as ctx}]
            [{:description (format "%s is gone or terminating" target)
              :timeout 60 :interval 5 :verifies-absence? true
              ;; a fetch that failed returns nil like a deleted object, so
              ;; gone needs a read that answered NotFound
              :check (fn [] (or (some-> (mutation/read-subject ctx) :metadata :deletionTimestamp some?)
                                (kubectl/confirmed-gone? context (:namespace ref) (:type ref) (:name ref))))}])})
