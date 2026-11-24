;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.objects.delete
  "Deletes one namespaced object after a server-side dry run accepted it."
  (:require [gumshoe.deletion :as deletion]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.shell :as shell]
            [clojure.string :as str]))

(defn- dry-run
  "A server-side dry run: admission webhooks and policies judge it and nothing
   changes, so its refusal is the real delete's refusal."
  [context pick]
  (deletion/dry-run-result (apply shell/execute "kubectl" (str "--context=" context) (deletion/dry-run-args pick))))

(defn- evidence
  [context {:keys [namespace]}]
  (into {} (for [type ["pods" "jobs" "ingresses" "deployments" "statefulsets" "daemonsets" "cronjobs"]]
             [type (kubectl/get-namespaced context namespace type)])))

(mutation/book
 {:description "Deletes one namespaced object after a server-side dry run accepted it"
  :options {:pick {:desc "\"<type> <namespace>/<name>\" - interactive selection when omitted"
                   :alias :p :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get deletion/types
                  :kubectl-can-delete deletion/types}
  :select {:mode :one :label "Object" :flag :pick
           :candidates (fn [context] (deletion/picks (into {} (for [type deletion/types] [type (kubectl/get-all context type)]))))}
  :empty-message "nothing to delete"
  :confirm {:action "delete an object" :destructive? true}
  :subject {:parse deletion/parse-pick
            :invalid (str "the pick is not \"<type> <namespace>/<name>\" of " (str/join ", " deletion/types))}
  :derive (fn [{:keys [context ref]}]
            (when ref
              {:dry-run (dry-run context ref)
               :dependents (deletion/dependents ref (evidence context ref))}))
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
              :timeout 60 :interval 5
              ;; a fetch that failed returns nil like a deleted object, so
              ;; gone needs a read that answered NotFound
              :check (fn [] (or (some-> (mutation/read-subject ctx) :metadata :deletionTimestamp some?)
                                (kubectl/confirmed-gone? context (:namespace ref) (:type ref) (:name ref))))}])})
