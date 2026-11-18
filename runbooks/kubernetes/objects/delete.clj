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

(defn- fetch
  [context {:keys [type namespace name]}]
  (kubectl/get-namespaced-resource context namespace type name))

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
  :derive (fn [{:keys [context target]}]
            (let [pick (deletion/parse-pick target)]
              (when pick
                {:pick pick
                 :dry-run (dry-run context pick)
                 :dependents (deletion/dependents pick (evidence context pick))})))
  :items (fn [{:keys [target dry-run dependents]}]
           (if dry-run (deletion/deletion-items target dry-run dependents) [(str "delete " target)]))
  :precondition (fn [{:keys [context pick dry-run]}]
                  (let [why (cond
                              (nil? pick) (str "the pick is not \"<type> <namespace>/<name>\" of " (str/join ", " deletion/types))
                              (not (:ok? dry-run)) (str "the dry run was refused: " (:message dry-run))
                              (nil? (fetch context pick)) "the object no longer exists"
                              :else nil)]
                    (when why (println (str "refusing: " why)))
                    (nil? why)))
  :announce (fn [{:keys [target]}] (format "Delete %s" target))
  :effect (fn [{:keys [context pick]}]
            (effect/plan (apply effect/kubectl context (deletion/delete-args (or pick {})))))
  :verify (fn [{:keys [context pick target]}]
            [{:description (format "%s is gone or terminating" target)
              :timeout 60 :interval 5
              :check (fn [] (deletion/deleted? (fetch context pick)))}])})
