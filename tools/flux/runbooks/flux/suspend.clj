;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.flux.suspend
  "Suspends a Flux HelmRelease, Kustomization, GitRepository or OCIRepository."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.tools.flux :as flux]))

(defn- fetch
  [context {:keys [type namespace name]}]
  (kubectl/get-namespaced-resource context namespace type name))

(defn- candidates
  [context]
  (flux/delivery-picks (into {} (for [[kind type] flux/suspendable] [kind (kubectl/get-all context type)])) true))

(mutation/book
 {:description "Suspends a Flux HelmRelease, Kustomization, GitRepository or OCIRepository"
  :options {:pick {:desc "\"<Kind> <namespace>/<name>\" - interactive selection when omitted"
                   :alias :p :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get (vec (vals flux/suspendable))
                  :kubectl-can-patch (vec (vals flux/suspendable))}
  :select {:mode :one :label "Suspend" :flag :pick :candidates candidates}
  :empty-message "every Flux object is suspended already"
  :confirm {:action "suspend a Flux object"}
  :derive (fn [{:keys [context target]}]
            (let [pick (flux/parse-delivery-pick target)]
              {:pick pick :resource (when pick (fetch context pick))
               ;; one timestamp, shared by the plan and its announcement
               :timestamp (str (.truncatedTo (java.time.Instant/now) java.time.temporal.ChronoUnit/SECONDS))}))
  :items (fn [{:keys [pick resource target]}]
           (if resource (flux/suspend-items (:kind pick) resource true) [(str "suspend " target)]))
  :precondition (fn [{:keys [context pick]}]
                  (let [why (if pick
                              (flux/suspend-refusal (fetch context pick) true)
                              "the pick is not \"<Kind> <namespace>/<name>\" of a HelmRelease, Kustomization, GitRepository or OCIRepository")]
                    (when why (println (str "refusing: " why)))
                    (nil? why)))
  :announce (fn [{:keys [target]}] (format "Suspend %s" target))
  :effect (fn [{:keys [context pick timestamp]}]
            (flux/suspend-effect context (or pick {}) true timestamp))
  :verify (fn [{:keys [context pick target]}]
            [{:description (format "%s is suspended" target)
              :timeout 30 :interval 3
              :check (fn [] (= true (true? (-> (fetch context pick) :spec :suspend))))}])})
