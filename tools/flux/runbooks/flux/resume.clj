;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.flux.resume
  "Resumes a suspended Flux HelmRelease, Kustomization, GitRepository or OCIRepository."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.tools.flux :as flux]))

(defn- candidates
  [context]
  (flux/delivery-picks (into {} (for [[kind type] flux/suspendable] [kind (kubectl/get-all context type)])) false))

(mutation/book
 {:description "Resumes a suspended Flux HelmRelease, Kustomization, GitRepository or OCIRepository"
  :options {:pick {:desc "\"<Kind> <namespace>/<name>\" - interactive selection when omitted"
                   :alias :p :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get (vec (vals flux/suspendable))
                  :kubectl-can-patch (vec (vals flux/suspendable))}
  :select {:mode :one :label "Resume" :flag :pick :candidates candidates}
  :empty-message "no Flux object is suspended"
  :confirm {:action "resume a Flux object"}
  :subject {:parse flux/parse-delivery-pick
            :invalid "the pick is not \"<Kind> <namespace>/<name>\" of a HelmRelease, Kustomization, GitRepository or OCIRepository"}
  ;; one timestamp, shared by the plan and its announcement
  :derive (fn [_] {:timestamp (str (.truncatedTo (java.time.Instant/now) java.time.temporal.ChronoUnit/SECONDS))})
  :items (fn [{:keys [ref object target]}]
           (if object (flux/suspend-items (:kind ref) object false) [(str "resume " target)]))
  :refusal (fn [{:keys [object]}] (flux/suspend-refusal object false))
  :announce (fn [{:keys [target]}] (format "Resume %s" target))
  :effect (fn [{:keys [context ref timestamp]}]
            (flux/suspend-effect context (or ref {}) false timestamp))
  :verify (fn [{:keys [target] :as ctx}]
            [{:description (format "%s is no longer suspended" target)
              :timeout 30 :interval 3
              :check (fn [] (= false (true? (-> (mutation/read-subject ctx) :spec :suspend))))}])})
