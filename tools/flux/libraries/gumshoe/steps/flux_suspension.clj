;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.steps.flux-suspension
  "Suspend a Flux HelmRelease, Kustomization, GitRepository or OCIRepository
   and resume it, as one gumshoe.toggle declaration, {:on <spec> :off <spec>}.
   The decisions live in gumshoe.tools.flux."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.select :as select]
            [gumshoe.toggle :as toggle]
            [gumshoe.tools.flux :as flux]))

(def ^:private delivery-select {:mode :one :flag :pick})

(def suspending
  "Suspend a Flux object (:on) and resume it (:off)."
  (toggle/pair
   {:spec {:options (select/options delivery-select
                                    {:pick {:desc "\"<Kind> <namespace>/<name>\" - interactive selection when omitted"}})
           :prerequisites {:installed-tools ["kubectl" "fzf"]
                           :cluster-capabilities []
                           :kubectl-can-get (vec (vals flux/suspendable))
                           :kubectl-can-patch (vec (vals flux/suspendable))}
           :select delivery-select
           :subject {:parse flux/parse-delivery-pick
                     :invalid "the pick is not \"<Kind> <namespace>/<name>\" of a HelmRelease, Kustomization, GitRepository or OCIRepository"}
           ;; one timestamp, shared by the plan and its announcement
           :derive (fn [_] {:timestamp (str (.truncatedTo (java.time.Instant/now) java.time.temporal.ChronoUnit/SECONDS))})}
    :candidates (fn [context wanted]
                  (flux/delivery-picks (into {} (for [[kind type] flux/suspendable] [kind (kubectl/get-all context type)])) wanted))
    :refusal flux/suspend-refusal
    :items (fn [{:keys [ref object]} wanted] (flux/suspend-items (:kind ref) object wanted))
    :effect (fn [{:keys [context ref timestamp]} wanted] (flux/suspend-effect context (or ref {}) wanted timestamp))
    :applied? (fn [object wanted] (= wanted (true? (-> object :spec :suspend))))
    :verify {:timeout 30 :interval 3}
    :on {:verb "suspend" :select {:label "Suspend"}
         :empty-message "every Flux object is suspended already"
         :confirm {:action "suspend a Flux object" :destructive? false}
         :announce "Suspend %s" :verified "%s is suspended"}
    :off {:verb "resume" :select {:label "Resume"}
          :empty-message "no Flux object is suspended"
          :confirm {:action "resume a Flux object" :destructive? false}
          :announce "Resume %s" :verified "%s is no longer suspended"}}))

(def suspend "Suspends a Flux object." (:on suspending))
(def resume "Resumes a suspended Flux object." (:off suspending))
