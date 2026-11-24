;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.steps.workloads
  "The steps that switch a workload off and on again: pause and resume a
   Deployment's rollouts, suspend and resume a CronJob. Each pair is one
   gumshoe.toggle declaration, {:on <spec> :off <spec>}, whose specs a
   runbook runs with its own :description and a playbook composes with other
   steps. The decisions live in gumshoe.workloads."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.toggle :as toggle]
            [gumshoe.workloads :as workloads]))

(defn- deployment-pick
  [target]
  (let [pick (workloads/parse-pick target)]
    (when (= "Deployment" (:kind pick)) pick)))

(def deployment-pausing
  "Pause a Deployment's rollouts (:on) and resume them (:off)."
  (toggle/pair
   {:spec {:options {:pick {:desc "\"Deployment <namespace>/<name>\" - interactive selection when omitted"
                            :alias :p :coerce :string}}
           :prerequisites {:installed-tools ["kubectl" "fzf"]
                           :cluster-capabilities []
                           :kubectl-can-get ["deployments" "horizontalpodautoscalers"]
                           :kubectl-can-patch ["deployments"]}
           :select {:mode :one :label "Deployment" :flag :pick}
           :subject {:parse deployment-pick :invalid "the pick is not \"Deployment <namespace>/<name>\""}
           :derive (fn [{:keys [context ref object]}]
                     (let [hpa (when object
                                 (workloads/managing-hpa (kubectl/get-namespaced context (:namespace ref) "horizontalpodautoscalers")
                                                         "Deployment" object))]
                       {:hpa (some-> hpa kubectl/name-of)}))}
    :candidates (fn [context wanted]
                  (workloads/picks [["Deployment" (kubectl/get-all context "deployments")]]
                                   (fn [_ deployment] (not= wanted (workloads/paused? deployment)))))
    :refusal workloads/pause-refusal
    :items (fn [{:keys [object hpa]} wanted] (workloads/pause-items object hpa wanted))
    :effect (fn [{:keys [context ref]} wanted]
              (effect/plan (apply effect/kubectl context (workloads/pause-args ref wanted))))
    :applied? workloads/pause-applied?
    :verify {:interval 5}
    :on {:verb "pause" :empty-message "every Deployment is paused already"
         :confirm {:action "pause a Deployment's rollouts"}
         :announce "Pause %s" :verified "%s is paused"
         :verify {:timeout 60}}
    :off {:verb "resume" :empty-message "no Deployment is paused"
          :confirm {:action "resume a Deployment and roll out the changes made while it was paused"}
          :announce "Resume %s" :verified "%s is resumed and rolled out"
          :verify {:timeout 600}}}))

(def cronjob-suspending
  "Suspend a CronJob (:on) and resume it on its schedule (:off)."
  (toggle/pair
   {:spec {:options {:namespace {:desc "The namespace of the CronJob - interactive selection when omitted"
                                 :alias :n :coerce :string}
                     :job {:desc "The name of the CronJob - interactive selection when omitted"
                           :alias :j :coerce :string}}
           :prerequisites {:installed-tools ["kubectl" "fzf"]
                           :cluster-capabilities []
                           :kubectl-can-get ["cronjobs"]
                           :kubectl-can-patch ["cronjobs"]}
           :select {:mode :namespaced :label "CronJob" :namespace-flag :namespace :name-flag :job}
           :subject {:type "cronjobs"}}
    ;; in the order the API lists them, as kubectl get shows them
    :candidates (fn [context wanted]
                  (kubectl/namespaces-names {:items (filter #(not= wanted (workloads/suspended? %))
                                                            (kubectl/items-of (kubectl/get-all context "cronjobs")))}))
    :refusal workloads/suspend-refusal
    :items (fn [{:keys [object]} wanted] (workloads/suspend-items object wanted))
    :effect (fn [{:keys [context ref]} wanted]
              (effect/plan (apply effect/kubectl context (workloads/suspend-args (:namespace ref) (:name ref) wanted))))
    :applied? (fn [cronjob wanted] (= wanted (workloads/suspended? cronjob)))
    :verify {:timeout 30 :interval 3}
    :on {:verb "suspend" :empty-message "every CronJob is suspended already"
         :confirm {:action "suspend a CronJob"}
         :announce "Suspend CronJob %s" :verified "CronJob %s is suspended"}
    :off {:verb "resume" :empty-message "no CronJob is suspended"
          :confirm {:action "resume a CronJob"}
          :announce "Resume CronJob %s" :verified "CronJob %s is resumed"}}))

(def pause-deployment "Pauses a Deployment's rollouts." (:on deployment-pausing))
(def resume-deployment "Resumes a paused Deployment and waits for its rollout." (:off deployment-pausing))
(def suspend-cronjob "Suspends a CronJob." (:on cronjob-suspending))
(def resume-cronjob "Resumes a suspended CronJob." (:off cronjob-suspending))
