;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.cronjobs.resume
  "Resumes a suspended CronJob on its schedule."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.workloads :as workloads]))

(defn- candidates
  [context]
  (kubectl/namespaces-names {:items (filter #(workloads/suspended? %) (kubectl/items-of (kubectl/get-all context "cronjobs")))}))

(mutation/book
 {:description "Resumes a suspended CronJob on its schedule"
  :options {:namespace {:desc "The namespace of the CronJob - interactive selection when omitted"
                        :alias :n :coerce :string}
            :job {:desc "The name of the CronJob - interactive selection when omitted"
                  :alias :j :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["cronjobs"]
                  :kubectl-can-patch ["cronjobs"]}
  :select {:mode :namespaced :label "CronJob" :namespace-flag :namespace :name-flag :job
           :candidates candidates}
  :empty-message "no CronJob is suspended"
  :confirm {:action "resume a CronJob"}
  :subject {:type "cronjobs"}
  :items (fn [{:keys [object target]}]
           (if object (workloads/suspend-items object false) [(str "resume " target)]))
  :refusal (fn [{:keys [object]}] (workloads/suspend-refusal object false))
  :announce (fn [{:keys [target]}] (format "Resume CronJob %s" target))
  :effect (fn [{:keys [context ref]}]
            (effect/plan (apply effect/kubectl context (workloads/suspend-args (:namespace ref) (:name ref) false))))
  :verify (fn [{:keys [target] :as ctx}]
            [{:description (format "CronJob %s is resumed" target)
              :timeout 30 :interval 3
              :check (fn [] (= false (workloads/suspended? (mutation/read-subject ctx))))}])})
