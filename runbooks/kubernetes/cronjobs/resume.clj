;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.cronjobs.resume
  "Resumes a suspended CronJob on its schedule."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.workloads :as workloads]))

(defn- fetch
  [context target]
  (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
    (kubectl/get-namespaced-resource context namespace "cronjobs" name)))

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
  :derive (fn [{:keys [context target]}] {:cronjob (fetch context target)})
  :items (fn [{:keys [cronjob target]}]
           (if cronjob (workloads/suspend-items cronjob false) [(str "resume " target)]))
  :precondition (fn [{:keys [context target]}]
                  (let [why (workloads/suspend-refusal (fetch context target) false)]
                    (when why (println (str "refusing: " why)))
                    (nil? why)))
  :announce (fn [{:keys [target]}] (format "Resume CronJob %s" target))
  :effect (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
              (effect/plan (apply effect/kubectl context (workloads/suspend-args namespace name false)))))
  :verify (fn [{:keys [context target]}]
            [{:description (format "CronJob %s is resumed" target)
              :timeout 30 :interval 3
              :check (fn [] (= false (workloads/suspended? (fetch context target))))}])})
