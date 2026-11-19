;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.cronjobs.suspend
  "Suspends a CronJob so no new Job starts."
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
  (kubectl/namespaces-names {:items (filter #(not (workloads/suspended? %)) (kubectl/items-of (kubectl/get-all context "cronjobs")))}))

(mutation/book
 {:description "Suspends a CronJob so no new Job starts"
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
  :empty-message "every CronJob is suspended already"
  :confirm {:action "suspend a CronJob"}
  :derive (fn [{:keys [context target]}] {:cronjob (fetch context target)})
  :items (fn [{:keys [cronjob target]}]
           (if cronjob (workloads/suspend-items cronjob true) [(str "suspend " target)]))
  :pin (fn [{:keys [target]}] (assoc (kubectl/split-namespace-name target) :type "cronjobs"))
  :precondition (fn [{:keys [context target]}]
                  (let [why (workloads/suspend-refusal (fetch context target) true)]
                    (when why (println (str "refusing: " why)))
                    (nil? why)))
  :announce (fn [{:keys [target]}] (format "Suspend CronJob %s" target))
  :effect (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
              (effect/plan (apply effect/kubectl context (workloads/suspend-args namespace name true)))))
  :verify (fn [{:keys [context target]}]
            [{:description (format "CronJob %s is suspended" target)
              :timeout 30 :interval 3
              :check (fn [] (= true (workloads/suspended? (fetch context target))))}])})
