;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.cronjobs.start
  "Starts a Job from a CronJob right now instead of waiting for its schedule."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn cronjobs
  [context]
  (kubectl/namespaces-names (kubectl/get-all context "cronjobs")))

(defn random-suffix
  []
  (apply str (repeatedly 5 #(rand-nth "abcdefghijklmnopqrstuvwxyz"))))

(defn not-failed-check
  [context namespace job-name]
  {:description (format "job %s/%s exists and has not failed" namespace job-name)
   :timeout 30
   :check (fn []
            (let [job (kubectl/get-namespaced-resource context namespace "jobs" job-name)]
              (and (some? job)
                   (not-any? #(and (= "Failed" (:type %)) (= "True" (:status %)))
                             (-> job :status :conditions)))))})

(mutation/book
 {:description "Starts a Job from a CronJob right now instead of waiting for its schedule"
  :options {:namespace {:desc "The namespace of the CronJob - interactive selection when omitted"
                        :alias :n :coerce :string}
            :job {:desc "The name of the CronJob - interactive selection when omitted"
                  :alias :j :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["cronjobs"]
                  :kubectl-can-create ["jobs"]}
  :select {:mode :namespaced :label "CronJob" :namespace-flag :namespace :name-flag :job
           :candidates cronjobs}
  :confirm {:action "start a Job from a CronJob outside its schedule"}
  ;; the random job name is computed once here and shared by effect and verify
  :derive (fn [{:keys [target]}]
            (let [{:keys [name]} (kubectl/split-namespace-name target)]
              {:cronjob name :job-name (str name "-" (random-suffix))}))
  :announce (fn [{:keys [target]}] (format "Start CronJob %s" target))
  :effect (fn [{:keys [context target cronjob job-name]}]
            (let [{:keys [namespace]} (kubectl/split-namespace-name target)]
              (effect/plan (effect/kubectl context (str "--namespace=" namespace)
                                           "create" "job" job-name "--from" (str "cronjob/" cronjob)))))
  :verify (fn [{:keys [context target job-name]}]
            (let [{:keys [namespace]} (kubectl/split-namespace-name target)]
              [(not-failed-check context namespace job-name)]))})
