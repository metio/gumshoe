;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.cronjobs.start
  "Starts a Job from a CronJob right now instead of waiting for its schedule."
  (:require [gumshoe.conditions :as conditions]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]))

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
                   (not (conditions/holds? job "Failed")))))})

(def selection
  {:mode :namespaced :label "CronJob" :namespace-flag :namespace :name-flag :job
   :candidates cronjobs})

(mutation/book
 {:description "Starts a Job from a CronJob right now instead of waiting for its schedule"
  :options (select/options selection)
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["cronjobs"]
                  :kubectl-can-create ["jobs"]}
  :select selection
  :confirm {:action "start a Job from a CronJob outside its schedule" :destructive? false}
  ;; the random job name is computed once here and shared by effect and verify
  :derive (fn [{:keys [target]}]
            (let [{:keys [name]} (kubectl/namespace-name-map target)]
              {:cronjob name :job-name (str name "-" (random-suffix))}))
  :items (fn [{:keys [target job-name]}]
           (let [{:keys [namespace]} (kubectl/namespace-name-map target)]
             [(format "create Job %s/%s from CronJob %s" namespace job-name target)]))
  :announce (fn [{:keys [target]}] (format "Start CronJob %s" target))
  :effect (fn [{:keys [context target cronjob job-name]}]
            (let [{:keys [namespace]} (kubectl/namespace-name-map target)]
              (effect/plan (effect/kubectl context (str "--namespace=" namespace)
                                           "create" "job" job-name "--from" (str "cronjob/" cronjob)))))
  :verify (fn [{:keys [context target job-name]}]
            (let [{:keys [namespace]} (kubectl/namespace-name-map target)]
              [(not-failed-check context namespace job-name)]))})
