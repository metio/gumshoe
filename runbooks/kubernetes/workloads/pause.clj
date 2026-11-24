;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.workloads.pause
  "Pauses a Deployment's rollouts, so pod template changes wait until it is resumed."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.workloads :as workloads]))

(defn- candidates
  [context]
  (workloads/picks [["Deployment" (kubectl/get-all context "deployments")]]
                   (fn [_ deployment] (not (workloads/paused? deployment)))))

(defn- deployment-pick
  [target]
  (let [pick (workloads/parse-pick target)]
    (when (= "Deployment" (:kind pick)) pick)))

(mutation/book
 {:description "Pauses a Deployment's rollouts, so pod template changes wait until it is resumed"
  :options {:pick {:desc "\"Deployment <namespace>/<name>\" - interactive selection when omitted"
                   :alias :p :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["deployments" "horizontalpodautoscalers"]
                  :kubectl-can-patch ["deployments"]}
  :select {:mode :one :label "Deployment" :flag :pick :candidates candidates}
  :empty-message "every Deployment is paused already"
  :confirm {:action "pause a Deployment's rollouts"}
  :subject {:parse deployment-pick :invalid "the pick is not \"Deployment <namespace>/<name>\""}
  :derive (fn [{:keys [context ref object]}]
            (let [hpa (when object
                        (workloads/managing-hpa (kubectl/get-namespaced context (:namespace ref) "horizontalpodautoscalers")
                                                "Deployment" object))]
              {:hpa (some-> hpa kubectl/name-of)}))
  :items (fn [{:keys [object hpa target]}]
           (if object
             (workloads/pause-items object hpa true)
             [(str "pause " target)]))
  :refusal (fn [{:keys [object]}] (workloads/pause-refusal object true))
  :announce (fn [{:keys [target]}] (format "Pause %s" target))
  :effect (fn [{:keys [context ref]}]
            (effect/plan (apply effect/kubectl context (workloads/pause-args ref true))))
  :verify (fn [{:keys [target] :as ctx}]
            [{:description (format "%s is paused" target)
              :timeout 60 :interval 5
              :check (fn [] (workloads/pause-applied? (mutation/read-subject ctx) true))}])})
