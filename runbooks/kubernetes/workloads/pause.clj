;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.workloads.pause
  "Pauses a Deployment's rollouts, so pod template changes wait until it is resumed."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.workloads :as workloads]))

(defn- fetch
  [context {:keys [namespace name]}]
  (kubectl/get-namespaced-resource context namespace "deployments" name))

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
  :derive (fn [{:keys [context target]}]
            (let [pick (deployment-pick target)
                  deployment (when pick (fetch context pick))
                  hpa (when deployment
                        (workloads/managing-hpa (kubectl/get-namespaced context (:namespace pick) "horizontalpodautoscalers")
                                                "Deployment" deployment))]
              {:pick pick :deployment deployment :hpa (some-> hpa kubectl/name-of)}))
  :items (fn [{:keys [deployment hpa target]}]
           (if deployment
             (workloads/pause-items deployment hpa true)
             [(str "pause " target)]))
  :pin :pick
  :precondition (fn [{:keys [context pick]}]
                  (let [why (if pick
                              (workloads/pause-refusal (fetch context pick) true)
                              "the pick is not \"Deployment <namespace>/<name>\"")]
                    (when why (println (str "refusing: " why)))
                    (nil? why)))
  :announce (fn [{:keys [target]}] (format "Pause %s" target))
  :effect (fn [{:keys [context pick]}]
            (effect/plan (apply effect/kubectl context (workloads/pause-args pick true))))
  :verify (fn [{:keys [context pick target]}]
            [{:description (format "%s is paused" target)
              :timeout 60 :interval 5
              :check (fn [] (workloads/pause-applied? (fetch context pick) true))}])})
