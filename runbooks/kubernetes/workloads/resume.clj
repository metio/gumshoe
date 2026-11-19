;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.workloads.resume
  "Resumes a paused Deployment and waits for the rollout it releases."
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
                   (fn [_ deployment] (workloads/paused? deployment))))

(defn- deployment-pick
  [target]
  (let [pick (workloads/parse-pick target)]
    (when (= "Deployment" (:kind pick)) pick)))

(mutation/book
 {:description "Resumes a paused Deployment and waits for the rollout it releases"
  :options {:pick {:desc "\"Deployment <namespace>/<name>\" - interactive selection when omitted"
                   :alias :p :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["deployments" "horizontalpodautoscalers"]
                  :kubectl-can-patch ["deployments"]}
  :select {:mode :one :label "Deployment" :flag :pick :candidates candidates}
  :empty-message "no Deployment is paused"
  :confirm {:action "resume a Deployment and roll out the changes made while it was paused"}
  :derive (fn [{:keys [context target]}]
            (let [pick (deployment-pick target)
                  deployment (when pick (fetch context pick))
                  hpa (when deployment
                        (workloads/managing-hpa (kubectl/get-namespaced context (:namespace pick) "horizontalpodautoscalers")
                                                "Deployment" deployment))]
              {:pick pick :deployment deployment :hpa (some-> hpa kubectl/name-of)}))
  :items (fn [{:keys [deployment hpa target]}]
           (if deployment
             (workloads/pause-items deployment hpa false)
             [(str "resume " target)]))
  :pin :pick
  :precondition (fn [{:keys [context pick]}]
                  (let [why (if pick
                              (workloads/pause-refusal (fetch context pick) false)
                              "the pick is not \"Deployment <namespace>/<name>\"")]
                    (when why (println (str "refusing: " why)))
                    (nil? why)))
  :announce (fn [{:keys [target]}] (format "Resume %s" target))
  :effect (fn [{:keys [context pick]}]
            (effect/plan (apply effect/kubectl context (workloads/pause-args pick false))))
  :verify (fn [{:keys [context pick target]}]
            [{:description (format "%s is resumed and rolled out" target)
              :timeout 600 :interval 5
              :check (fn [] (workloads/pause-applied? (fetch context pick) false))}])})
