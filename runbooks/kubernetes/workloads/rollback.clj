;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.workloads.rollback
  "Rolls a Deployment back to its previous revision, or to a named one."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.workloads :as workloads]))

(defn- fetch
  [context {:keys [namespace name]}]
  (kubectl/get-namespaced-resource context namespace "deployments" name))

(defn- replicasets
  [context {:keys [namespace]}]
  (kubectl/get-namespaced context namespace "replicasets"))

(defn- candidates
  [context]
  (let [rs (kubectl/get-all context "replicasets")]
    (workloads/picks [["Deployment" (kubectl/get-all context "deployments")]]
                     (fn [_ deployment] (nil? (workloads/rollback-refusal deployment rs nil))))))

(defn- deployment-pick
  [target]
  (let [pick (workloads/parse-pick target)]
    (when (= "Deployment" (:kind pick)) pick)))

(mutation/book
 {:description "Rolls a Deployment back to its previous revision, or to a named one"
  :options {:pick {:desc "\"Deployment <namespace>/<name>\" - interactive selection when omitted"
                   :alias :p :coerce :string}
            :to-revision {:desc "The revision to go back to - the previous one when omitted" :coerce :long}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["deployments" "replicasets"]
                  :kubectl-can-patch ["deployments"]}
  :select {:mode :one :label "Deployment" :flag :pick :candidates candidates}
  :empty-message "no Deployment has an earlier revision to go back to"
  :confirm {:action "roll a Deployment back" :destructive? true}
  :derive (fn [{:keys [context target opts]}]
            (let [pick (deployment-pick target)
                  deployment (when pick (fetch context pick))
                  rs (when pick (replicasets context pick))
                  to (:to-revision opts)]
              {:pick pick :deployment deployment :replicasets rs :to to
               :target-revision (when deployment (workloads/rollback-target deployment rs to))}))
  :items (fn [{:keys [deployment replicasets to target-revision target]}]
           (if target-revision
             (workloads/rollback-items deployment replicasets to)
             [(format "roll %s back" target)]))
  :pin :pick
  :precondition (fn [{:keys [context pick to]}]
                  (let [why (if pick
                              (workloads/rollback-refusal (fetch context pick) (replicasets context pick) to)
                              "the pick is not \"Deployment <namespace>/<name>\"")]
                    (when why (println (str "refusing: " why)))
                    (nil? why)))
  :announce (fn [{:keys [target target-revision]}] (format "Roll %s back to revision %s" target (:revision target-revision)))
  :effect (fn [{:keys [context pick target-revision]}]
            (if target-revision
              (effect/plan (apply effect/kubectl context (workloads/rollback-args pick (:revision target-revision))))
              (effect/plan (effect/note "nothing to roll back to - the precondition says why"))))
  :verify (fn [{:keys [context pick target target-revision]}]
            [{:description (format "%s runs revision %s's images and rolled out" target (:revision target-revision))
              :timeout 600 :interval 10
              :check (fn [] (workloads/rolled-back? (fetch context pick) target-revision))}])})
