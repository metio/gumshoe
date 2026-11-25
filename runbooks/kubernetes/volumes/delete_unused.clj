;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.volumes.delete-unused
  "Deletes a PersistentVolumeClaim that no pod uses."
  (:require [gumshoe.cleanup :as cleanup]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.select :as select]))

(defn gone-check
  [context namespace name target]
  {:description (format "PersistentVolumeClaim %s is gone" target)
   :timeout 120 :interval 10 :verifies-absence? true
   :check (fn [] (kubectl/confirmed-gone? context namespace "persistentvolumeclaims" name))})

(def selection
  {:mode :namespaced :label "PersistentVolumeClaim" :namespace-flag :namespace :name-flag :pvc
   :candidates kubectl/unused-pvc-candidates})

(mutation/book
 {:description "Deletes a PersistentVolumeClaim that no pod uses"
  :options (select/options selection)
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["persistentvolumeclaims" "pods"]
                  :kubectl-can-delete ["persistentvolumeclaims"]}
  :select selection
  :empty-message "every PersistentVolumeClaim is in use"
  :confirm {:action "delete a PersistentVolumeClaim - the data on the volume is lost"
            :destructive? true}
  :subject {:type "persistentvolumeclaims"}
  ;; a pod may have started using the claim since it was offered
  :recheck (fn [{:keys [context ref]}]
             {:pods (kubectl/get-json-result context "pods" (str "--namespace=" (:namespace ref)))})
  :refusal (fn [{:keys [ref object pods]}] (cleanup/claim-refusal ref object pods))
  :announce (fn [{:keys [target]}] (format "Delete unused PersistentVolumeClaim %s" target))
  ;; --wait=false: the pvc-protection finalizer holds the claim for as long as
  ;; a pod uses it, and the gone check is the bounded wait
  :effect (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
              (effect/plan (effect/kubectl context (str "--namespace=" namespace)
                                           "delete" "persistentvolumeclaim" name "--wait=false"))))
  :verify (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
              [(gone-check context namespace name target)]))})
