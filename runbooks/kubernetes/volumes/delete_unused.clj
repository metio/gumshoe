;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.volumes.delete-unused
  "Deletes a PersistentVolumeClaim nothing holds: no pod uses it, no
   StatefulSet keeps it for one of its ordinals, it has no owner, and no
   workload names it in its pod template, even one scaled to 0 or suspended
   (see gumshoe.claims)."
  (:require [gumshoe.claims :as claims]
            [gumshoe.cleanup :as cleanup]
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
   :candidates claims/unused-candidates})

(mutation/book
 {:description "Deletes a PersistentVolumeClaim nothing uses or keeps"
  :options (select/options selection)
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get (into ["persistentvolumeclaims"] claims/holder-types)
                  :kubectl-can-delete ["persistentvolumeclaims"]}
  :select selection
  :empty-message "every PersistentVolumeClaim is used by a pod or kept by a workload or owner"
  :confirm {:action "delete a PersistentVolumeClaim - the data on the volume is lost"
            :destructive? true}
  :subject {:type "persistentvolumeclaims"}
  ;; a pod or a workload may have started using the claim since it was offered
  :recheck (fn [{:keys [context ref]}]
             {:holders (claims/read-holders context (:namespace ref))})
  :refusal (fn [{:keys [ref object holders]}] (cleanup/claim-refusal ref object holders))
  :derive (fn [{:keys [context object]}]
            {:volume-line (when object (cleanup/claim-volume-line object (cleanup/read-claim-volume context object)))})
  :items (fn [{:keys [target volume-line]}] (cond-> [(str "delete " target)] volume-line (conj volume-line)))
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
