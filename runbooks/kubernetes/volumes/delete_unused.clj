;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.volumes.delete-unused
  "Deletes a PersistentVolumeClaim that no pod uses."
  (:require [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]))

(defn gone-check
  [context namespace name target]
  {:description (format "PersistentVolumeClaim %s is gone" target)
   :timeout 120 :interval 10
   :check (fn [] (kubectl/confirmed-gone? context namespace "persistentvolumeclaims" name))})

(mutation/book
 {:description "Deletes a PersistentVolumeClaim that no pod uses"
  :options {:namespace {:desc "The namespace of the PersistentVolumeClaim - interactive selection when omitted"
                        :alias :n :coerce :string}
            :pvc {:desc "The name of the PersistentVolumeClaim - interactive selection when omitted"
                  :alias :p :coerce :string}}
  :prerequisites {:installed-tools ["kubectl" "fzf"]
                  :cluster-capabilities []
                  :kubectl-can-get ["persistentvolumeclaims" "pods"]
                  :kubectl-can-delete ["persistentvolumeclaims"]}
  :select {:mode :namespaced :label "PersistentVolumeClaim" :namespace-flag :namespace :name-flag :pvc
           :candidates kubectl/unused-pvc-candidates}
  :empty-message "every PersistentVolumeClaim is in use"
  :confirm {:action "delete a PersistentVolumeClaim - the data on the volume is lost"
            :destructive? true}
  ;; a pod may have started using the claim since it was offered
  :precondition (fn [{:keys [context target]}]
                  (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
                    (kubectl/pvc-unused? context namespace name)))
  :announce (fn [{:keys [target]}] (format "Delete unused PersistentVolumeClaim %s" target))
  :effect (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
              (effect/plan (effect/kubectl context (str "--namespace=" namespace)
                                           "delete" "persistentvolumeclaim" name))))
  :verify (fn [{:keys [context target]}]
            (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
              [(gone-check context namespace name target)]))})
