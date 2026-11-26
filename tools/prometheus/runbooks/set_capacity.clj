;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.prometheus.set-capacity
  "Grows the storage of a prometheus-operator managed Prometheus. The full
   resize plan is validated before anything happens (expandable storage
   classes, no shrinking), the change runs as an effect plan (so --dry-run
   shows it), the Prometheus is pinned between the confirmation and the
   effect, a failure part-way prints what it left behind, and afterwards
   every volume is verified to have the new capacity, the operator to have
   recreated the StatefulSet with the new template and every replica ready,
   and every pod to run under that StatefulSet again.

   Steps: pause the Prometheus and patch its volumeClaimTemplate, resize the
   existing PVCs, delete the StatefulSet with orphaned pods, unpause so the
   operator recreates the StatefulSet against the new template."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.outcome :as outcome]
            [gumshoe.prometheus :as prometheus]
            [gumshoe.runbook :as runbook]
            [gumshoe.select :as select]
            [gumshoe.stdout :as stdout]
            [gumshoe.storage :as storage]))

(def prometheus-type prometheus/prometheus-type)

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get [prometheus-type "pods" "persistentvolumeclaims" "storageclasses" "statefulsets"]
   :kubectl-can-patch [prometheus-type "persistentvolumeclaims"]
   :kubectl-can-delete ["statefulsets"]})

(def prometheus-select
  {:mode :namespaced
   :label "Prometheus"
   :candidates (fn [context] (kubectl/namespaces-names (kubectl/get-all context prometheus-type)))
   :namespace-flag :namespace
   :name-flag :prometheus
   :empty-message "there is no Prometheus to resize"
   :on-empty :fail})

(def options
  (select/options prometheus-select
                  {:prometheus {:desc "The Prometheus instance to resize - interactive selection when omitted"}
                   :capacity {:desc "The new storage capacity (bare numbers mean Gi)"
                              :alias :c
                              :require true
                              :coerce :string}}))

(defn- planned
  "{:values {:plan :problems}} for growing the Prometheus resource to
   capacity, or {:unread <why>}. The reads are strict, as mutation/derived
   reads, so a read that did not answer is never planned from as an absent
   object."
  [context cluster namespace name capacity]
  (mutation/derived
   (fn [_]
     (let [resource (kubectl/get-namespaced-resource context namespace prometheus-type name)
           plan (prometheus/resize-plan {:pods (prometheus/pods-of context namespace name)
                                         :volume-name (prometheus/volume-name-of resource)
                                         :pvcs (kubectl/get-all context "persistentvolumeclaims")
                                         :storage-classes (kubectl/get-all context "storageclasses")
                                         :capacity capacity})]
       ;; the storage-provider preflights (a ceph pool's free capacity) fail
       ;; closed, as they do for the StatefulSet enlarge_volumes book: a
       ;; resize that would overfill the backend is refused here too
       {:plan plan
        :problems (into (storage/expansion-problems plan)
                        (storage/resize-preflight-problems
                         {:context context :cluster cluster :namespace namespace :plan plan}))}))
   nil))

(defn change
  "The confirmed change that grows the claims of plan to capacity. The
   Prometheus is the change's subject, pinned, so the change is refused when
   it changed between the confirmation and the effect."
  [{:keys [context cluster target capacity plan applied-before]}]
  (let [{:keys [namespace name]} (kubectl/namespace-name-map target)
        steps (prometheus/resize-steps context namespace name capacity plan)]
    {:title "resize the Prometheus storage"
     :select prometheus-select
     :subject {:type prometheus-type}
     :confirm {:action (format "resize the storage of Prometheus %s to %s - the StatefulSet is recreated" target capacity)
               :destructive? true}
     :items (fn [_] (map :pvc plan))
     :announce (fn [_] (format "Resize storage of Prometheus %s to %s" target capacity))
     :effect (fn [_] (storage/plan-of steps))
     :verify (fn [_] (prometheus/resize-checks context cluster namespace name capacity plan))
     :recovery (fn [{:keys [applied]}]
                 (prometheus/resize-recovery {:context context :target target}
                                             steps
                                             (- (or applied 0) applied-before)))}))

(defn- set-capacity
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        capacity (storage/normalize-capacity (:capacity opts))]
    (if-let [target (select/pick prometheus-select opts context)]
      (let [{:keys [namespace name]} (kubectl/namespace-name-map target)
            {{:keys [plan problems]} :values unread :unread} (planned context cluster namespace name capacity)]
        (if unread
          (do (stdout/error (format "could not read what the change rests on - %s" unread)) false)
          (do (stdout/print-section "📋 Resize plan")
              (doseq [{:keys [pvc current target]} plan]
                (stdout/err-println (format "  %s: %s -> %s" pvc current target)))
              (if (seq problems)
                (do (doseq [problem problems] (stdout/error problem))
                    false)
                (mutation/run-on! (change {:context context :cluster cluster :target target
                                           :capacity capacity :plan plan
                                           :applied-before (outcome/applied-count)})
                                  target opts announcement-data)))))
      false)))

(runbook/execute!
 {:description "Grows the storage of a prometheus-operator managed Prometheus"
  :options options
  :prerequisites prerequisites
  :action set-capacity})
