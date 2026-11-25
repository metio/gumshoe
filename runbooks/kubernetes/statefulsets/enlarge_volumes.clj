;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.statefulsets.enlarge-volumes
  "Grows the volumes of any StatefulSet. A volumeClaimTemplate is immutable,
   so the PVCs are enlarged, the StatefulSet is deleted with its pods
   orphaned, and it is recreated with the new template - after which
   everything matches again. The full plan is validated first, the change
   runs as an effect plan (so --dry-run shows it), the StatefulSet is pinned
   between the confirmation and the effect, a failure part-way prints what
   it left behind, and the result is verified volume by volume."
  (:require [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.outcome :as outcome]
            [gumshoe.runbook :as runbook]
            [gumshoe.select :as select]
            [gumshoe.stdout :as stdout]
            [gumshoe.storage :as storage]))

(def options
  {:namespace {:desc "The namespace of the StatefulSet - interactive selection when omitted"
               :alias :n
               :coerce :string}
   :statefulset {:desc "The StatefulSet to enlarge - interactive selection when omitted"
                 :alias :s
                 :coerce :string}
   :template {:desc "The volume claim template to enlarge - interactive selection when omitted"
              :alias :t
              :coerce :string}
   :capacity {:desc "The new storage capacity (bare numbers mean Gi)"
              :alias :c
              :require true
              :coerce :string}})

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get ["statefulsets" "persistentvolumeclaims" "storageclasses" "pods"]
   :kubectl-can-patch ["persistentvolumeclaims"]
   :kubectl-can-create ["statefulsets"]
   :kubectl-can-delete ["statefulsets"]})

(def statefulset-select
  {:mode :namespaced
   :label "StatefulSet"
   :candidates (fn [context] (kubectl/namespaces-names (kubectl/get-all context "statefulsets")))
   :namespace-flag :namespace
   :name-flag :statefulset
   :empty-message "there is no StatefulSet to enlarge"
   :on-empty :fail})

(defn- read-strictly
  "{:values <what read returns>} or {:unread <why>}, read as mutation/derived
   reads, so a read that did not answer is never planned from as an absent
   object."
  [read]
  (mutation/derived (fn [_] (read)) nil))

(defn- unread!
  [why]
  (stdout/error (format "could not read what the change rests on - %s" why))
  false)

(defn- planned
  "The expansion plan for the claims of template-name, with every problem
   that must stop it, read strictly."
  [context cluster namespace statefulset template-name capacity]
  (read-strictly
   #(let [plan (storage/expansion-plan
                {:pvc-names (storage/statefulset-pvc-names statefulset template-name)
                 :pvcs {:items (kubectl/filter-list
                                (kubectl/get-all context "persistentvolumeclaims")
                                (fn [pvc] (= namespace (kubectl/namespace-of pvc))))}
                 :storage-classes (kubectl/get-all context "storageclasses")
                 :capacity capacity})]
      ;; Second-level prerequisites: the book's :prerequisites already proved
      ;; the generic ability to do this (get PVCs/storage classes, patch
      ;; PVCs). These are the checks that depend on WHAT was selected - the
      ;; chosen PVCs' storage class allows expansion, the sizes are sane, and
      ;; the storage backend (a ceph pool's free capacity) has room for this
      ;; growth. They fail closed, so an unverifiable backend blocks the resize.
      {:plan plan
       :problems (into (storage/expansion-problems plan)
                       (storage/resize-preflight-problems
                        {:context context :cluster cluster :namespace namespace :plan plan}))})))

(defn change
  "The confirmed change that grows the claims of plan to capacity. The
   StatefulSet is the change's subject: read again right before the
   confirmation, so the manifest applied again is the one confirmed, and
   pinned, so it is refused when the StatefulSet changed meanwhile."
  [{:keys [context cluster target template capacity plan applied-before]}]
  (let [{:keys [namespace name]} (kubectl/namespace-name-map target)
        steps (fn [object] (storage/statefulset-resize-steps context object template capacity plan))]
    {:title "enlarge volumes"
     :select statefulset-select
     :subject {:type "statefulsets"}
     :confirm {:action (format "enlarge the volumes of StatefulSet %s to %s - the StatefulSet is deleted (pods orphaned) and recreated"
                               target capacity)
               :destructive? true}
     :items (fn [_] (map :pvc plan))
     :announce (fn [_] (format "Enlarge volumes of StatefulSet %s to %s" target capacity))
     :effect (fn [{:keys [object]}] (storage/plan-of (steps object)))
     :verify (fn [_] (storage/statefulset-checks context cluster namespace name template capacity plan))
     :recovery (fn [{:keys [object applied]}]
                 (storage/statefulset-recovery {:context context :target target}
                                               (steps object)
                                               (- (or applied 0) applied-before)))}))

(defn- enlarge
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        capacity (storage/normalize-capacity (:capacity opts))]
    (if-let [target (select/pick statefulset-select opts context)]
      (let [{:keys [namespace name]} (kubectl/namespace-name-map target)
            {statefulset :values unread :unread}
            (read-strictly #(kubectl/get-namespaced-resource context namespace "statefulsets" name))
            templates (mapv #(-> % :metadata :name) (-> statefulset :spec :volumeClaimTemplates))]
        (cond
          unread (unread! unread)

          (empty? templates)
          (do (stdout/error (format "StatefulSet %s has no volume claim templates" target)) false)

          :else
          (if-let [template (interact/choose-one "Template" templates (:template opts))]
            (let [{{:keys [plan problems]} :values unread :unread}
                  (planned context cluster namespace statefulset template capacity)]
              (if unread
                (unread! unread)
                (do (stdout/print-section "📋 Preflight")
                    (doseq [{:keys [pvc current target]} plan]
                      (stdout/err-println (format "  plan: %s: %s -> %s" pvc current target)))
                    (if (seq problems)
                      (do (doseq [problem problems] (stdout/error problem))
                          (stdout/err-println "Refusing: a second-level prerequisite failed - nothing was changed.")
                          false)
                      (mutation/run-on! (change {:context context :cluster cluster :target target
                                                 :template template :capacity capacity :plan plan
                                                 :applied-before (outcome/applied-count)})
                                        target opts announcement-data)))))
            (do (stdout/error "no volume claim template selected") false))))
      false)))

(runbook/execute!
 {:description "Grows the volumes of any StatefulSet via orphan-delete and recreate"
  :options options
  :prerequisites prerequisites
  :action enlarge})
