;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.statefulsets.enlarge-volumes
  "Grows the volumes of any StatefulSet. A volumeClaimTemplate is immutable,
   so the StatefulSet is deleted with its pods orphaned, the PVCs are
   enlarged, and the StatefulSet is recreated with the new template - after
   which everything matches again. The full plan is validated first, and the
   result is verified volume by volume."
  (:require [cheshire.core :as json]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.announce :as announce]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.storage :as storage]
            [gumshoe.watch :as watch]))

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

(defn- execute-enlargement!
  [context namespace statefulset template-name capacity plan]
  (let [manifest (storage/recreate-manifest statefulset template-name capacity)
        resizes (for [{:keys [pvc]} plan]
                  (do (stdout/print-command "kubectl" "patch" "persistentvolumeclaim" pvc)
                      (kubectl/patch! context namespace "persistentvolumeclaims" pvc
                                      {:spec {:resources {:requests {:storage capacity}}}})))]
    (if-not (every? #(zero? (:exit %)) (doall resizes))
      (do (stdout/error "could not enlarge every PVC - nothing was deleted, resolve manually") false)
      (let [name (kubectl/name-of statefulset)
            deleted (shell/execute "kubectl" (str "--context=" context) (str "--namespace=" namespace)
                                   "delete" "statefulset" name "--cascade=orphan")]
        (if-not (zero? (:exit deleted))
          (do (stdout/error (format "could not delete the StatefulSet:\n%s" (:err deleted))) false)
          (let [applied (shell/execute-with-stdin (json/encode manifest)
                                                  "kubectl" (str "--context=" context)
                                                  "apply" "--filename" "-")]
            (if (zero? (:exit applied))
              (do (stdout/ok "StatefulSet recreated with the new volume claim template") true)
              (do (stdout/error (format "THE STATEFULSET IS DELETED but could not be recreated:\n%s\nre-apply this manifest immediately:\n%s"
                                        (:err applied) (json/encode manifest)))
                  false))))))))

(defn- enlarge
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        capacity (storage/normalize-capacity (:capacity opts))
        candidates (kubectl/namespaces-names (kubectl/get-all context "statefulsets"))
        target (interact/choose-namespaced "StatefulSet" candidates (:namespace opts) (:statefulset opts))]
    (if (nil? target)
      (do (stdout/error "no StatefulSet selected") false)
      (let [{:keys [namespace name]} (kubectl/split-namespace-name target)
            statefulset (kubectl/get-namespaced-resource context namespace "statefulsets" name)
            templates (mapv #(-> % :metadata :name) (-> statefulset :spec :volumeClaimTemplates))
            template (interact/choose-one "Template" templates (:template opts))]
        (cond
          (empty? templates)
          (do (stdout/error (format "StatefulSet %s has no volume claim templates" target)) false)

          (nil? template)
          (do (stdout/error "no volume claim template selected") false)

          :else
          (let [pvc-names (storage/statefulset-pvc-names statefulset template)
                plan (storage/expansion-plan {:pvc-names pvc-names
                                              :pvcs {:items (kubectl/filter-list
                                                             (kubectl/get-all context "persistentvolumeclaims")
                                                             #(= namespace (kubectl/namespace-of %)))}
                                              :storage-classes (kubectl/get-all context "storageclasses")
                                              :capacity capacity})
                problems (storage/expansion-problems plan)]
            (stdout/print-section "📋 Enlargement plan")
            (doseq [{:keys [pvc current target]} plan]
              (stdout/err-println (format "  %s: %s -> %s" pvc current target)))
            (if (seq problems)
              (do (doseq [problem problems] (stdout/error problem))
                  false)
              (flow/change!
               {:confirmation {:action (format "enlarge volumes to %s - the StatefulSet is deleted (pods orphaned) and recreated" capacity)
                               :target (format "%s (%s)" target cluster)
                               :items (map :pvc plan)
                               :destructive? true}
                :announce! #(announce/announce! cluster announcement-data
                                                             (format "Enlarge volumes of StatefulSet %s to %s" target capacity))
                :execute! #(execute-enlargement! context namespace statefulset template capacity plan)
                :post-checks (concat
                              ;; best-effort: on offline-resize storage classes the capacity only
                              ;; updates on a pod restart, and the resize already succeeded. We watch
                              ;; the namespace events and PVC resize conditions while waiting, so a
                              ;; resizer error is surfaced live.
                              [{:description (format "all %d PVCs report capacity %s" (count plan) capacity)
                                :timeout 300 :interval 15 :soft? true
                                :watch (watch/resize-watchers context cluster namespace (map :pvc plan))
                                :check (fn []
                                         (every? (fn [{:keys [pvc]}]
                                                   (= capacity (-> (kubectl/get-namespaced-resource context namespace
                                                                                                    "persistentvolumeclaims" pvc)
                                                                   :status :capacity :storage)))
                                                 plan))}]
                              [{:description (format "StatefulSet %s exists again with the new template" target)
                                :timeout 60
                                :check (fn []
                                         (let [recreated (kubectl/get-namespaced-resource context namespace
                                                                                          "statefulsets" name)]
                                           (some #(and (= template (-> % :metadata :name))
                                                       (= capacity (-> % :spec :resources :requests :storage)))
                                                 (-> recreated :spec :volumeClaimTemplates))))}
                               {:description "every pod of the StatefulSet is Running"
                                :timeout 300
                                :interval 10
                                :check (fn []
                                         (let [recreated (kubectl/get-namespaced-resource context namespace
                                                                                          "statefulsets" name)]
                                           (= (-> recreated :spec :replicas)
                                              (-> recreated :status :readyReplicas))))}])}))))))))

(runbook/execute!
 {:description "Grows the volumes of any StatefulSet via orphan-delete and recreate"
  :options options
  :prerequisites prerequisites
  :action enlarge})
