;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.prometheus.set-capacity
  "Grows the storage of a prometheus-operator managed Prometheus. The full
   resize plan is validated before anything happens (expandable storage
   classes, no shrinking), and afterwards every volume is verified to have
   the new capacity and Prometheus is verified to be running again.

   Steps: pause the Prometheus and patch its volumeClaimTemplate, resize the
   existing PVCs, delete the StatefulSet with orphaned pods, unpause so the
   operator recreates the StatefulSet against the new template."
  (:require [infra.watch :as watch]
            [infra.flow :as flow]
            [infra.interact :as interact]
            [infra.kubectl :as kubectl]
            [infra.announce :as announce]
            [infra.prometheus :as prometheus]
            [infra.storage :as storage]
            [infra.runbook :as runbook]
            [infra.shell :as shell]
            [infra.stdout :as stdout]))

(def prometheus-type "prometheuses.monitoring.coreos.com")

(def options
  {:namespace {:desc "The namespace of the Prometheus - interactive selection when omitted"
               :alias :n
               :coerce :string}
   :prometheus {:desc "The Prometheus instance to resize - interactive selection when omitted"
                :alias :p
                :coerce :string}
   :capacity {:desc "The new storage capacity (bare numbers mean Gi)"
              :alias :c
              :require true
              :coerce :string}})

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get [prometheus-type "pods" "persistentvolumeclaims" "storageclasses"]
   :kubectl-can-patch [prometheus-type "persistentvolumeclaims"]
   :kubectl-can-delete ["statefulsets"]})

(defn- pods-of-prometheus
  [context namespace name]
  {:items (filter #(= namespace (kubectl/namespace-of %))
                  (kubectl/items-of
                   (kubectl/get-selected context "pods"
                                         (format "app.kubernetes.io/name=prometheus,app.kubernetes.io/instance=%s" name))))})

(defn- execute-resize!
  [context namespace name capacity plan]
  (let [paused (kubectl/patch! context namespace prometheus-type name
                               {:spec {:paused true
                                       :storage {:volumeClaimTemplate
                                                 {:spec {:resources {:requests {:storage capacity}}}}}}})]
    (if-not (zero? (:exit paused))
      (do (stdout/error (format "could not pause and patch the Prometheus:\n%s" (:err paused))) false)
      (let [resizes (for [{:keys [pvc]} plan]
                      (do (stdout/print-command "kubectl" "patch" "persistentvolumeclaim" pvc)
                          (kubectl/patch! context namespace "persistentvolumeclaims" pvc
                                          {:spec {:resources {:requests {:storage capacity}}}})))]
        (if-not (every? #(zero? (:exit %)) (doall resizes))
          (do (stdout/error "could not resize every PVC - the Prometheus stays paused, resolve manually") false)
          (let [deleted (shell/execute "kubectl" (str "--context=" context) (str "--namespace=" namespace)
                                       "delete" "statefulset"
                                       (str "--selector=operator.prometheus.io/name=" name)
                                       "--cascade=orphan")]
            (if-not (zero? (:exit deleted))
              (do (stdout/error (format "could not delete the StatefulSet:\n%s" (:err deleted))) false)
              (let [unpaused (kubectl/patch! context namespace prometheus-type name
                                             {:spec {:paused false}})]
                (if (zero? (:exit unpaused))
                  true
                  (do (stdout/error (format "could not unpause the Prometheus:\n%s" (:err unpaused))) false))))))))))

(defn- set-capacity
  [opts {:keys [announcement-data]}]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        capacity (storage/normalize-capacity (:capacity opts))
        candidates (kubectl/namespaces-names (kubectl/get-all context prometheus-type))
        target (interact/choose-namespaced "Prometheus" candidates (:namespace opts) (:prometheus opts))]
    (if (nil? target)
      (do (stdout/error "no Prometheus selected") false)
      (let [{:keys [namespace name]} (kubectl/split-namespace-name target)
            resource (kubectl/get-namespaced-resource context namespace prometheus-type name)
            volume-name (prometheus/volume-name-of resource)
            pods (pods-of-prometheus context namespace name)
            plan (prometheus/resize-plan {:pods pods
                                          :volume-name volume-name
                                          :pvcs (kubectl/get-all context "persistentvolumeclaims")
                                          :storage-classes (kubectl/get-all context "storageclasses")
                                          :capacity capacity})
            problems (storage/expansion-problems plan)]
        (stdout/print-section "📋 Resize plan")
        (doseq [{:keys [pvc current target]} plan]
          (stdout/err-println (format "  %s: %s -> %s" pvc current target)))
        (if (seq problems)
          (do (doseq [problem problems] (stdout/error problem))
              false)
          (flow/change!
           {:confirmation {:action (format "resize the Prometheus storage to %s - the StatefulSet is recreated" capacity)
                           :target (format "%s (%s)" target cluster)
                           :items (map :pvc plan)
                           :destructive? true}
            :announce! #(announce/announce! cluster announcement-data
                                                         (format "Resize storage of Prometheus %s to %s" target capacity))
            :execute! #(execute-resize! context namespace name capacity plan)
            ;; the capacity check is best-effort: on a storage class that only
            ;; resizes the filesystem on a pod restart it converges later, and
            ;; the resize itself already succeeded. While we wait we watch the
            ;; namespace events and the PVC resize conditions, so a resizer error
            ;; surfaces live instead of leaving the operator guessing.
            :post-checks [{:description (format "all %d PVCs report capacity %s" (count plan) capacity)
                           :timeout 300 :interval 15 :soft? true
                           :watch (watch/resize-watchers context cluster namespace (map :pvc plan))
                           :check (fn []
                                    (every? (fn [{:keys [pvc]}]
                                              (= capacity (-> (kubectl/get-namespaced-resource context namespace
                                                                                               "persistentvolumeclaims" pvc)
                                                              :status :capacity :storage)))
                                            plan))}
                          {:description "every Prometheus pod is Running again"
                           :timeout 300 :interval 15
                           :check (fn []
                                    (let [pods (kubectl/items-of (pods-of-prometheus context namespace name))]
                                      (and (seq pods)
                                           (every? #(= "Running" (-> % :status :phase)) pods))))}]}))))))

(runbook/execute!
 {:description "Grows the storage of a prometheus-operator managed Prometheus"
  :options options
  :prerequisites prerequisites
  :action set-capacity})
