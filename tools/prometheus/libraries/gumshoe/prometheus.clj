;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.prometheus
  "Prometheus-operator specifics for storage resizes; the generic planning
   and validation lives in gumshoe.storage."
  (:require [clojure.string :as str]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.storage :as storage]))

(def prometheus-type "prometheuses.monitoring.coreos.com")

(def governing-service-selector
  "The label prometheus-operator puts on the governing service
   (prometheus-operated) of every Prometheus it runs."
  "operated-prometheus=true")

(def default-port-name
  "The name of the governing service's port unless a Prometheus sets
   spec.portName."
  "web")

(defn api-port
  "The number of a service's port of that name, or nil."
  [service port-name]
  (kubectl/service-port service port-name))

(defn volume-name-of
  "The pod volume the storage claim template becomes: the template's own name,
   else the operator's default <prefix>-<name>-db, with prefix prometheus for a
   Prometheus and prom-agent for a PrometheusAgent."
  [prometheus]
  (or (not-empty (-> prometheus :spec :storage :volumeClaimTemplate :metadata :name))
      (format "%s-%s-db"
              (if (= "PrometheusAgent" (:kind prometheus)) "prom-agent" "prometheus")
              (-> prometheus :metadata :name))))

(defn pvc-of-pod
  [pod volume-name]
  (->> (-> pod :spec :volumes)
       (filter #(= volume-name (:name %)))
       first
       :persistentVolumeClaim
       :claimName))

(defn resize-plan
  "One entry per prometheus pod: which PVC changes from what to what, and
   whether its storage class allows it."
  [{:keys [pods volume-name pvcs storage-classes capacity]}]
  (vec
   (for [pod (sort-by kubectl/name-of (kubectl/items-of pods))
         :let [pvc-name (pvc-of-pod pod volume-name)
               pvc (first (filter #(= pvc-name (kubectl/name-of %)) (kubectl/items-of pvcs)))
               class-name (-> pvc :spec :storageClassName)
               class (first (filter #(= class-name (kubectl/name-of %)) (kubectl/items-of storage-classes)))]
         :when pvc-name]
     {:pod (kubectl/name-of pod)
      :pvc pvc-name
      :exists? (some? pvc)
      :storage-class class-name
      :resizable? (true? (:allowVolumeExpansion class))
      :current (-> pvc :spec :resources :requests :storage)
      :target capacity})))

(defn pods-selector
  "The label selector of the pods prometheus-operator runs for the
   Prometheus called name."
  [name]
  (format "app.kubernetes.io/name=prometheus,app.kubernetes.io/instance=%s" name))

(defn pods-of
  "The pods of the Prometheus namespace/name, as a list document."
  [context namespace name]
  {:items (filter #(= namespace (kubectl/namespace-of %))
                  (kubectl/items-of (kubectl/get-selected context "pods" (pods-selector name))))})

(defn statefulset-selector
  "The label selector of the StatefulSets prometheus-operator runs for the
   Prometheus called name, one per shard."
  [name]
  (str "operator.prometheus.io/name=" name))

(defn- patch-step
  [stage context namespace name patch]
  {:stage stage
   :effect (apply effect/kubectl context (kubectl/merge-patch-args namespace prometheus-type name patch))})

(defn unpause-args
  "The kubectl command that unpauses the Prometheus, as an operator types it."
  [context namespace name]
  (format "kubectl --context=%s --namespace=%s patch %s %s --type=merge --patch='{\"spec\":{\"paused\":false}}'"
          context namespace prometheus-type name))

(defn resize-steps
  "The steps that grow the storage of the Prometheus namespace/name to
   capacity: pause it and request capacity in its volumeClaimTemplate, so the
   operator leaves its StatefulSet alone; enlarge every claim of plan; delete
   the StatefulSet with its pods orphaned, since a volumeClaimTemplate is
   immutable; and unpause it, so the operator creates the StatefulSet again
   from the new template."
  [context namespace name capacity plan]
  (-> [(patch-step :pause context namespace name
                   {:spec {:paused true
                           :storage {:volumeClaimTemplate (storage/claim-patch capacity)}}})]
      (into (storage/resize-claim-steps context namespace plan capacity))
      (conj {:stage :delete
             :effect (effect/kubectl context (str "--namespace=" namespace)
                                     "delete" "statefulset"
                                     (str "--selector=" (statefulset-selector name))
                                     "--cascade=orphan")}
            (patch-step :unpause context namespace name {:spec {:paused false}}))))

(defn resize-recovery
  "What a run of resize-steps left behind after `applied` of its effects
   ran, and what to do about it; nil when none ran. target names the
   Prometheus as namespace/name."
  [{:keys [context target]} steps applied]
  (let [{:keys [stopped stages] :as where} (storage/progress steps applied)
        {:keys [namespace name]} (kubectl/namespace-name-map target)
        paused (format "Prometheus %s is PAUSED, and its volumeClaimTemplate already requests the new capacity:" target)]
    (when (seq stages)
      (str/join
       "\n"
       (case (:stage stopped)
         :claims
         (concat [paused
                  "the operator leaves its StatefulSet alone while it is paused."]
                 (storage/claims-left where)
                 [(format "Find in the kubectl output above why %s could not be patched, then run this book again" (:claim stopped))
                  "with the same capacity: it pauses and patches the Prometheus again, which changes nothing, and"
                  "patches a claim that already requests the capacity to the same value."])

         :delete
         (concat [paused
                  "its StatefulSet could not be deleted, so it still has the old volume claim template."]
                 (storage/claims-left where)
                 ["Find why in the kubectl output above, then run this book again with the same capacity."])

         :unpause
         (concat [(format "THE STATEFULSET OF PROMETHEUS %s IS DELETED, and the Prometheus is still PAUSED, so the" target)
                  "operator does not create it again. Its pods were orphaned and keep running."]
                 (storage/claims-left where)
                 ["Unpause it now:"
                  (str "  " (unpause-args context namespace name))])

         (concat [(format "Every effect ran: Prometheus %s was unpaused with the new volume claim template." target)]
                 (storage/claims-left where)
                 ["Nothing was rolled back. The post-check messages above name what did not hold."]))))))

(defn statefulsets-of
  "The StatefulSets of the Prometheus namespace/name, as a list document, or
   nil when the read did not answer."
  [context namespace name]
  (kubectl/get-json context "statefulsets" (str "--namespace=" namespace)
                    (str "--selector=" (statefulset-selector name))))

(defn recreated?
  "Whether the operator runs the Prometheus again with the new template: it
   is unpaused, it has at least one StatefulSet, and every one of them
   requests capacity in the Prometheus' claim template with every replica
   ready (storage/statefulset-resized?)."
  [prometheus statefulsets capacity]
  (let [sets (kubectl/items-of statefulsets)
        volume-name (volume-name-of prometheus)]
    (boolean (and (some? prometheus)
                  (not (true? (-> prometheus :spec :paused)))
                  (seq sets)
                  (every? #(storage/statefulset-resized? % volume-name capacity) sets)))))

(defn- controlled-by?
  [uids pod]
  (some #(and (= "StatefulSet" (:kind %)) (true? (:controller %)) (contains? uids (:uid %)))
        (-> pod :metadata :ownerReferences)))

(defn pods-adopted?
  "Whether there are pods and every one is Running and controlled by one of
   statefulsets, matched by uid. A pod orphaned by the delete keeps Running
   with no controller until a recreated StatefulSet adopts it, so Running
   alone says nothing about the change."
  [pods statefulsets]
  (let [uids (set (keep #(-> % :metadata :uid) (kubectl/items-of statefulsets)))
        pods (kubectl/items-of pods)]
    (boolean (and (seq pods)
                  (every? #(and (= "Running" (-> % :status :phase)) (controlled-by? uids %)) pods)))))

(defn resize-checks
  "The post-checks of a Prometheus resize: the claims report capacity; the
   operator recreated the StatefulSet with the new template and every replica
   is ready; and every pod of the Prometheus is Running under that
   StatefulSet again."
  [context cluster namespace name capacity plan]
  [(storage/claims-capacity-check context cluster namespace plan capacity)
   {:description (format "the StatefulSet of Prometheus %s/%s exists again with the new template and every replica is ready"
                         namespace name)
    :timeout 300 :interval 15
    :check (fn []
             (recreated? (kubectl/get-namespaced-resource context namespace prometheus-type name)
                         (statefulsets-of context namespace name)
                         capacity))}
   {:description "every Prometheus pod is Running again under its StatefulSet"
    :timeout 300 :interval 15
    :check (fn []
             (pods-adopted? (pods-of context namespace name)
                            (statefulsets-of context namespace name)))}])
