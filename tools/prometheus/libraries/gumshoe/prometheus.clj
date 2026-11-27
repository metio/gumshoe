;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.prometheus
  "Prometheus-operator specifics for storage resizes; the generic planning
   and validation lives in gumshoe.storage."
  (:require [clojure.string :as str]
            [gumshoe.conditions :as conditions]
            [gumshoe.deletes :as deletes]
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
   whether its storage class allows it. A pod mounts a claim of its own
   namespace, so the claim is matched by namespace and name: two Prometheus
   of one name in different namespaces have claims of the same name."
  [{:keys [pods volume-name pvcs storage-classes capacity]}]
  (vec
   (for [pod (sort-by kubectl/name-of (kubectl/items-of pods))
         :let [pvc-name (pvc-of-pod pod volume-name)
               pvc (first (filter #(and (= pvc-name (kubectl/name-of %))
                                        (= (kubectl/namespace-of pod) (kubectl/namespace-of %)))
                                  (kubectl/items-of pvcs)))
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

(defn selected-by?
  "Whether service selects pod: the pod carries every label of the service's
   selector. A service without a selector selects nothing."
  [service pod]
  (let [selector (-> service :spec :selector)
        labels (-> pod :metadata :labels)]
    (boolean (and (seq selector)
                  (every? (fn [[k v]] (= v (get labels k))) selector)))))

(defn pod-api-port
  "The container port of pod that the service's port named port-name
   forwards to: its targetPort when that is a number, the pod's container
   port of that name when it is a name, and the service port itself when it
   sets none. nil when it cannot be resolved."
  [service port-name pod]
  (when-let [port (first (filter #(= port-name (:name %)) (-> service :spec :ports)))]
    (let [target (:targetPort port)]
      (cond
        (nil? target) (:port port)
        (integer? target) target
        (re-matches #"\d+" (str target)) (parse-long (str target))
        :else (->> (-> pod :spec :containers)
                   (mapcat :ports)
                   (filter #(= target (:name %)))
                   first
                   :containerPort)))))

(defn delete-targets
  "The pods a metric deletion must reach for the Prometheus named prometheus
   behind service, each with the port its API listens on, as
   {:targets [{:pod <name> :port <port>}]}, or {:refusal <why>}. pods are
   the Prometheus' own pods, read by its pod selector; only those carrying
   its instance label (`instance`, the Prometheus' name) that the service
   selects count, so a pod of another Prometheus is never among them. Every replica and every shard holds its own TSDB, so
   deleting from fewer than all of them leaves the metric queryable, and a
   pod that is not ready, or whose port cannot be resolved, refuses the
   whole deletion rather than skip it."
  [{:keys [service port-name prometheus instance pods]}]
  (let [mine (sort-by kubectl/name-of (filter #(and (selected-by? service %)
                                                    (= instance (get-in % [:metadata :labels :app.kubernetes.io/instance])))
                                              (kubectl/items-of pods)))
        unready (remove #(and (= "Running" (-> % :status :phase)) (conditions/ready? %)) mine)
        targets (mapv (fn [pod] {:pod (kubectl/name-of pod) :port (pod-api-port service port-name pod)}) mine)]
    (cond
      (nil? pods)
      {:refusal (format "could not read the pods of Prometheus %s" prometheus)}

      (empty? mine)
      {:refusal (format "service %s selects no pod of Prometheus %s" (kubectl/name-of service) prometheus)}

      (seq unready)
      {:refusal (format "pod(s) %s of Prometheus %s are not ready, so the metric could not be deleted from every replica"
                        (str/join ", " (map kubectl/name-of unready)) prometheus)}

      (some (comp nil? :port) targets)
      {:refusal (format "the port '%s' of service %s cannot be resolved on pod(s) %s - check --port-name"
                        port-name (kubectl/name-of service)
                        (str/join ", " (map :pod (filter (comp nil? :port) targets))))}

      :else {:targets targets})))

(defn delete-on-every-pod!
  "Deletes from every target in turn and reports each: `on-pod!` is
   (fn [target record!] -> truthy) and runs the deletion and its post-check
   on one pod, calling (record! k v) as each is known: :reached? once a
   port-forward to the pod runs, :deleted? when the
   pod accepted the deletion, :verified? with the post-check's verdict (a
   best-effort check that ran out its time counts, as verify/all counts it)
   and :confirmed? when the pod answered that no series is left. A pod whose
   forward fails part-way so still reports what it did, and an on-pod! that
   returns falsy is not verified whatever it recorded. Returns
   {:results [{:pod :reached? :deleted? :verified? :confirmed?}] :ok? <every pod
   deleted and verified>}. Each pod is tried even after one fails: the
   operator confirmed the deletion on all of them, and the report names
   which ones kept the metric."
  [targets on-pod!]
  (let [results (mapv (fn [{:keys [pod] :as target}]
                        (let [seen (atom {:pod pod :reached? false :deleted? false :verified? false :confirmed? false})
                              finished (on-pod! target (fn [k v] (swap! seen assoc k v)))]
                          (cond-> @seen (not finished) (assoc :verified? false))))
                      targets)]
    {:results results
     :ok? (boolean (and (seq results) (every? #(and (:deleted? %) (:verified? %)) results)))}))

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
             :effect (deletes/effect context {:type "statefulset" :namespace namespace
                                              :selector (statefulset-selector name)}
                                     {:cascade :orphan})}
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
