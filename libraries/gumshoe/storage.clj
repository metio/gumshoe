;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.storage
  "Planning, validation and effect plans for volume enlargements - shared by
   every book that grows PVCs, whatever controller owns them. All the ways an
   enlargement can go wrong are caught before anything touches the cluster.

   A resize runs as a plan of steps, each {:effect <one effect> :stage
   <keyword>}, a claim's step also naming its :claim. The steps are data, so
   the same vector gives the plan to run or rehearse and, once a run stopped
   after some of its effects, says what it left behind (see `progress`)."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [gumshoe.claims :as claims]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.verify :as verify]
            [gumshoe.watch :as watch]))

(defn normalize-capacity
  "A bare number means Gi: '100' -> '100Gi'."
  [capacity]
  (if (re-matches #"\d+" (str capacity))
    (str capacity "Gi")
    (str capacity)))

(defn gi-value
  [capacity]
  (some-> (re-matches #"(\d+)Gi" (str capacity)) second parse-long))

(defn statefulset-pvc-names
  "The PVC names a StatefulSet's volume claim template creates:
   <template>-<statefulset>-<ordinal>, the ordinals counting from
   spec.ordinals.start."
  [statefulset template-name]
  (let [replicas (or (-> statefulset :spec :replicas) 1)
        start (or (-> statefulset :spec :ordinals :start) 0)]
    (vec (for [ordinal (range start (+ start replicas))]
           (format "%s-%s-%d" template-name (kubectl/name-of statefulset) ordinal)))))

(defn statefulset-claim-names
  "Every claim of template-name a resize of statefulset must grow: the
   claims of its current replicas (statefulset-pvc-names), then each claim
   of pvcs that belongs to the template at another ordinal
   (gumshoe.claims/statefulset-claim?), in ordinal order. A StatefulSet
   scaled down keeps those claims and mounts them again when it scales up,
   so a resize that left them would bring those replicas back on the old
   size."
  [statefulset template-name pvcs]
  (let [current (statefulset-pvc-names statefulset template-name)]
    (into current
          (->> (kubectl/items-of pvcs)
               (filter #(claims/statefulset-claim? statefulset template-name %))
               (sort-by #(claims/template-ordinal statefulset template-name (kubectl/name-of %)))
               (map kubectl/name-of)
               (remove (set current))))))

(defn unclaimed-template-names
  "A problem for every claim of pvcs named for an ordinal of template-name
   outside the current replicas that carries neither the StatefulSet's
   selector labels nor an owner reference to it. The controller would mount
   it on a scale-up by its name, yet nothing shows it was made for this
   StatefulSet, so the resize neither grows it nor leaves it behind
   silently."
  [statefulset template-name pvcs]
  (let [current (set (statefulset-pvc-names statefulset template-name))]
    (vec (for [claim (kubectl/items-of pvcs)
               :let [name (kubectl/name-of claim)]
               :when (and (= (kubectl/namespace-of statefulset) (kubectl/namespace-of claim))
                          (not (current name))
                          (claims/template-ordinal statefulset template-name name)
                          (not (claims/statefulset-claim? statefulset template-name claim)))]
           (format (str "PVC %s has the name of a claim of StatefulSet %s, which would mount it on a scale-up,"
                        " but carries neither its selector labels nor an owner reference to it -"
                        " label it, enlarge it by hand or delete it, then run this book again")
                   name (kubectl/namespace-name-of statefulset))))))

(defn expansion-plan
  "One entry per PVC: what changes from what to what, and whether its
   storage class allows it."
  [{:keys [pvc-names pvcs storage-classes capacity]}]
  (vec (for [pvc-name pvc-names
             :let [pvc (first (filter #(= pvc-name (kubectl/name-of %)) (kubectl/items-of pvcs)))
                   class-name (-> pvc :spec :storageClassName)
                   class (first (filter #(= class-name (kubectl/name-of %)) (kubectl/items-of storage-classes)))]]
         {:pvc pvc-name
          :exists? (some? pvc)
          :storage-class class-name
          :resizable? (true? (:allowVolumeExpansion class))
          :current (-> pvc :spec :resources :requests :storage)
          :target capacity})))

(defonce ^:private resize-preflights (atom []))

(defn register-resize-preflight!
  "Registers a storage-provider preflight for a resize - a SECOND-LEVEL
   prerequisite. A book's :prerequisites are the first level: the generic ability
   to do the operation at all (get PVCs and storage classes, patch PVCs), checked
   before selection. These run AFTER selection and depend on what was chosen: is
   the selected PVC's storage class expandable, does the backend have room. `check`
   is (fn [context] -> seq of problem strings, empty when safe), where context is
   {:context :cluster :namespace :plan}. Any problem stops the resize. A provider
   checks its backend has room for the growth (the ceph package checks the pool's
   free capacity).

   Fail CLOSED: a check that can not confirm safety must RETURN a problem, never
   assume ok. A check that throws is itself treated as a problem."
  [check]
  {:pre [(fn? check)]}
  (swap! resize-preflights conj check))

(defn resize-preflight-problems
  "Problems from every registered provider preflight for `context`
   (a map {:context :cluster :namespace :plan}). Best-effort and fail-closed: a
   check that throws becomes a problem rather than being silently skipped, so a
   broken capacity check blocks the resize instead of waving it through."
  [context]
  (vec
   (mapcat (fn [check]
             (try
               (let [result (check context)]
                 (cond
                   (nil? result) []
                   (sequential? result) (remove nil? result)
                   :else [(str "a storage preflight returned an unexpected value: " (pr-str result))]))
               (catch Exception e
                 [(str "a storage preflight failed to run (treated as unsafe): "
                       (or (ex-message e) (str e)))])))
           @resize-preflights)))

(defn expansion-problems
  "Every reason this plan must not run. An empty result means it is safe."
  [plan]
  (vec
   (concat
    (when (empty? plan)
      ["no volumes to enlarge found"])
    (for [{:keys [pvc exists?]} plan
          :when (false? exists?)]
      (format "PVC %s does not exist" pvc))
    (for [{:keys [pvc storage-class resizable? exists?]} plan
          :when (and (not (false? exists?)) (not resizable?))]
      (format "storage class %s of PVC %s does not allow volume expansion" storage-class pvc))
    (for [{:keys [pvc current target exists?]} plan
          :when (and (not (false? exists?))
                     (gi-value current) (gi-value target)
                     (< (gi-value target) (gi-value current)))]
      (format "PVC %s already has %s - shrinking to %s is not possible" pvc current target))
    (for [{:keys [pvc current target exists?]} plan
          :when (and (not (false? exists?))
                     (or (nil? (gi-value current)) (nil? (gi-value target))))]
      (format "PVC %s: cannot compare '%s' with '%s' - only Gi capacities are supported"
              pvc current target)))))

(defn recreate-manifest
  "A StatefulSet manifest ready for re-apply after an orphan-cascade delete:
   server-managed fields are dropped and the volume claim template carries
   the new capacity, so the recreated StatefulSet matches its PVCs again."
  [statefulset template-name capacity]
  (-> statefulset
      (dissoc :status)
      (update :metadata dissoc :uid :resourceVersion :creationTimestamp :generation :managedFields :selfLink)
      (update-in [:metadata :annotations] dissoc (keyword "kubectl.kubernetes.io/last-applied-configuration"))
      (update-in [:spec :volumeClaimTemplates]
                 (fn [templates]
                   (mapv (fn [template]
                           (if (= template-name (-> template :metadata :name))
                             (assoc-in template [:spec :resources :requests :storage] capacity)
                             template))
                         templates)))))

;; --- the resize as a plan --------------------------------------------------------

(defn claim-patch
  "The merge patch that requests capacity on a claim."
  [capacity]
  {:spec {:resources {:requests {:storage capacity}}}})

(defn resize-claim-steps
  "One step per claim of plan, in plan order, merge-patching its request to
   capacity. Kubernetes never shrinks a claim, so a step that ran cannot be
   undone."
  [context namespace plan capacity]
  (vec (for [{:keys [pvc]} plan]
         {:stage :claims
          :claim pvc
          :effect (apply effect/kubectl context
                         (kubectl/merge-patch-args namespace "persistentvolumeclaims" pvc (claim-patch capacity)))})))

(defn resize-claim-effects
  "The effects of resize-claim-steps."
  [context namespace plan capacity]
  (mapv :effect (resize-claim-steps context namespace plan capacity)))

(defn plan-of
  "The effect plan of steps."
  [steps]
  (mapv :effect steps))

(defn progress
  "Where a run of steps stands after `applied` of their effects ran.
   gumshoe.effect/run! applies a plan in order and stops at the first effect
   that fails, so the first `applied` steps ran and the next one is where it
   stopped. Returns :resized, the claims whose step ran; :unresized, those
   whose step did not; :stages, the stages a step of which ran; and :stopped,
   the step it stopped at, nil when every step ran."
  [steps applied]
  (let [applied (or applied 0)
        [ran rest] (split-at applied steps)]
    {:resized (vec (keep :claim ran))
     :unresized (vec (keep :claim rest))
     :stages (set (map :stage ran))
     :stopped (first rest)}))

(defn- listed
  [claims]
  (map #(str "  " %) claims))

(defn claims-left
  "The lines naming which claims a stopped resize enlarged and which it did
   not."
  [{:keys [resized unresized]}]
  (concat
   (when (seq resized)
     (cons "Enlarged, and they stay so - a claim cannot shrink, so this cannot be undone:" (listed resized)))
   (when (seq unresized)
     (cons "Not enlarged:" (listed unresized)))))

(defn claims-capacity-check
  "The post-check that every claim of plan reports capacity. It is best
   effort: on a storage class that resizes the filesystem offline the
   capacity only updates on a pod restart, and the resize itself already
   succeeded. The namespace's Warning events and the claims' resize
   conditions are watched while it waits, so a resizer error surfaces live."
  [context cluster namespace plan capacity]
  {:description (format "all %d PVCs report capacity %s" (count plan) capacity)
   :timeout 300 :interval 15 :soft? true
   :watch (watch/resize-watchers context cluster namespace (map :pvc plan))
   :check (fn []
            (every? (fn [{:keys [pvc]}]
                      (= capacity (-> (kubectl/get-namespaced-resource context namespace
                                                                       "persistentvolumeclaims" pvc)
                                      :status :capacity :storage)))
                    plan))})

;; --- a StatefulSet's volumes -----------------------------------------------------

(defn statefulset-resize-steps
  "The steps that grow the claims of `template-name` of statefulset to
   capacity: enlarge every claim of plan, delete the StatefulSet with its pods
   orphaned, since a volumeClaimTemplate is immutable, and apply it again with
   the new template."
  [context statefulset template-name capacity plan]
  (let [namespace (kubectl/namespace-of statefulset)
        name (kubectl/name-of statefulset)
        manifest (json/encode (recreate-manifest statefulset template-name capacity))]
    (conj (resize-claim-steps context namespace plan capacity)
          {:stage :delete
           :effect (effect/kubectl context (str "--namespace=" namespace)
                                   "delete" "statefulset" name "--cascade=orphan")}
          {:stage :recreate
           :manifest manifest
           :effect (effect/kubectl-stdin context manifest "apply" "--filename" "-")})))

(defn statefulset-recovery
  "What a run of statefulset-resize-steps left behind after `applied` of its
   effects ran, and what to do about it; nil when none ran. target names the
   StatefulSet as namespace/name."
  [{:keys [context target]} steps applied]
  (let [{:keys [stopped stages] :as where} (progress steps applied)
        manifest (:manifest (last steps))]
    (when (seq stages)
      (str/join
       "\n"
       (case (:stage stopped)
         :claims
         (concat [(format "StatefulSet %s was not touched, and its pods keep running." target)]
                 (claims-left where)
                 [(format "Find in the kubectl output above why %s could not be patched, then run this book again" (:claim stopped))
                  "with the same capacity: a claim that already requests it is patched to the same value."])

         :delete
         (concat [(format "StatefulSet %s could not be deleted: it still exists with its old volume claim template," target)
                  "and its pods were not touched."]
                 (claims-left where)
                 ["Find why in the kubectl output above, then run this book again with the same capacity."])

         :recreate
         (concat [(format "THE STATEFULSET %s IS DELETED and could not be recreated. Its pods were orphaned:" target)
                  "they keep running, but nothing replaces a pod that stops."]
                 (claims-left where)
                 ["Re-apply it now, with the new volume claim template:"
                  (format "  kubectl --context=%s apply --filename - <<'EOF'" context)
                  (str "  " manifest)
                  "  EOF"])

         (concat [(format "Every effect ran: StatefulSet %s was recreated with the new volume claim template." target)]
                 (claims-left where)
                 ["Nothing was rolled back. The post-check messages above name what did not hold."]))))))

(defn statefulset-checks
  "The post-checks of a StatefulSet's resize: the claims report capacity,
   the StatefulSet exists again with the new template, and every replica is
   ready."
  [context cluster namespace name template-name capacity plan]
  [(claims-capacity-check context cluster namespace plan capacity)
   {:description (format "StatefulSet %s/%s exists again with the new template" namespace name)
    :timeout 60
    :check (fn []
             (let [recreated (kubectl/get-namespaced-resource context namespace "statefulsets" name)]
               (boolean
                (some #(and (= template-name (-> % :metadata :name))
                            (= capacity (-> % :spec :resources :requests :storage)))
                      (-> recreated :spec :volumeClaimTemplates)))))}
   {:description "every pod of the StatefulSet is Running"
    :timeout 300
    :interval 10
    :check (fn []
             (verify/object-holds? (kubectl/get-namespaced-resource context namespace "statefulsets" name)
                                   #(= (-> % :spec :replicas) (-> % :status :readyReplicas))))}])
