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
            [gumshoe.deletes :as deletes]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.quantity :as quantity]
            [gumshoe.reads.result :as result]
            [gumshoe.stdout :as stdout]
            [gumshoe.verify :as verify]
            [gumshoe.watch :as watch]
            [gumshoe.workloads :as workloads]))

(defn normalize-capacity
  "A bare number means Gi: '100' -> '100Gi'."
  [capacity]
  (if (re-matches #"\d+" (str capacity))
    (str capacity "Gi")
    (str capacity)))

(def ^:private bytes-per-gi 1073741824.0)

(defn gi-value
  "capacity as a whole number of Gi - '100Gi' -> 100, '1Ti' -> 1024, the
   form the API server reads a multiple of 1024Gi back as - or nil when it
   is not a whole number of Gi or does not parse."
  [capacity]
  (when-let [bytes (quantity/quantity->number capacity)]
    (let [gi (/ (double bytes) bytes-per-gi)]
      (when (and (>= gi 0) (== gi (Math/rint gi)))
        (long gi)))))

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
      (format "PVC %s: cannot compare '%s' with '%s' - only whole Gi capacities (such as 100Gi or 1Ti) are supported"
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
                      (quantity/at-least? (-> (kubectl/get-namespaced-resource context namespace
                                                                               "persistentvolumeclaims" pvc)
                                              :status :capacity :storage)
                                          capacity))
                    plan))})

;; --- claims no pod mounts --------------------------------------------------------
;;
;; A kept claim - one a scaled-down StatefulSet keeps for a replica that does
;; not run - is mounted by no pod. Our own patch only sets its request, so the
;; request alone proves nothing: the claim is done once the cluster shows it
;; took the resize up. Evidence the claim carried before the change is stale
;; and never decides anything; "before" is the claim and the events as read
;; right before the effect (kept-claims-baseline), compared by the cluster's
;; own fields rather than the local clock.

(def ^:private resize-error-conditions
  "The claim conditions a resizer sets when it failed to grow the volume."
  #{"ControllerResizeError" "NodeResizeError"})

(def ^:private infeasible-statuses
  "The allocatedResourceStatuses that say a resize can never succeed."
  #{"ControllerResizeInfeasible" "NodeResizeInfeasible"})

(def ^:private node-expansion-statuses
  "The allocatedResourceStatuses that say the resize controller finished and
   only the node's part, done when a pod mounts the volume, is left."
  #{"NodeResizePending" "NodeResizeInProgress"})

(def ^:private resize-failure-reasons
  "The reasons of the Warning events the external resizer and the kubelet
   emit for a claim they failed to grow."
  #{"VolumeResizeFailed" "FileSystemResizeFailed"})

(defn- event-key
  "What tells one state of an event from another: a repeated event keeps its
   uid and raises its count or its series."
  [event]
  [(-> event :metadata :uid) (:count event) (:lastTimestamp event) (:eventTime event)
   (-> event :series :count) (-> event :series :lastObservedTime)])

(defn- condition-key
  "What tells a condition written after the change from one carried over:
   Kubernetes keeps a resize condition's lastTransitionTime and message while
   its type and status stay the same."
  [condition]
  ((juxt :type :status :lastTransitionTime) condition))

(defn- one-line
  [text]
  (str/trim (str/replace (str text) #"\s+" " ")))

(defn- condition-text
  [{:keys [type message]}]
  (str type (when-not (str/blank? (str message)) (str ": " (one-line message)))))

(defn kept-claim-names
  "The names in retained among the claims of plan, in plan order."
  [plan retained]
  (let [retained (set retained)]
    (vec (keep #(when (contains? retained (:pvc %)) (:pvc %)) plan))))

(defn kept-claims-baseline
  "The state the post-check of the kept claims judges against, read right
   before the effect: {:claims {name claim-or-nil} :events #{event-key}}.
   Reads as the caller binds them; under kubectl/*strict-reads*, which a
   change's :refusal runs under, a read that did not answer throws, so no
   baseline is recorded from a cluster that did not answer."
  [context namespace claim-names]
  {:claims (into {} (for [claim-name claim-names]
                      [claim-name (kubectl/get-namespaced-resource context namespace "persistentvolumeclaims" claim-name)]))
   :events (set (map event-key (kubectl/items-of (kubectl/get-namespaced context namespace "events"))))})

(defn kept-claims-refusal
  "Why the change must not run given baseline, or nil: a kept claim that no
   longer exists cannot be resized."
  [{:keys [claims]}]
  (when-let [gone (seq (sort (keep (fn [[claim-name claim]] (when (nil? claim) claim-name)) claims)))]
    (format "the kept %s %s no longer %s" (if (next gone) "claims" "claim") (str/join ", " gone)
            (if (next gone) "exist" "exists"))))

(defn kept-claim-verdict
  "Pure: how the resize of a kept claim to capacity stands.

   claim is the claim as read now, nil when it does not exist, or :unread;
   events the namespace's events, or :unread; baseline what
   kept-claims-baseline read right before the effect, nil when it was not
   recorded. Returns {:claim :state :line}, :state being

   :rejected  the claim does not exist, requests less than capacity, or
              carries a resize error condition, an infeasible
              allocatedResourceStatus or a resize failure event the baseline
              did not hold;
   :done      the cluster took the resize up: status.capacity is at least
              capacity, status.allocatedResources is at least capacity with
              only the node's expansion left (NodeResizePending,
              NodeResizeInProgress), or a FileSystemResizePending condition
              the baseline did not hold appeared;
   :pending   anything else, among it a claim that only requests capacity,
              a controller resize in progress, evidence carried over from
              before the change, and reads that did not answer.

   Without a baseline nothing on the claim counts as written after the
   change: only its capacity can make it done, and only its absence or its
   request can reject it. Quantities compare as numbers, so 1Ti is 1024Gi."
  [{claim-name :name :keys [capacity claim events baseline]}]
  (let [verdict (fn [state text] {:claim claim-name :state state :line (format "kept claim %s: %s" claim-name text)})
        known? (some? baseline)
        before (get-in baseline [:claims claim-name])
        before-conditions (set (map condition-key (-> before :status :conditions)))
        fresh? #(and known? (not (contains? before-conditions (condition-key %))))
        active (fn [types] (filter #(and (contains? types (:type %)) (not= "False" (:status %)))
                                   (-> claim :status :conditions)))
        errors (active resize-error-conditions)
        statuses (-> claim :status :allocatedResourceStatuses)
        before-statuses (-> before :status :allocatedResourceStatuses)
        infeasible (filter (fn [[_ status]] (contains? infeasible-statuses status)) statuses)
        fresh-infeasible (some (fn [[resource status]] (when (and known? (not= status (get before-statuses resource))) status))
                               infeasible)
        failed-event (when (and known? (sequential? events))
                       (some #(when (and (= "Warning" (:type %))
                                         (contains? resize-failure-reasons (:reason %))
                                         (= "PersistentVolumeClaim" (-> % :involvedObject :kind))
                                         (= claim-name (-> % :involvedObject :name))
                                         (not (contains? (:events baseline) (event-key %))))
                                (str (:reason %) ": " (one-line (:message %))))
                             events))
        requested (-> claim :spec :resources :requests :storage)
        reported (-> claim :status :capacity :storage)
        allocated (-> claim :status :allocatedResources :storage)
        storage-status (get statuses :storage)
        filesystem-pending (first (filter fresh? (active #{"FileSystemResizePending"})))
        age (if known? "from before the change" "of unknown age")
        carried (concat (map #(str "a " (condition-text %) " " age) (remove fresh? errors))
                        (keep (fn [[_ status]] (when-not (= status fresh-infeasible) (str status " " age)))
                              infeasible))]
    (cond
      (= :unread claim) (verdict :pending "could not be read")
      (nil? claim) (verdict :rejected "not found, so it was not resized")
      (not (quantity/at-least? requested capacity))
      (verdict :rejected (format "requests %s, not %s - the resize was not accepted" requested capacity))
      (some fresh? errors) (verdict :rejected (str "resize rejected - " (condition-text (first (filter fresh? errors)))))
      fresh-infeasible (verdict :rejected (str "resize rejected - " fresh-infeasible))
      failed-event (verdict :rejected (str "resize rejected - " failed-event))
      (= :unread events) (verdict :pending "the namespace's events could not be read, so a resize failure could not be ruled out")
      (quantity/at-least? reported capacity) (verdict :done (str "grew to " reported))
      (and (quantity/at-least? allocated capacity) (contains? node-expansion-statuses storage-status))
      (verdict :done (format "the volume grew to %s, its filesystem grows when a pod mounts it (%s)" allocated storage-status))
      filesystem-pending
      (verdict :done "the volume grew, its filesystem grows when a pod mounts it (FileSystemResizePending)")
      :else
      (verdict :pending (str/join ", " (concat [(format "requests %s, the cluster has not taken the resize up (capacity %s%s)"
                                                         requested (or reported "unknown")
                                                         (if storage-status (str ", " storage-status) ""))]
                                               carried
                                               (when-not known? ["the state before the change was not recorded, so no condition on it counts"])))))))

(defn- read-or-unread
  "What read returns, or :unread when a read in it did not answer."
  [read]
  (try (read)
       (catch clojure.lang.ExceptionInfo e
         (if (result/unread-exception? e) :unread (throw e)))))

(defn retained-claims-check
  "The post-check that every claim of claim-names, the kept claims of a
   StatefulSet no pod mounts, took the resize to capacity up, judged by
   kept-claim-verdict over strict reads of each claim and the namespace's
   events against @baseline, the kept-claims-baseline read right before the
   effect. It holds once every claim is :done, ends the wait at once when one
   is :rejected, and otherwise waits; a claim still pending at the timeout
   fails the run, naming how each claim last stood, since a resize the
   cluster never showed taking up is not verified."
  [context namespace claim-names capacity baseline]
  (let [last-verdicts (atom nil)
        lines-of (fn [state] (keep #(when (= state (:state %)) (:line %)) @last-verdicts))]
    {:description (format "the %d kept %s of replicas that do not run (%s) took up the resize to %s"
                          (count claim-names) (if (= 1 (count claim-names)) "claim" "claims")
                          (str/join ", " claim-names) capacity)
     :timeout 300 :interval 10
     :check (fn []
              (let [events (read-or-unread #(kubectl/items-of (kubectl/get-namespaced context namespace "events")))
                    verdicts (mapv (fn [claim-name]
                                     (kept-claim-verdict
                                      {:name claim-name :capacity capacity :events events :baseline @baseline
                                       :claim (read-or-unread #(kubectl/get-namespaced-resource
                                                                context namespace "persistentvolumeclaims" claim-name))}))
                                   claim-names)]
                (reset! last-verdicts verdicts)
                (when (every? #(= :done (:state %)) verdicts)
                  (doseq [{:keys [line]} verdicts]
                    (stdout/err-println (str "  " line)))
                  true)))
     :abort (fn [] (some->> (seq (lines-of :rejected)) (str/join "; ")))
     :detail (fn [] (some->> (seq (concat (lines-of :pending) (lines-of :done))) (str/join "; ")))}))

;; --- a StatefulSet's volumes -----------------------------------------------------

(def statefulset-gone-timeout
  "Seconds the resize waits for an orphan-deleted StatefulSet to be gone
   before it gives up without recreating it. The garbage collector removes
   the orphan finalizer, so a stalled kube-controller-manager keeps the
   StatefulSet for as long as it is stalled."
  120)

(defn statefulset-resize-steps
  "The steps that grow the claims of `template-name` of statefulset to
   capacity: enlarge every claim of plan, delete the StatefulSet with its pods
   orphaned, since a volumeClaimTemplate is immutable, wait until it is gone,
   and apply it again with the new template.

   The delete returns once the API server accepted it (--wait=false, under a
   request timeout), and the wait that follows is bounded by
   statefulset-gone-timeout, so a garbage collector that never removes the
   orphan finalizer stops the run with recovery text instead of hanging it.
   Applying the manifest before the old StatefulSet is gone would patch the
   object being deleted."
  [context statefulset template-name capacity plan]
  (let [namespace (kubectl/namespace-of statefulset)
        name (kubectl/name-of statefulset)
        manifest (json/encode (recreate-manifest statefulset template-name capacity))]
    (conj (resize-claim-steps context namespace plan capacity)
          {:stage :delete
           :effect (deletes/effect context {:type "statefulset" :namespace namespace :name name}
                                   {:cascade :orphan :wait false :extra ["--request-timeout=30s"]})}
          {:stage :gone
           :effect (effect/kubectl context (str "--namespace=" namespace)
                                   "wait" "--for=delete" (str "statefulset/" name)
                                   (str "--timeout=" statefulset-gone-timeout "s"))}
          {:stage :recreate
           :manifest manifest
           :effect (effect/kubectl-stdin context manifest "apply" "--filename" "-")})))

(defn statefulset-state
  "Where the StatefulSet namespace/name stands, for the recovery text of a
   resize that stopped at its delete: :gone when a read answered that it
   does not exist, :deleting when it carries a deletionTimestamp, :present
   when it exists and is not being deleted, :unread when no read answered."
  [context namespace name]
  (binding [kubectl/*strict-reads* false]
    (let [{:keys [ok? doc]} (kubectl/get-json-result context (str "--namespace=" namespace) "statefulsets" name)]
      (cond
        (and ok? (-> doc :metadata :deletionTimestamp)) :deleting
        ok? :present
        :else (case (kubectl/presence context {:type "statefulsets" :namespace namespace :name name})
                :absent :gone
                :unread)))))

(defn- reapply-lines
  [context manifest]
  [(format "  kubectl --context=%s apply --filename - <<'EOF'" context)
   (str "  " manifest)
   "  EOF"])

(defn statefulset-recovery
  "What a run of statefulset-resize-steps left behind after `applied` of its
   effects ran, and what to do about it; nil when none ran. target names the
   StatefulSet as namespace/name, and state is where it stands now, as
   statefulset-state reads it, for a run that stopped at the delete or the
   wait after it: a delete kubectl reported as failed may still have been
   accepted, and then the StatefulSet is being deleted, or gone, and a run of
   the book again finds nothing to enlarge."
  [{:keys [context target state]} steps applied]
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

         (:delete :gone)
         (case state
           :present
           (concat [(format "StatefulSet %s could not be deleted: it still exists with its old volume claim template," target)
                    "and its pods were not touched."]
                   (claims-left where)
                   ["Find why in the kubectl output above, then run this book again with the same capacity."])

           :deleting
           (concat [(format "StatefulSet %s IS BEING DELETED: the API server accepted the delete, and the garbage collector" target)
                    "has not removed it yet. Its pods were orphaned: they keep running, but nothing replaces a pod that stops."
                    "It was not recreated, and running this book again will not find it once it is gone."]
                   (claims-left where)
                   ["Check that kube-controller-manager runs; once the StatefulSet is gone, re-apply it with the new volume claim template:"]
                   (reapply-lines context manifest))

           :gone
           (concat [(format "THE STATEFULSET %s IS DELETED and was not recreated. Its pods were orphaned:" target)
                    "they keep running, but nothing replaces a pod that stops."]
                   (claims-left where)
                   ["Re-apply it now, with the new volume claim template:"]
                   (reapply-lines context manifest))

           (concat [(format "Whether StatefulSet %s still exists is unknown: the delete may have been accepted, and the read after it did not answer." target)]
                   (claims-left where)
                   [(let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
                      (format "Read it with: kubectl --context=%s --namespace=%s get statefulset %s" context namespace name))
                    "If it is still there without a deletionTimestamp, run this book again with the same capacity."
                    "If it is gone or being deleted, re-apply it once it is gone, with the new volume claim template:"]
                   (reapply-lines context manifest)))

         :recreate
         (concat [(format "THE STATEFULSET %s IS DELETED and could not be recreated. Its pods were orphaned:" target)
                  "they keep running, but nothing replaces a pod that stops."]
                 (claims-left where)
                 ["Re-apply it now, with the new volume claim template:"]
                 (reapply-lines context manifest))

         (concat [(format "Every effect ran: StatefulSet %s was recreated with the new volume claim template." target)]
                 (claims-left where)
                 ["Nothing was rolled back. The post-check messages above name what did not hold."]))))))

(defn template-requests?
  "Whether statefulset carries a volumeClaimTemplate called template-name
   that requests capacity."
  [statefulset template-name capacity]
  (boolean
   (some #(and (= template-name (-> % :metadata :name))
               (quantity/same? capacity (-> % :spec :resources :requests :storage)))
         (-> statefulset :spec :volumeClaimTemplates))))

(defn statefulset-ready?
  "Whether the StatefulSet controller caught up with statefulset: it observed
   the current generation, and as many replicas are ready as the spec asks
   for. The API server omits a zero readyReplicas and defaults spec.replicas
   to 1. A nil statefulset - NotFound, or a lax read that did not answer - is
   never ready."
  [statefulset]
  (verify/object-holds?
   statefulset
   (fn [{:keys [metadata status] :as object}]
     (and (>= (or (:observedGeneration status) 0) (or (:generation metadata) 0))
          (= (workloads/desired-replicas "StatefulSet" object) (or (:readyReplicas status) 0))))))

(defn statefulset-resized?
  "Whether statefulset exists with the template template-name requesting
   capacity and every replica ready (statefulset-ready?): what a resize that
   deleted the StatefulSet with orphaned pods needs to see before it is done,
   because the orphaned pods keep running whether or not anything recreates
   their controller."
  [statefulset template-name capacity]
  (verify/object-holds? statefulset #(and (template-requests? % template-name capacity)
                                          (statefulset-ready? %))))

(defn- statefulset-state-checks
  [context namespace name template-name capacity]
  [{:description (format "StatefulSet %s/%s exists again with the new template" namespace name)
    :timeout 60
    :check (fn []
             (template-requests? (kubectl/get-namespaced-resource context namespace "statefulsets" name)
                                 template-name capacity))}
   {:description "every pod of the StatefulSet is Running"
    :timeout 300
    :interval 10
    :check (fn []
             (statefulset-ready? (kubectl/get-namespaced-resource context namespace "statefulsets" name)))}])

(defn statefulset-checks
  "The post-checks of a StatefulSet's resize: the claims of the running
   replicas report capacity, the kept claims among plan - the names in
   retained, which no pod mounts - took the resize up (see
   retained-claims-check, judged against @baseline, the
   kept-claims-baseline read right before the effect; without one no
   condition on a kept claim counts, only its capacity), the StatefulSet
   exists again with the new template, and every replica is ready. Without
   retained every claim of plan must report capacity."
  ([context cluster namespace name template-name capacity plan]
   (statefulset-checks context cluster namespace name template-name capacity plan nil nil))
  ([context cluster namespace name template-name capacity plan retained]
   (statefulset-checks context cluster namespace name template-name capacity plan retained nil))
  ([context cluster namespace name template-name capacity plan retained baseline]
   (let [kept (kept-claim-names plan retained)
         running (vec (remove #(contains? (set kept) (:pvc %)) plan))]
     (vec
      (concat
       (when (seq running) [(claims-capacity-check context cluster namespace running capacity)])
       (when (seq kept) [(retained-claims-check context namespace kept capacity (or baseline (atom nil)))])
       (statefulset-state-checks context namespace name template-name capacity))))))
