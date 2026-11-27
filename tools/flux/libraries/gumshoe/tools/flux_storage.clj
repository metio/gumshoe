;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.flux-storage
  "Clearing a HelmRelease's Helm storage, so helm-controller's next action is
   an install, as the steps of one playbook.

   helm-controller decides between install and upgrade by what Helm's storage
   holds: with no revision of the release it installs. Deleting the storage
   while anything can still act on the release races that decision, so the
   order matters:

   1. the object that renders the HelmRelease is suspended first, so it cannot
      re-apply the HelmRelease while it is changed
   2. the HelmRelease is suspended, and the steps wait for an action already
      running to finish; a pending revision younger than spec.timeout is a
      running action, and every deleting step refuses while one exists
   3. the release's workloads go, then its volumes, so no pod still mounts a
      claim being deleted
   4. the storage goes, and is confirmed empty before anything resumes
   5. the HelmRelease resumes and installs, then its owner resumes

   The owner is the Kustomization or ResourceSet whose status.inventory lists
   the HelmRelease. The workloads and volumes are the ones Helm stamped with
   this release's meta.helm.sh annotations, plus the claims a released
   StatefulSet made from its volumeClaimTemplates. Every wait derives from live
   values: the HelmRelease's spec.timeout and spec.interval and the owner's
   reconcile interval.

   Every step from the owner's suspension to its resume, and the playbook
   for a caller's steps, carries `recovery`: a failure prints which of the
   HelmRelease and its owner stay suspended and how to resume them.

   Helm's storage is read in the release's storage namespace only, selected
   by owner=helm and the release's name label, so the steps need `list` on
   Secrets in that namespace and nowhere else. Deleting the storage needs
   `delete` on Secrets there too; that step's :kubectl-can-delete prerequisite
   checks it across every namespace, because a prerequisite cannot name a
   namespace the HelmRelease decides, and a missing delete right must stop
   the run before the workloads and volumes go.

   A release can own objects Helm never stamped, which the steps cannot find.
   A casebook adds its own steps for them at one of `positions`, named for the
   step they follow, through (steps extra) or (playbook-with extra).

   The steps are built from the shared step libraries: gumshoe.steps.flux
   suspends and resumes the owner and the HelmRelease, gumshoe.steps.objects
   deletes the workloads and volumes, gumshoe.change/compose combines each
   builder's part with the step's own keys and the observation every change
   step shares, and gumshoe.playbook places a caller's steps at the
   positions."
  (:require [clojure.string :as str]
            [gumshoe.change :as change]
            [gumshoe.deletes :as deletes]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.playbook :as playbook]
            [gumshoe.select :as select]
            [gumshoe.shell :as shell]
            [gumshoe.steps.flux :as flux-steps]
            [gumshoe.steps.objects :as objects]
            [gumshoe.stdout :as stdout]
            [gumshoe.tools.flux :as flux]))

(def resourceset-type flux-steps/resourceset-type)

(def ^:private reconcile-every-annotation :fluxcd.controlplane.io/reconcileEvery)

(def ^:private default-resourceset-interval
  "How often a ResourceSet reconciles without a reconcileEvery annotation."
  "1h")

;; --- the owner ---------------------------------------------------------------------

(defn inventory-id
  "The id a Kustomization or ResourceSet inventory records for this HelmRelease:
   <namespace>_<name>_<group>_<kind>."
  [release]
  (format "%s_%s_helm.toolkit.fluxcd.io_HelmRelease"
          (kubectl/namespace-of release) (kubectl/name-of release)))

(def ^:private owner-types
  {"Kustomization" flux/kustomization-type
   "ResourceSet" resourceset-type})

(defn owners
  "Every Kustomization and ResourceSet among `candidates` (kind -> objects)
   whose inventory lists `release`, as {:kind :type :namespace :name :object}."
  [release candidates]
  (let [id (inventory-id release)]
    (vec (for [[kind objects] (sort-by key candidates)
               object objects
               :when (some #(= id (:id %)) (-> object :status :inventory :entries))]
           {:kind kind :type (owner-types kind)
            :namespace (kubectl/namespace-of object) :name (kubectl/name-of object)
            :object object}))))

(defn can-patch-owner?
  "Whether the current user may patch the owner {:type :namespace :name}
   itself, as kubectl auth can-i answers for that one object. Any answer but
   yes, an unreachable API server included, is no."
  [context {:keys [type namespace name]}]
  (zero? (shell/exit-code-of "kubectl" (str "--context=" context) "auth" "can-i" "patch"
                             (str type "/" name) (str "--namespace=" namespace) "--quiet")))

(defn owner-patch-refusal
  "Why the owner {:kind :type :namespace :name} cannot be suspended or
   resumed by this user, judged by `can-patch?` (owner -> boolean), or nil.
   The owner is a Kustomization or a ResourceSet, so a prerequisite fixed to
   one kind would check the wrong rights for the other; this asks for the
   object found. No owner needs no rights."
  [can-patch? {:keys [kind namespace name] :as owner}]
  (when (and owner (not (can-patch? owner)))
    (format "you may not patch %s %s/%s, or it could not be confirmed (kubectl auth can-i patch %s/%s --namespace=%s) - it could be neither suspended nor resumed"
            kind namespace name (:type owner) name namespace)))

(defn owner-interval-seconds
  "How often the owner reconciles: a Kustomization's spec.interval, a
   ResourceSet's reconcileEvery annotation or its hourly default."
  [{:keys [kind object]}]
  (case kind
    "ResourceSet" (flux/duration-seconds
                   (or (get-in object [:metadata :annotations reconcile-every-annotation])
                       default-resourceset-interval))
    (flux/interval-seconds object)))

(defn owner-suspended?
  "Whether the owner is told not to reconcile: spec.suspend on a Kustomization,
   the reconcile annotation set to disabled on a ResourceSet."
  [kind object]
  (flux-steps/suspended? kind object))

(defn owner-suspend-effect
  "Suspends or resumes the owner. A resume also asks for a reconcile at
   timestamp, so the owner catches up at once rather than at its next interval."
  [context owner wanted timestamp]
  (flux-steps/suspend-effect context owner wanted timestamp))

;; --- what the release owns -----------------------------------------------------------

(def workload-types ["deployments" "statefulsets" "daemonsets" "jobs" "cronjobs"])

(defn- released-by?
  [release-name release-namespace object]
  (let [annotations (-> object :metadata :annotations)]
    (and (= release-name (:meta.helm.sh/release-name annotations))
         (= release-namespace (:meta.helm.sh/release-namespace annotations)))))

(defn release-workloads
  "The workloads Helm recorded for the release, from `lists` (type -> objects),
   as {:type :namespace :name :object}."
  [release-name release-namespace lists]
  (vec (for [[type objects] lists
             object objects
             :when (released-by? release-name release-namespace object)]
         {:type type :namespace (kubectl/namespace-of object) :name (kubectl/name-of object)
          :object object})))

(defn- claimed-from-template?
  "A claim a StatefulSet made from a volumeClaimTemplate is named
   <template>-<statefulset>-<ordinal>."
  [claim-name statefulsets]
  (some (fn [sts]
          (some #(re-matches (re-pattern (str (java.util.regex.Pattern/quote
                                               (str (-> % :metadata :name) "-" (kubectl/name-of sts) "-"))
                                              "\\d+"))
                             claim-name)
                (-> sts :spec :volumeClaimTemplates)))
        statefulsets))

(defn release-volumes
  "The claims Helm recorded for the release, and those its StatefulSets made
   from their templates, as {:type :namespace :name}."
  [release-name release-namespace pvcs statefulsets]
  (vec (for [pvc pvcs
             :when (or (released-by? release-name release-namespace pvc)
                       (claimed-from-template? (kubectl/name-of pvc) statefulsets))]
         {:type "persistentvolumeclaims" :namespace (kubectl/namespace-of pvc) :name (kubectl/name-of pvc)})))

;; --- reading the cluster ----------------------------------------------------------------

(defn- by-name [objects]
  (into {} (map (juxt kubectl/name-of identity)) objects))

(defn- strict
  "The value of (read) with kubectl/*strict-reads* bound, so a read that did
   not answer throws ::kubectl/unread where it would have returned the empty
   result of a read that found nothing. A resource type the cluster does not
   serve still reads as empty."
  [read]
  (binding [kubectl/*strict-reads* true] (read)))

(defn- owner-candidates
  [context]
  {"Kustomization" (kubectl/items-of (kubectl/get-all context flux/kustomization-type))
   "ResourceSet" (kubectl/items-of (kubectl/get-all context resourceset-type))})

(defn observe
  "Everything the steps decide on, read once per step: the HelmRelease, its
   owners, the release's workloads and volumes. nil when the HelmRelease
   does not exist, {:unread <reason>} when any read did not answer. Every
   read runs strict, because each step decides on an absence - no workload,
   no claim, no owner - and an unanswered read must not stand in for one.

   `seen` holds the release's StatefulSets an earlier step found. A claim made
   from a volumeClaimTemplate carries no Helm annotation, so once the workloads
   step deleted its StatefulSet only `seen` still ties it to the release."
  ([context target] (observe context target nil))
  ([context target seen]
   (try
     (strict
      (fn []
        (let [{:keys [namespace name]} (kubectl/namespace-name-map target)]
          (when-let [release (kubectl/get-namespaced-resource context namespace flux/helmrelease-type name)]
            (let [release-name (flux/release-name release)
                  release-namespace (flux/release-namespace release)
                  lists (into {} (for [type workload-types]
                                   [type (kubectl/items-of (kubectl/get-namespaced context release-namespace type))]))
                  workloads (release-workloads release-name release-namespace lists)
                  live (map :object (filter #(= "statefulsets" (:type %)) workloads))
                  statefulsets (vec (vals (merge (by-name seen) (by-name live))))
                  pvcs (kubectl/items-of (kubectl/get-namespaced context release-namespace "persistentvolumeclaims"))
                  found (owners release (owner-candidates context))]
              {:release release
               :release-name release-name
               :release-namespace release-namespace
               :storage-namespace (flux/storage-namespace release)
               :owners found
               :owner (when (= 1 (count found)) (first found))
               :workloads (mapv #(dissoc % :object) workloads)
               :statefulsets statefulsets
               :volumes (release-volumes release-name release-namespace pvcs statefulsets)})))))
     (catch clojure.lang.ExceptionInfo e
       (if (::kubectl/unread (ex-data e))
         {:unread (ex-message e)}
         (throw e))))))

(defn observer
  "An observe for one run of the steps: it carries the release's StatefulSets
   and its owner, per target, from each observation that answered into the
   next. Every observation carries the owner the run last saw as :known-owner,
   which is the owner the run suspended, so the resume reaches it even when
   the owners cannot be read at that moment."
  []
  (let [seen (atom {})]
    (fn [context target]
      (let [observed (observe context target (:statefulsets (get @seen target)))]
        (when (and observed (not (:unread observed)))
          (swap! seen update target
                 (fn [{:keys [owner]}]
                   {:statefulsets (:statefulsets observed)
                    :owner (or (:owner observed) owner)})))
        (some-> observed (assoc :known-owner (:owner (get @seen target))))))))

(defn waits
  "Every wait of the playbook, in seconds, from live values: the HelmRelease's
   spec.timeout bounds one Helm action and one deletion; the release becomes
   Ready within its timeout plus its interval, in case the reconcile request is
   not honoured; the owner within its own interval plus the release's timeout."
  [{:keys [release owner]}]
  (let [timeout (flux/helm-timeout-seconds release)
        interval (or (flux/interval-seconds release) timeout)
        owner-interval (if owner (or (owner-interval-seconds owner) interval) 0)]
    {:in-flight timeout
     :deletion timeout
     :release-ready (+ timeout interval)
     :owner-ready (+ owner-interval timeout)}))

(defn- now-seconds [] (quot (System/currentTimeMillis) 1000))

(defn- revisions
  "The release's revisions in its storage namespace, or nil when the storage
   cannot be read. The read names the storage namespace and the release's name
   label, so it lists a handful of Secrets in one namespace however many
   releases the cluster holds, and needs list rights on Secrets there only.
   The rows are still matched on both, so a listing that answered with more
   than was asked for cannot lend this release another one's revision."
  [context {:keys [release-name storage-namespace]}]
  (some->> (kubectl/helm-releases context storage-namespace release-name)
           (filterv #(and (= release-name (:release %)) (= storage-namespace (:namespace %))))))

(defn- quiet?
  "No Helm action is running on the release. An unreadable storage counts as
   not quiet, because a deleting step must not proceed on a guess."
  [context {:keys [release] :as observed}]
  (let [rows (revisions context observed)]
    (and (some? rows) (not (flux/action-in-flight? rows release (now-seconds))))))

(defn- object-lines [objects]
  (map objects/line objects))

;; --- recovery ------------------------------------------------------------------------

(defn- resume-line
  "One suspended object and the command that resumes it, as the steps do."
  [context {:keys [kind type namespace name]}]
  (format "  %s %s/%s - resume: kubectl --context=%s -n %s %s"
          kind namespace name context namespace
          (if (= "ResourceSet" kind)
            (format "annotate %s %s --overwrite fluxcd.controlplane.io/reconcile=enabled" type name)
            (format "patch %s %s --type=merge --patch='{\"spec\":{\"suspend\":false}}'" type name))))

(defn recovery-text
  "What a failed step leaves behind, from an observation taken at the
   failure: which of the HelmRelease and its owner are still suspended, the
   command that resumes each, and the order to resume them in."
  [context target {:keys [release owner unread]}]
  (let [suspended (concat
                   (when (true? (-> release :spec :suspend))
                     [{:kind "HelmRelease" :type flux/helmrelease-type
                       :namespace (kubectl/namespace-of release) :name (kubectl/name-of release)}])
                   (when (and owner (owner-suspended? (:kind owner) (:object owner)))
                     [owner]))]
    (cond
      unread
      (str "The cluster did not answer (" unread "), so whether HelmRelease " target
           " and its owner are still suspended is unknown. Read both before leaving them:"
           " while either is suspended, Flux does not reconcile the release.")

      (nil? release)
      (str "HelmRelease " target " cannot be read. Check whether the Kustomization or ResourceSet"
           " that rendered it is still suspended.")

      (empty? suspended)
      (str "Neither HelmRelease " target " nor its owner is suspended; Flux reconciles the release again.")

      :else
      (str/join "\n"
                (concat
                 [(str "Flux does not reconcile HelmRelease " target " while these stay suspended:")]
                 (map #(resume-line context %) suspended)
                 ["The release's workloads, volumes and Helm storage stay as the steps before this one left them."
                  "Resume the HelmRelease once its Helm storage no longer holds the revision that blocked it, then the owner."
                  "Running this playbook again repeats every step, but no longer finds the claims a StatefulSet it"
                  "already deleted made from its volumeClaimTemplates; delete those by hand."])))))

(defn recovery
  "The :recovery of every step from the owner's suspension to its resume, and
   of the playbook: it reads the HelmRelease and its owner again at the
   failure."
  [{:keys [context target]}]
  (recovery-text context target (observe context target)))

;; --- the steps ---------------------------------------------------------------------

(defn- observing
  "The part every change step shares: the value :observed, a fresh
   observation taken once per step, and the step's :recovery. It is a value
   rather than a read, so each step's precondition judges the observation
   its confirmation showed, with only the pinned HelmRelease read again."
  [observe!]
  {:values {:observed (fn [{:keys [context target]}] (observe! context target))}
   :recovery recovery})

(def ^:private release-pin
  (fn [{:keys [observed]}]
    (when-let [release (:release observed)]
      {:type flux/helmrelease-type :namespace (kubectl/namespace-of release) :name (kubectl/name-of release)})))

(defn- release-object
  "The observed HelmRelease as gumshoe.steps.flux names an object."
  [{:keys [observed]}]
  (when-let [release (:release observed)]
    {:kind "HelmRelease" :type flux/helmrelease-type
     :namespace (kubectl/namespace-of release) :name (kubectl/name-of release)}))

(defn- wait-of
  "The step's wait named `k`, from its observation."
  [k]
  (fn [{:keys [observed]}] (k (waits observed))))

(defn- fresh-install?
  "The release's storage holds one revision, 1, deployed: helm-controller
   installed it afresh."
  [context observed]
  (let [rows (revisions context observed)]
    (and (= 1 (count rows))
         (= 1 (:version (first rows)))
         (= "deployed" (:status (first rows))))))

(defn- readable?
  "Whether the step's observation answered, saying why not when it did not.
   A step acts only on what it read, so it refuses on a read that did not
   answer instead of taking it for an empty one."
  [{:keys [release unread]}]
  (cond
    unread (do (stdout/error (str "refusing: the cluster did not answer - " unread)) false)
    (nil? release) (do (stdout/error "refusing: the HelmRelease cannot be read") false)
    :else true))

(defn- unless-unread
  "The step's confirmation items, or why its observation is unknown."
  [items]
  (fn [{:keys [observed] :as ctx}]
    (if-let [why (:unread observed)]
      [(str "unknown - the cluster did not answer: " why)]
      (items ctx))))

(defn- deletable?
  "The :precondition of every deleting step: its observation answered and no
   Helm action is running on the release."
  [{:keys [context observed]}]
  (and (readable? observed) (quiet? context observed)))

(defn- report!
  "Prints what the playbook found and answers whether it may go on."
  [context {:keys [release owners owner workloads volumes] :as observed}]
  (let [rows (when release (revisions context observed))]
    (cond
      (:unread observed)
      (do (stdout/error (str "the cluster did not answer, so the release's owner and objects are unknown - "
                             (:unread observed)))
          false)

      (nil? release)
      (do (stdout/error "the HelmRelease cannot be read") false)

      (nil? rows)
      (do (stdout/error "the Helm storage cannot be read, so whether an action is running is unknown") false)

      (< 1 (count owners))
      (do (stdout/error (str "more than one object lists the HelmRelease in its inventory: "
                             (str/join ", " (map #(str (:kind %) " " (:namespace %) "/" (:name %)) owners))))
          false)

      (flux/action-in-flight? rows release (now-seconds))
      (do (stdout/error (format "a Helm action on %s is running (a pending revision younger than spec.timeout, %ds) - refusing to act"
                                (:release-name observed) (flux/helm-timeout-seconds release)))
          false)

      :else
      (do (println (format "owner:     %s" (if owner (str (:kind owner) " " (:namespace owner) "/" (:name owner))
                                              "none - no Kustomization or ResourceSet lists it")))
          (println (format "storage:   %d revision(s) of %s in %s, statuses %s"
                           (count rows) (:release-name observed) (:storage-namespace observed)
                           (str/join ", " (distinct (map :status rows)))))
          (println (format "workloads: %s" (or (not-empty (str/join ", " (object-lines workloads))) "none")))
          (println (format "volumes:   %s" (or (not-empty (str/join ", " (object-lines volumes))) "none")))
          (println (format "waits:     %s" (pr-str (waits observed))))
          true))))

;; Secrets are absent from :kubectl-can-get on purpose. The storage is read
;; with `list` in its storage namespace, which is known only once the
;; HelmRelease is read, and a :kubectl-can-get prerequisite checks `get`
;; across every namespace before that; it would refuse an operator who holds
;; exactly the rights the read needs. The report's own storage read is the
;; check: it runs before any effect, and refuses when it does not answer.
(def ^:private reads
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities [:flux]
   :kubectl-can-get (into [flux/helmrelease-type flux/kustomization-type "persistentvolumeclaims"]
                          workload-types)})

(defn- anchored
  "The playbook's steps in the order they must run, each paired with the
   position a caller's steps take right after it, or nil where none may go."
  []
  (let [observe! (observer)
        shared (observing observe!)
        change (fn [& parts] (apply change/compose shared parts))]
    [[nil
      {:title "🔬 Read the HelmRelease, its owner and its storage"
       :prerequisites reads
       :check (fn [{:keys [context target]}] (report! context (observe! context target)))}]

     [nil
      (change
       {:title "⏸  Suspend the owner (Kustomization or ResourceSet) that renders the HelmRelease"
        ;; the rights are checked on the owner found, whose kind varies
        :prerequisites {}
        :refusal (fn [{:keys [context observed]}]
                   (owner-patch-refusal #(can-patch-owner? context %) (:owner observed)))
        :confirm {:action "suspend the owner that renders the HelmRelease" :destructive? false}
        :precondition (fn [{:keys [observed]}] (readable? observed))
        :items (unless-unread
                (fn [{:keys [observed]}]
                  [(if-let [{:keys [kind namespace name]} (:owner observed)]
                     (format "suspend %s %s/%s, so it does not re-apply the HelmRelease meanwhile" kind namespace name)
                     "no Kustomization or ResourceSet lists the HelmRelease - nothing to suspend")]))}
       (flux-steps/suspend {:object (comp :owner :observed) :none "no owner to suspend"
                            :timeout (wait-of :deletion) :interval 5}))]

     [:after-suspend
      (change
       {:title "⏸  Suspend the HelmRelease and wait for a running action to finish"
        :prerequisites {:kubectl-can-patch [flux/helmrelease-type]}
        :confirm {:action "suspend the HelmRelease" :destructive? false}
        :pin release-pin
        :precondition (fn [{:keys [observed]}] (readable? observed))
        :items (unless-unread
                (fn [{:keys [target observed]}]
                  [(format "suspend HelmRelease %s and wait up to %ds (spec.timeout) for a running Helm action to end"
                           target (:in-flight (waits observed)))]))}
       (flux-steps/suspend {:object release-object :none "no HelmRelease to suspend"
                            :timeout (wait-of :in-flight) :interval 5
                            :description "the HelmRelease is suspended and no Helm action is running"
                            :also (fn [{:keys [context observed]} _] (quiet? context observed))}))]

     [:after-workloads
      (-> (change
           {:title "🗑  Delete the release's workloads"
            :prerequisites {:kubectl-can-delete workload-types}
            :confirm {:action "delete the release's workloads" :destructive? true}
            :precondition deletable?}
           (objects/delete {:objects (comp :workloads :observed) :none "the release has no workloads"
                            :timeout (wait-of :deletion)}))
          (update :items unless-unread))]

     [:after-volumes
      (-> (change
           {:title "🗑  Delete the release's volumes"
            :prerequisites {:kubectl-can-delete ["persistentvolumeclaims"]}
            :confirm {:action "delete the release's volumes - their data is lost" :destructive? true :separately? true}
            :precondition deletable?}
           (objects/delete {:objects (comp :volumes :observed) :none "the release has no volumes"
                            :timeout (wait-of :deletion)
                            :item #(str "delete " % " and the data on it")}))
          (update :items unless-unread))]

     [:after-storage
      (change
       {:title "🗑  Delete the Helm storage and confirm it is empty"
        :prerequisites {:kubectl-can-delete ["secrets"]}
        :confirm {:action "delete the release's Helm storage" :destructive? true}
        :precondition deletable?
        :items (unless-unread
                (fn [{:keys [observed]}]
                  [(format "delete every Helm revision Secret of %s in %s, so the next action is an install"
                           (:release-name observed) (:storage-namespace observed))]))
        :effect (fn [{:keys [context observed]}]
                  (effect/plan
                   (deletes/effect context {:type "secrets" :namespace (:storage-namespace observed)
                                            :selector (str "owner=helm,name=" (:release-name observed))})))
        :verify (fn [{:keys [context observed]}]
                  [{:description "the release's Helm storage is empty"
                    :timeout (:deletion (waits observed)) :interval 5 :verifies-absence? true
                    :check #(= [] (revisions context observed))}])})]

     [nil
      (change
       {:title "▶  Resume the HelmRelease, which installs"
        :prerequisites {:kubectl-can-patch [flux/helmrelease-type]}
        :confirm {:action "resume the HelmRelease" :destructive? false}
        :precondition (fn [{:keys [observed]}] (readable? observed))
        :items (fn [{:keys [target]}] [(format "resume HelmRelease %s and request a reconcile, which installs it" target)])}
       (flux-steps/resume {:object release-object :none "no HelmRelease to resume"
                           :timeout (wait-of :release-ready) :interval 15
                           :description "the HelmRelease is Ready on a fresh install (revision 1, deployed)"
                           :also (fn [{:keys [context observed]} _] (fresh-install? context observed))}))]

     [:after-resume
      (change
       {:title "▶  Resume the owner (Kustomization or ResourceSet)"
        :prerequisites {}
        :refusal (fn [{:keys [context observed]}]
                   (owner-patch-refusal #(can-patch-owner? context %) (:known-owner observed)))
        :confirm {:action "resume the owner that renders the HelmRelease" :destructive? false}
        :precondition (fn [{:keys [observed]}] (or (some? (:known-owner observed)) (readable? observed)))
        :items (fn [{:keys [observed]}]
                 [(if-let [{:keys [kind namespace name]} (:known-owner observed)]
                    (format "resume %s %s/%s and request a reconcile" kind namespace name)
                    "no owner to resume")])}
       (flux-steps/resume {:object (comp :known-owner :observed) :none "no owner to resume"
                           :timeout (fn [{:keys [observed]}]
                                      (:owner-ready (waits (assoc observed :owner (:known-owner observed)))))
                           :interval 15}))]]))

(def positions
  "Where a caller's steps may go, in run order, each named for the step it
   follows: the HelmRelease is suspended, its workloads are deleted, its
   volumes are deleted, its storage is deleted and confirmed empty, the owner
   is resumed. Every position but :after-resume lies before the HelmRelease
   resumes, so a step there runs while nothing can act on the release."
  (playbook/positions (anchored)))

(defn- refuse
  "Why a caller's step cannot go at a position: a destructive step at
   :after-resume would act on the fresh install."
  [position step]
  (when (and (= :after-resume position) (playbook/destructive? step))
    (format "a destructive step cannot go at :after-resume, after the HelmRelease installed afresh: %s"
            (pr-str (:title step)))))

(defn insert-at
  "The steps of `anchored`, a vector of [position step], with the steps
   `extra` holds for a position right after the step that position names.
   Only the position names are read, never a title. Kept for callers of this
   namespace; it is gumshoe.playbook/insert-at."
  [anchored extra]
  (playbook/insert-at anchored extra))

(defn steps
  "The playbook's steps, in the order they must run. `extra` maps a name from
   `positions` to the steps a caller adds there, such as objects of the
   release that carry no meta.helm.sh annotations:

     (steps {:after-volumes [delete-database-step]})

   An unknown position, or a destructive step at :after-resume, throws when
   the steps are built, so a book that asks for one fails as it loads."
  ([] (steps nil))
  ([extra]
   (playbook/with-extra (anchored) extra {:of "the flux-storage steps" :refuse refuse})))

(defn helmrelease-picks
  [context]
  (mapv kubectl/namespace-name-of (kubectl/items-of (kubectl/get-all context flux/helmrelease-type))))

(def helmrelease-select
  {:mode :namespaced :label "HelmRelease" :namespace-flag :namespace :name-flag :name
   :candidates helmrelease-picks})

(def playbook
  {:description "Clears a HelmRelease's Helm storage so the next action is an install"
   :options (select/options helmrelease-select)
   :select helmrelease-select
   :confirm {:action "suspend its owner, delete the release's workloads, volumes and Helm storage, then resume it to install afresh"
             :destructive? true}
   :recovery recovery
   :steps (steps)})

(defn playbook-with
  "The playbook with the caller's steps at named positions, as `steps` takes
   them: (playbook-with {:after-volumes [delete-database-step]})."
  [extra]
  (assoc playbook :steps (steps extra)))
