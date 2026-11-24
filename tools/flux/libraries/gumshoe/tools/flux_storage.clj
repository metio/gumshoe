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
   reconcile interval."
  (:require [clojure.string :as str]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.stdout :as stdout]
            [gumshoe.tools.flux :as flux]))

(def resourceset-type "resourcesets.fluxcd.controlplane.io")

(def ^:private reconcile-annotation :fluxcd.controlplane.io/reconcile)
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
  (case kind
    "ResourceSet" (= "disabled" (get-in object [:metadata :annotations reconcile-annotation]))
    (true? (-> object :spec :suspend))))

(defn owner-suspend-effect
  "Suspends or resumes the owner. A resume also asks for a reconcile at
   timestamp, so the owner catches up at once rather than at its next interval."
  [context {:keys [kind type namespace name] :as owner} wanted timestamp]
  (case kind
    "ResourceSet"
    (effect/plan
     (effect/kubectl context "-n" namespace "annotate" type name "--overwrite"
                     (str "fluxcd.controlplane.io/reconcile=" (if wanted "disabled" "enabled")))
     (when-not wanted
       (effect/kubectl context "-n" namespace "annotate" type name "--overwrite"
                       (str "reconcile.fluxcd.io/requestedAt=" timestamp))))
    (flux/suspend-effect context owner wanted timestamp)))

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
        (let [{:keys [namespace name]} (kubectl/split-namespace-name target)]
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
   cannot be read."
  [context {:keys [release-name storage-namespace]}]
  (some->> (kubectl/helm-releases context)
           (filterv #(and (= release-name (:release %)) (= storage-namespace (:namespace %))))))

(defn- quiet?
  "No Helm action is running on the release. An unreadable storage counts as
   not quiet, because a deleting step must not proceed on a guess."
  [context {:keys [release] :as observed}]
  (let [rows (revisions context observed)]
    (and (some? rows) (not (flux/action-in-flight? rows release (now-seconds))))))

(defn- ready?
  [object]
  (some #(and (= "Ready" (:type %)) (= "True" (:status %))) (-> object :status :conditions)))

(defn- fetch-release [context {:keys [release]}]
  (kubectl/get-namespaced-resource context (kubectl/namespace-of release)
                                   flux/helmrelease-type (kubectl/name-of release)))

(defn- fetch-owner [context {:keys [type namespace name]}]
  (kubectl/get-namespaced-resource context namespace type name))

(defn- delete-effect
  [context objects nothing]
  (if (empty? objects)
    (effect/plan (effect/note nothing))
    (vec (for [{:keys [type namespace name]} objects]
           (effect/kubectl context "-n" namespace "delete" type name
                           "--ignore-not-found" "--wait=false")))))

(defn- gone-checks
  [context objects timeout]
  (for [{:keys [type namespace name]} objects]
    {:description (format "%s %s/%s is gone" type namespace name)
     :timeout timeout :interval 5
     :check #(kubectl/confirmed-gone? context namespace type name)}))

(defn- object-lines [objects]
  (map (fn [{:keys [type namespace name]}] (format "%s %s/%s" type namespace name)) objects))

;; --- the steps ---------------------------------------------------------------------

(defn- deriver
  "The :derive of every change step: a fresh observation and a timestamp."
  [observe!]
  (fn [{:keys [context target]}]
    {:observed (observe! context target)
     :timestamp (str (java.time.Instant/now))}))

(def ^:private release-pin
  (fn [{:keys [observed]}]
    (when-let [release (:release observed)]
      {:type flux/helmrelease-type :namespace (kubectl/namespace-of release) :name (kubectl/name-of release)})))

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

(def ^:private reads
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities [:flux]
   :kubectl-can-get (into [flux/helmrelease-type flux/kustomization-type "secrets" "persistentvolumeclaims"]
                          workload-types)})

(defn steps
  "The playbook's steps, in the order they must run."
  []
  (let [observe! (observer)
        derive-step (deriver observe!)]
    [{:title "🔬 Read the HelmRelease, its owner and its storage"
      :prerequisites reads
      :check (fn [{:keys [context target]}] (report! context (observe! context target)))}

     {:title "⏸  Suspend the owner (Kustomization or ResourceSet) that renders the HelmRelease"
      :prerequisites {:kubectl-can-patch [flux/kustomization-type]}
      :confirm {:action "suspend the owner that renders the HelmRelease"}
      :derive derive-step
      :precondition (fn [{:keys [observed]}] (readable? observed))
      :items (unless-unread
              (fn [{:keys [observed]}]
                [(if-let [{:keys [kind namespace name]} (:owner observed)]
                   (format "suspend %s %s/%s, so it does not re-apply the HelmRelease meanwhile" kind namespace name)
                   "no Kustomization or ResourceSet lists the HelmRelease - nothing to suspend")]))
      :effect (fn [{:keys [context observed timestamp]}]
                (if-let [owner (:owner observed)]
                  (owner-suspend-effect context owner true timestamp)
                  (effect/plan (effect/note "no owner to suspend"))))
      :verify (fn [{:keys [context observed]}]
                (when-let [owner (:owner observed)]
                  [{:description (format "%s %s/%s is suspended" (:kind owner) (:namespace owner) (:name owner))
                    :timeout (:deletion (waits observed)) :interval 5
                    :check #(owner-suspended? (:kind owner) (fetch-owner context owner))}]))}

     {:title "⏸  Suspend the HelmRelease and wait for a running action to finish"
      :prerequisites {:kubectl-can-patch [flux/helmrelease-type]}
      :confirm {:action "suspend the HelmRelease"}
      :derive derive-step
      :pin release-pin
      :precondition (fn [{:keys [observed]}] (readable? observed))
      :items (unless-unread
              (fn [{:keys [target observed]}]
                [(format "suspend HelmRelease %s and wait up to %ds (spec.timeout) for a running Helm action to end"
                         target (:in-flight (waits observed)))]))
      :effect (fn [{:keys [context observed timestamp]}]
                (let [release (:release observed)]
                  (flux/suspend-effect context {:type flux/helmrelease-type
                                                :namespace (kubectl/namespace-of release)
                                                :name (kubectl/name-of release)}
                                       true timestamp)))
      :verify (fn [{:keys [context observed]}]
                [{:description "the HelmRelease is suspended and no Helm action is running"
                  :timeout (:in-flight (waits observed)) :interval 5
                  :check #(and (true? (-> (fetch-release context observed) :spec :suspend))
                               (quiet? context observed))}])}

     {:title "🗑  Delete the release's workloads"
      :prerequisites {:kubectl-can-delete workload-types}
      :confirm {:action "delete the release's workloads" :destructive? true}
      :derive derive-step
      :precondition (fn [{:keys [context observed]}] (and (readable? observed) (quiet? context observed)))
      :items (unless-unread
              (fn [{:keys [observed]}]
                (or (seq (map #(str "delete " %) (object-lines (:workloads observed))))
                    ["the release has no workloads to delete"])))
      :effect (fn [{:keys [context observed]}]
                (delete-effect context (:workloads observed) "the release has no workloads"))
      :verify (fn [{:keys [context observed]}]
                (gone-checks context (:workloads observed) (:deletion (waits observed))))}

     {:title "🗑  Delete the release's volumes"
      :prerequisites {:kubectl-can-delete ["persistentvolumeclaims"]}
      :confirm {:action "delete the release's volumes - their data is lost" :destructive? true :separately? true}
      :derive derive-step
      :precondition (fn [{:keys [context observed]}] (and (readable? observed) (quiet? context observed)))
      :items (unless-unread
              (fn [{:keys [observed]}]
                (or (seq (map #(str "delete " % " and the data on it") (object-lines (:volumes observed))))
                    ["the release has no volumes to delete"])))
      :effect (fn [{:keys [context observed]}]
                (delete-effect context (:volumes observed) "the release has no volumes"))
      :verify (fn [{:keys [context observed]}]
                (gone-checks context (:volumes observed) (:deletion (waits observed))))}

     {:title "🗑  Delete the Helm storage and confirm it is empty"
      :prerequisites {:kubectl-can-delete ["secrets"]}
      :confirm {:action "delete the release's Helm storage" :destructive? true}
      :derive derive-step
      :precondition (fn [{:keys [context observed]}] (and (readable? observed) (quiet? context observed)))
      :items (unless-unread
              (fn [{:keys [observed]}]
                [(format "delete every Helm revision Secret of %s in %s, so the next action is an install"
                         (:release-name observed) (:storage-namespace observed))]))
      :effect (fn [{:keys [context observed]}]
                (effect/plan
                 (effect/kubectl context "-n" (:storage-namespace observed) "delete" "secrets"
                                 (str "--selector=owner=helm,name=" (:release-name observed))
                                 "--ignore-not-found")))
      :verify (fn [{:keys [context observed]}]
                [{:description "the release's Helm storage is empty"
                  :timeout (:deletion (waits observed)) :interval 5
                  :check #(= [] (revisions context observed))}])}

     {:title "▶  Resume the HelmRelease, which installs"
      :confirm {:action "resume the HelmRelease"}
      :derive derive-step
      :precondition (fn [{:keys [observed]}] (readable? observed))
      :items (fn [{:keys [target]}] [(format "resume HelmRelease %s and request a reconcile, which installs it" target)])
      :effect (fn [{:keys [context observed timestamp]}]
                (let [release (:release observed)]
                  (flux/suspend-effect context {:type flux/helmrelease-type
                                                :namespace (kubectl/namespace-of release)
                                                :name (kubectl/name-of release)}
                                       false timestamp)))
      :verify (fn [{:keys [context observed]}]
                [{:description "the HelmRelease is Ready on a fresh install (revision 1, deployed)"
                  :timeout (:release-ready (waits observed)) :interval 15
                  :check #(and (ready? (fetch-release context observed))
                               (let [rows (revisions context observed)]
                                 (and (= 1 (count rows))
                                      (= 1 (:version (first rows)))
                                      (= "deployed" (:status (first rows))))))}])}

     {:title "▶  Resume the owner (Kustomization or ResourceSet)"
      :confirm {:action "resume the owner that renders the HelmRelease"}
      :derive derive-step
      :precondition (fn [{:keys [observed]}] (or (some? (:known-owner observed)) (readable? observed)))
      :items (fn [{:keys [observed]}]
               [(if-let [{:keys [kind namespace name]} (:known-owner observed)]
                  (format "resume %s %s/%s and request a reconcile" kind namespace name)
                  "no owner to resume")])
      :effect (fn [{:keys [context observed timestamp]}]
                (if-let [owner (:known-owner observed)]
                  (owner-suspend-effect context owner false timestamp)
                  (effect/plan (effect/note "no owner to resume"))))
      :verify (fn [{:keys [context observed]}]
                (when-let [owner (:known-owner observed)]
                  [{:description (format "%s %s/%s is resumed and Ready" (:kind owner) (:namespace owner) (:name owner))
                    :timeout (:owner-ready (waits (assoc observed :owner owner))) :interval 15
                    :check #(let [o (fetch-owner context owner)]
                              (and (not (owner-suspended? (:kind owner) o)) (ready? o)))}]))}]))

(defn helmrelease-picks
  [context]
  (mapv kubectl/namespace-name-of (kubectl/items-of (kubectl/get-all context flux/helmrelease-type))))

(def playbook
  {:description "Clears a HelmRelease's Helm storage so the next action is an install"
   :options {:namespace {:desc "The namespace of the HelmRelease - interactive selection when omitted"
                         :alias :n :coerce :string}
             :name {:desc "The name of the HelmRelease - interactive selection when omitted"
                    :alias :r :coerce :string}}
   :select {:mode :namespaced :label "HelmRelease" :namespace-flag :namespace :name-flag :name
            :candidates helmrelease-picks}
   :confirm {:action "suspend its owner, delete the release's workloads, volumes and Helm storage, then resume it to install afresh"
             :destructive? true}
   :steps (steps)})
