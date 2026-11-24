;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.action-books
  "The mutation-book specs of the CloudNativePG actions. Each book file merges
   one of these with its own :description, so the paired books share a single
   spec that differs only in the wanted state: hibernate and rehydrate through
   cluster-annotation-book, suspend and resume as the two sides of one
   gumshoe.toggle declaration. The decisions live in gumshoe.cnpg.actions."
  (:require [gumshoe.cnpg.actions :as actions]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.toggle :as toggle]))

(defn- cluster-picks
  [context refusal]
  (vec (for [cluster (sort-by kubectl/namespace-name-of (kubectl/items-of (kubectl/get-all context cnpg/cluster-type)))
             :when (nil? (refusal cluster))]
         (kubectl/namespace-name-of cluster))))

(def ^:private cluster-option
  {:cluster {:desc "\"<namespace>/<cluster>\" - interactive selection when omitted" :alias :c :coerce :string}})

(defn- prerequisites
  [type & {:keys [list-pods?]}]
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get (cond-> [type] list-pods? (conj "pods"))
   :kubectl-can-patch [type]})

(def ^:private cluster-subject
  {:type cnpg/cluster-type :parse actions/parse-pick :invalid "the pick is not \"<namespace>/<cluster>\""})

(defn- cluster-annotation-book
  "A book that writes one annotation on a Cluster: the shape restart, reload
   and hibernation share."
  [{:keys [verb empty-message confirm refusal items arg verify-description verify-timeout applied? pods? stamp?]}]
  {:options cluster-option
   :prerequisites (prerequisites cnpg/cluster-type :list-pods? pods?)
   :select {:mode :one :label "Cluster" :flag :cluster
            :candidates (fn [context] (cluster-picks context refusal))}
   :empty-message empty-message
   :confirm confirm
   :subject (assoc cluster-subject :facts #(select-keys (:metadata %) [:annotations]))
   :derive (fn [_] {:stamp (when stamp? (str (java.time.Instant/now)))})
   :items (fn [{:keys [object target]}] (if object (items object) [(str verb " " target)]))
   :refusal (fn [{:keys [object]}] (refusal object))
   :announce (fn [{:keys [target]}] (format "%s %s" verb target))
   :effect (fn [{:keys [context ref stamp]}]
             (effect/plan (effect/kubectl context (str "--namespace=" (:namespace ref)) "annotate" cnpg/cluster-type
                                          (:name ref) "--overwrite" (arg stamp))))
   :verify (fn [{:keys [context ref target stamp] :as ctx}]
             [{:description (format verify-description target)
               :timeout verify-timeout :interval 10
               :check (fn [] (applied? (mutation/read-subject ctx)
                                       (when pods? (kubectl/get-namespaced context (:namespace ref) "pods"))
                                       stamp))}])})

(def restart
  (cluster-annotation-book
   {:verb "Restart"
    :empty-message "no healthy cluster to restart"
    :confirm {:action "restart every instance of a cluster - each is briefly unavailable, the primary included" :destructive? true}
    :refusal actions/restart-refusal
    :items actions/restart-items
    :arg #(actions/stamp-arg actions/restart-annotation %)
    :stamp? true :pods? true
    :verify-description "every instance of %s restarted and the cluster is healthy"
    :verify-timeout 1200
    :applied? (fn [cluster pods stamp] (actions/restarted? cluster pods stamp))}))

(def reload
  (cluster-annotation-book
   {:verb "Reload"
    :empty-message "no healthy cluster to reload"
    :confirm {:action "reload a cluster's configuration"}
    :refusal actions/reload-refusal
    :items actions/reload-items
    :arg #(actions/stamp-arg actions/reload-annotation %)
    :stamp? true
    :verify-description "%s took the reload and is healthy"
    :verify-timeout 600
    :applied? (fn [cluster _ stamp] (actions/reloaded? cluster stamp))}))

(defn hibernation
  [wanted]
  (cluster-annotation-book
   {:verb (if wanted "Hibernate" "Rehydrate")
    :empty-message (if wanted "no healthy cluster to hibernate" "no cluster is hibernated")
    :confirm (if wanted
               {:action "hibernate a cluster - every instance pod is deleted and nothing accepts connections" :destructive? true}
               {:action "rehydrate a cluster"})
    :refusal #(actions/hibernation-refusal % wanted)
    :items #(actions/hibernation-items % wanted)
    :arg (fn [_] (actions/hibernation-arg wanted))
    :verify-description (if wanted "%s is hibernated" "%s is rehydrated and healthy")
    :verify-timeout (if wanted 300 900)
    :applied? (fn [cluster _ _] (actions/hibernation-applied? cluster wanted))}))

(def backup
  {:options (merge cluster-option
                   {:method {:desc "barmanObjectStore, plugin or volumeSnapshot - the first the cluster configures when omitted"
                             :coerce :string}})
   :prerequisites {:installed-tools ["kubectl" "fzf"]
                   :cluster-capabilities []
                   :kubectl-can-get [cnpg/cluster-type cnpg/backup-type]
                   :kubectl-can-create [cnpg/backup-type]}
   :select {:mode :one :label "Cluster" :flag :cluster
            :candidates (fn [context] (cluster-picks context #(actions/backup-refusal % nil)))}
   :empty-message "no healthy cluster configures a backup destination"
   :confirm {:action "start an on-demand backup"}
   :subject cluster-subject
   :derive (fn [{:keys [object]}]
             {:backup-name (when object (actions/backup-name object (java.time.Instant/now)))})
   :items (fn [{:keys [object backup-name opts target]}]
            (if object (actions/backup-items object (:method opts) backup-name) [(str "back up " target)]))
   :refusal (fn [{:keys [object opts]}] (actions/backup-refusal object (:method opts)))
   :announce (fn [{:keys [target backup-name]}] (format "Back up %s as %s" target backup-name))
   :effect (fn [{:keys [context object backup-name opts]}]
             (if object
               (effect/plan (effect/kubectl-stdin context (actions/backup-json (actions/backup-manifest object (:method opts) backup-name))
                                                  "create" "-f" "-"))
               (effect/plan (effect/note "nothing to back up - the precondition says why"))))
   :verify (fn [{:keys [context ref backup-name]}]
             (let [current #(kubectl/get-namespaced-resource context (:namespace ref) cnpg/backup-type backup-name)]
               [{:description (format "Backup %s/%s completed" (:namespace ref) backup-name)
                 :timeout 3600 :interval 15
                 :abort #(first (actions/backup-signals (current)))
                 :check #(actions/backup-completed? (current))}]))})

(def scheduled-backup-suspending
  "The suspend and resume books of a ScheduledBackup, as {:on :off} (see
   gumshoe.toggle)."
  (toggle/pair
   {:spec {:options {:scheduled-backup {:desc "\"<namespace>/<name>\" - interactive selection when omitted" :alias :s :coerce :string}}
           :prerequisites (prerequisites cnpg/scheduled-backup-type)
           :select {:mode :one :label "ScheduledBackup" :flag :scheduled-backup}
           :subject {:type cnpg/scheduled-backup-type
                     :parse actions/parse-pick
                     :invalid "the pick is not \"<namespace>/<name>\""}}
    :refusal actions/suspend-refusal
    :items (fn [{:keys [object]} wanted] (actions/suspend-items object wanted))
    :patch actions/suspend-patch
    :applied? (fn [scheduled-backup wanted] (= wanted (actions/suspended? scheduled-backup)))
    :verify {:timeout 30 :interval 3}
    :on {:verb "suspend" :empty-message "every ScheduledBackup is suspended already"
         :confirm {:action "suspend scheduled backups"}
         :announce "Suspend ScheduledBackup %s" :verified "ScheduledBackup %s is suspended"}
    :off {:verb "resume" :empty-message "no ScheduledBackup is suspended"
          :confirm {:action "resume scheduled backups"}
          :announce "Resume ScheduledBackup %s" :verified "ScheduledBackup %s is resumed"}}))

(defn scheduled-backup-suspension
  "The book that suspends a ScheduledBackup when wanted, else the one that
   resumes it."
  [wanted]
  ((if wanted :on :off) scheduled-backup-suspending))
