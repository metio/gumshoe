;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.action-books
  "The mutation-book specs of the CloudNativePG actions. Each book file merges
   one of these with its own :description, so the paired books (hibernate and
   rehydrate, suspend and resume) share a single spec that differs only in the
   wanted state. The decisions live in gumshoe.cnpg.actions."
  (:require [gumshoe.cnpg.actions :as actions]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]))

(defn- fetch
  [context type {:keys [namespace name]}]
  (kubectl/get-namespaced-resource context namespace type name))

(defn- cluster-picks
  [context refusal]
  (vec (for [cluster (sort-by kubectl/namespace-name-of (kubectl/items-of (kubectl/get-all context cnpg/cluster-type)))
             :when (nil? (refusal cluster))]
         (kubectl/namespace-name-of cluster))))

(defn- refuse
  [why]
  (when why (println (str "refusing: " why)))
  (nil? why))

(def ^:private cluster-option
  {:cluster {:desc "\"<namespace>/<cluster>\" - interactive selection when omitted" :alias :c :coerce :string}})

(defn- prerequisites
  [type & {:keys [list-pods?]}]
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get (cond-> [type] list-pods? (conj "pods"))
   :kubectl-can-patch [type]})

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
   :derive (fn [{:keys [context target]}]
             (let [pick (actions/parse-pick target)]
               {:pick pick
                :cluster (when pick (fetch context cnpg/cluster-type pick))
                :stamp (when stamp? (str (java.time.Instant/now)))}))
   :items (fn [{:keys [cluster target]}] (if cluster (items cluster) [(str verb " " target)]))
   :pin (fn [{:keys [pick]}]
          (when pick {:type cnpg/cluster-type :namespace (:namespace pick) :name (:name pick)
                      :facts #(select-keys (:metadata %) [:annotations])}))
   :precondition (fn [{:keys [context pick]}]
                   (refuse (if pick (refusal (fetch context cnpg/cluster-type pick)) "the pick is not \"<namespace>/<cluster>\"")))
   :announce (fn [{:keys [target]}] (format "%s %s" verb target))
   :effect (fn [{:keys [context pick stamp]}]
             (effect/plan (effect/kubectl context (str "--namespace=" (:namespace pick)) "annotate" cnpg/cluster-type
                                          (:name pick) "--overwrite" (arg stamp))))
   :verify (fn [{:keys [context pick target stamp]}]
             [{:description (format verify-description target)
               :timeout verify-timeout :interval 10
               :check (fn [] (applied? (fetch context cnpg/cluster-type pick)
                                       (when pods? (kubectl/get-namespaced context (:namespace pick) "pods"))
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
   :derive (fn [{:keys [context target]}]
             (let [pick (actions/parse-pick target)
                   cluster (when pick (fetch context cnpg/cluster-type pick))]
               {:pick pick :cluster cluster
                :backup-name (when cluster (actions/backup-name cluster (java.time.Instant/now)))}))
   :items (fn [{:keys [cluster backup-name opts target]}]
            (if cluster (actions/backup-items cluster (:method opts) backup-name) [(str "back up " target)]))
   :pin (fn [{:keys [pick]}] (when pick {:type cnpg/cluster-type :namespace (:namespace pick) :name (:name pick)}))
   :precondition (fn [{:keys [context pick opts]}]
                   (refuse (if pick (actions/backup-refusal (fetch context cnpg/cluster-type pick) (:method opts))
                               "the pick is not \"<namespace>/<cluster>\"")))
   :announce (fn [{:keys [target backup-name]}] (format "Back up %s as %s" target backup-name))
   :effect (fn [{:keys [context cluster backup-name opts]}]
             (if cluster
               (effect/plan (effect/kubectl-stdin context (actions/backup-json (actions/backup-manifest cluster (:method opts) backup-name))
                                                  "create" "-f" "-"))
               (effect/plan (effect/note "nothing to back up - the precondition says why"))))
   :verify (fn [{:keys [context pick backup-name]}]
             (let [current #(fetch context cnpg/backup-type {:namespace (:namespace pick) :name backup-name})]
               [{:description (format "Backup %s/%s completed" (:namespace pick) backup-name)
                 :timeout 3600 :interval 15
                 :abort #(first (actions/backup-signals (current)))
                 :check #(actions/backup-completed? (current))}]))})

(defn scheduled-backup-suspension
  [wanted]
  (let [type cnpg/scheduled-backup-type]
    {:options {:scheduled-backup {:desc "\"<namespace>/<name>\" - interactive selection when omitted" :alias :s :coerce :string}}
     :prerequisites (prerequisites type)
     :select {:mode :one :label "ScheduledBackup" :flag :scheduled-backup
              :candidates (fn [context]
                            (vec (for [sb (sort-by kubectl/namespace-name-of (kubectl/items-of (kubectl/get-all context type)))
                                       :when (nil? (actions/suspend-refusal sb wanted))]
                                   (kubectl/namespace-name-of sb))))}
     :empty-message (if wanted "every ScheduledBackup is suspended already" "no ScheduledBackup is suspended")
     :confirm {:action (if wanted "suspend scheduled backups" "resume scheduled backups")}
     :derive (fn [{:keys [context target]}]
               (let [pick (actions/parse-pick target)]
                 {:pick pick :scheduled-backup (when pick (fetch context type pick))}))
     :items (fn [{:keys [scheduled-backup target]}]
              (if scheduled-backup (actions/suspend-items scheduled-backup wanted) [(str (if wanted "suspend " "resume ") target)]))
     :pin (fn [{:keys [pick]}] (when pick {:type type :namespace (:namespace pick) :name (:name pick)}))
     :precondition (fn [{:keys [context pick]}]
                     (refuse (if pick (actions/suspend-refusal (fetch context type pick) wanted) "the pick is not \"<namespace>/<name>\"")))
     :announce (fn [{:keys [target]}] (format "%s ScheduledBackup %s" (if wanted "Suspend" "Resume") target))
     :effect (fn [{:keys [context pick]}]
               (effect/plan (effect/kubectl context (str "--namespace=" (:namespace pick)) "patch" type (:name pick)
                                            "--type=merge" (str "--patch=" (actions/suspend-patch wanted)))))
     :verify (fn [{:keys [context pick target]}]
               [{:description (format "ScheduledBackup %s is %s" target (if wanted "suspended" "resumed"))
                 :timeout 30 :interval 3
                 :check #(= wanted (actions/suspended? (fetch context type pick)))}])}))
