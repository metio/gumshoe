;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.actions
  "The decisions behind the CloudNativePG action books, pure over the objects:
   an on-demand Backup, a rolling restart, a reload, declarative hibernation,
   and suspending a ScheduledBackup. Every action but the ScheduledBackup one is
   an annotation or an object the operator acts on; these functions say when to
   refuse, what the confirmation promises, what to write, and when the operator
   has done it."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [gumshoe.cnpg.instances :as instances]
            [gumshoe.cnpg.recoverability :as recoverability]
            [gumshoe.conditions :as conditions]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.pick :as pick]))

(def healthy-phase "Cluster in healthy state")
(def restart-annotation "kubectl.kubernetes.io/restartedAt")
(def reload-annotation "cnpg.io/reloadedAt")
(def hibernation-annotation "cnpg.io/hibernation")
(def hibernation-condition "cnpg.io/hibernation")

(defn- annotation
  [object key]
  (get-in object [:metadata :annotations (keyword key)]))

(def parse-pick
  "\"<namespace>/<name>\" as {:namespace :name}, or nil."
  pick/namespaced)

(defn healthy?
  "The cluster reports a healthy phase with every instance ready."
  [cluster]
  (boolean (and cluster
                (= healthy-phase (-> cluster :status :phase))
                (= (-> cluster :spec :instances) (-> cluster :status :readyInstances)))))

;; --- hibernation -----------------------------------------------------------------

(defn hibernation-requested?
  [cluster]
  (= "on" (annotation cluster hibernation-annotation)))

(defn hibernated?
  "The operator reports every instance pod gone."
  [cluster]
  (= "True" (:status (conditions/condition cluster hibernation-condition))))

(defn hibernation-refusal
  [cluster wanted]
  (cond
    (nil? cluster) "the cluster no longer exists"
    (and wanted (hibernation-requested? cluster)) "hibernation is requested already"
    (and (not wanted) (not (hibernation-requested? cluster))) "the cluster is not hibernated"
    (and wanted (not= healthy-phase (-> cluster :status :phase)))
    (format "the cluster is not healthy (%s), and the operator defers hibernation until it is"
            (or (-> cluster :status :phase) "no phase"))
    :else nil))

(defn hibernation-items
  [cluster wanted]
  (let [target (kubectl/namespace-name-of cluster)
        count (or (-> cluster :spec :instances) 0)]
    (if wanted
      [(format "hibernate cluster %s - the operator deletes its %d instance pod(s), the primary first, and keeps their volumes" target count)
       "no instance runs until it is rehydrated, so nothing accepts connections, archives WAL or takes a backup"]
      [(format "rehydrate cluster %s - the operator recreates its instance pods on the kept volumes" target)])))

(defn hibernation-arg
  [wanted]
  (str hibernation-annotation "=" (if wanted "on" "off")))

(defn hibernation-applied?
  "Hibernated once the operator reports it; rehydrated once the condition is
   gone and the cluster is healthy again."
  [cluster wanted]
  (if wanted
    (hibernated? cluster)
    (boolean (and (nil? (conditions/condition cluster hibernation-condition)) (healthy? cluster)))))

;; --- restart and reload ----------------------------------------------------------

(defn- running-refusal
  [cluster]
  (cond
    (nil? cluster) "the cluster no longer exists"
    (hibernation-requested? cluster) "the cluster is hibernated, so no instance runs - rehydrate it first"
    (not= healthy-phase (-> cluster :status :phase))
    (format "the cluster is not healthy (%s) - wait until it is, or investigate first"
            (or (-> cluster :status :phase) "no phase"))
    :else nil))

(defn restart-refusal [cluster] (running-refusal cluster))
(defn reload-refusal [cluster] (running-refusal cluster))

(defn restart-items
  [cluster]
  (let [method (or (-> cluster :spec :primaryUpdateMethod) "restart")]
    [(format "restart every instance of cluster %s, one at a time, replicas first" (kubectl/namespace-name-of cluster))
     (if (= "switchover" method)
       "then the primary by a switchover (primaryUpdateMethod switchover) - clients of the -rw Service reconnect to the new primary"
       "then the primary in place (primaryUpdateMethod restart) - it accepts no writes while it restarts")]))

(defn reload-items
  [cluster]
  [(format "reload cluster %s - the operator reconciles every instance again and rolls out configuration that changed, such as a renewed Secret or ConfigMap"
           (kubectl/namespace-name-of cluster))])

(defn stamp-arg
  [annotation-key stamp]
  (str annotation-key "=" stamp))

(defn restarted?
  "Every instance pod carries the restart stamp and the cluster is healthy."
  [cluster pods stamp]
  (let [mine (instances/instance-pods (kubectl/items-of pods) (kubectl/namespace-of cluster) (kubectl/name-of cluster))]
    (boolean (and (healthy? cluster)
                  (seq mine)
                  (= (count mine) (-> cluster :spec :instances))
                  (every? #(= stamp (annotation % restart-annotation)) mine)))))

(defn reloaded?
  [cluster stamp]
  (boolean (and (= stamp (annotation cluster reload-annotation)) (healthy? cluster))))

;; --- on-demand backup ------------------------------------------------------------

(defn wal-archiver
  "The plugin that archives the cluster's WAL, or nil."
  [cluster]
  (:name (first (filter #(and (true? (:isWALArchiver %)) (not (false? (:enabled %)))) (-> cluster :spec :plugins)))))

(defn backup-methods
  "The backup methods the cluster's spec configures, the preferred one first."
  [cluster]
  (vec (remove nil? [(when (-> cluster :spec :backup :barmanObjectStore) "barmanObjectStore")
                     (when (wal-archiver cluster) "plugin")
                     (when (-> cluster :spec :backup :volumeSnapshot) "volumeSnapshot")])))

(defn backup-refusal
  [cluster method]
  (let [methods (backup-methods cluster)]
    (cond
      (nil? cluster) "the cluster no longer exists"
      (hibernation-requested? cluster) "the cluster is hibernated, so no instance can take a backup - rehydrate it first"
      (empty? methods) "the cluster configures no backup destination - neither a barmanObjectStore, a WAL-archiving plugin, nor volume snapshots"
      (and method (not (some #{method} methods)))
      (format "the cluster does not configure %s backups (it configures %s)" method (str/join ", " methods))
      (not= healthy-phase (-> cluster :status :phase))
      (format "the cluster is not healthy (%s) - a backup now may fail or wait" (or (-> cluster :status :phase) "no phase"))
      :else nil)))

(defn backup-name
  "The Backup's name: the cluster's plus the UTC time, as the cnpg plugin names it."
  [cluster ^java.time.Instant now]
  (str (kubectl/name-of cluster) "-"
       (.format (java.time.format.DateTimeFormatter/ofPattern "yyyyMMddHHmmss")
                (.atZone now java.time.ZoneOffset/UTC))))

(defn backup-manifest
  [cluster method name]
  (let [method (or method (first (backup-methods cluster)))]
    {:apiVersion "postgresql.cnpg.io/v1"
     :kind "Backup"
     :metadata {:namespace (kubectl/namespace-of cluster) :name name
                :labels {:cnpg.io/cluster (kubectl/name-of cluster)}}
     :spec (cond-> {:cluster {:name (kubectl/name-of cluster)} :method method}
             (= "plugin" method) (assoc :pluginConfiguration {:name (wal-archiver cluster)}))}))

(defn backup-items
  [cluster method name]
  (let [method (or method (first (backup-methods cluster)))]
    [(format "start Backup %s/%s of cluster %s with method %s" (kubectl/namespace-of cluster) name (kubectl/name-of cluster) method)
     (format "it runs where spec.backup.target says (%s)" (or (-> cluster :spec :backup :target) "prefer-standby, the default - a standby when one is ready"))]))

(defn backup-json
  [manifest]
  (json/generate-string manifest))

(defn backup-completed?
  [backup]
  (= "completed" (-> backup :status :phase)))

(defn backup-signals
  "Why the wait can end: a failed Backup's error."
  [backup]
  (when (= "failed" (-> backup :status :phase))
    [(str "the Backup failed: " (or (-> backup :status :error) "no error recorded"))]))

;; --- scheduled backups -----------------------------------------------------------

(defn suspended?
  [scheduled-backup]
  (true? (-> scheduled-backup :spec :suspend)))

(defn suspend-refusal
  [scheduled-backup wanted]
  (cond
    (nil? scheduled-backup) "the ScheduledBackup no longer exists"
    (= wanted (suspended? scheduled-backup)) (if wanted "it is suspended already" "it is not suspended")
    :else nil))

(defn- archiving-item
  "What suspending the schedule does to the cluster's recovery points, from
   the Cluster read live (nil when it does not exist). With WAL archived, the
   newest recovery point keeps advancing and only the base backup ages;
   without, the newest backup is the newest recovery point."
  [cluster-name cluster]
  (cond
    (nil? cluster)
    (format (str "cluster %s was not found, so whether it archives WAL is unknown - "
                 "without archiving, its newest backup is its newest recovery point and ages from now on")
            cluster-name)

    (recoverability/archives-wal? cluster)
    (format (str "cluster %s archives WAL, so its newest recovery point keeps advancing; its newest "
                 "base backup ages from now on, and a restore replays more WAL the older it gets")
            cluster-name)

    :else
    (format (str "cluster %s archives no WAL, so its newest recovery point is its newest backup, "
                 "which ages from now on")
            cluster-name)))

(defn suspend-items
  "The confirmation items of suspending (wanted true) or resuming a
   ScheduledBackup. cluster is the Cluster it backs up, read live, or nil when
   that Cluster does not exist."
  [scheduled-backup wanted cluster]
  (let [target (kubectl/namespace-name-of scheduled-backup)
        cluster-name (-> scheduled-backup :spec :cluster :name)
        schedule (-> scheduled-backup :spec :schedule)]
    (if wanted
      [(format "suspend ScheduledBackup %s - no backup of cluster %s starts on its schedule %s until it is resumed" target cluster-name schedule)
       (archiving-item cluster-name cluster)]
      [(format "resume ScheduledBackup %s - backups of cluster %s start again on its schedule %s" target cluster-name schedule)])))

(defn suspend-patch
  [wanted]
  (json/generate-string {:spec {:suspend wanted}}))
