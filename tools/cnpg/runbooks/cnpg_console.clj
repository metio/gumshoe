;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.cnpg-console
  "Reads every instance of a CloudNativePG cluster - WAL positions, recovery
   state, replication slots - and proposes which instance a promotion would
   discard least from. Read-only unless --promote names an instance, and that
   path asks for approval before it writes anything."
  (:require [clojure.string :as str]
            [gumshoe.cnpg-console :as console]
            [gumshoe.cnpg.instances :as inst]
            [gumshoe.command :as command]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(def options
  {:namespace {:desc "Namespace of the cluster - interactive selection when omitted"
               :alias :n
               :coerce :string}
   :cluster {:desc "Name of the cluster - interactive selection when omitted"
             :alias :c
             :coerce :string}
   :promote {:desc "Promote this instance to primary - asks for approval first"
             :coerce :string}})

(def prerequisites
  {:installed-tools ["kubectl" "fzf"]
   :cluster-capabilities []
   :kubectl-can-get [cnpg/cluster-type "pods"]})

(def ^:private instance-sql
  ;; a primary is not in recovery and so has no receive LSN; its position is
  ;; pg_current_wal_lsn, without which it ranks below every standby
  (str "select pg_is_in_recovery(), "
       "coalesce((case when pg_is_in_recovery() "
       "then pg_last_wal_receive_lsn() else pg_current_wal_lsn() end)::text, ''), "
       "coalesce(pg_last_wal_replay_lsn()::text, ''), "
       "coalesce((now() - pg_last_xact_replay_timestamp())::text, '')"))

(def ^:private slot-sql
  (str "select slot_name, active, wal_status, "
       "coalesce(pg_size_pretty(safe_wal_size), '') from pg_replication_slots"))

(defn- psql
  [context namespace pod sql]
  (shell/stdout-of "kubectl" (str "--context=" context) (str "--namespace=" namespace)
                   "exec" pod "-c" "postgres" "--"
                   "psql" "-U" "postgres" "-tAF|" "-c" sql))

(defn- read-instance
  "One instance's position. An exec that fails - the container of a crashlooping
   pod does not exist - leaves the instance unreadable rather than silently
   absent, because an unread instance may be the furthest ahead of all."
  [context namespace pod]
  (let [out (str/trim (str (psql context namespace pod instance-sql)))]
    (if (str/blank? out)
      {:instance pod :readable? false}
      (let [[recovery receive replay lag] (str/split out #"\|" -1)]
        {:instance pod
         :readable? true
         :in-recovery? (= "t" recovery)
         :receive-lsn receive
         :replay-lsn replay
         :replay-lag lag}))))

(defn- read-slots
  [context namespace pod]
  (for [line (str/split-lines (str/trim (str (psql context namespace pod slot-sql))))
        :when (not (str/blank? line))
        :let [[slot-name active wal-status safe] (str/split line #"\|" -1)]]
    {:instance pod :slot-name slot-name :active (= "t" active)
     :wal-status wal-status :safe-wal-size safe}))

(defn- print-readings!
  [readings]
  (stdout/print-section "Instances")
  (doseq [{:keys [instance readable? in-recovery? receive-lsn replay-lsn replay-lag]} readings]
    (if readable?
      (stdout/print-data-table {:instance instance
                                :in-recovery (str in-recovery?)
                                :received receive-lsn
                                :replayed replay-lsn
                                :last-replay-age (if (str/blank? replay-lag) "never" replay-lag)})
      (stdout/print-data-table {:instance instance
                                :in-recovery "unreadable"
                                :received "unreadable"
                                :replayed "unreadable"
                                :last-replay-age "unreadable"}))))

(defn- promote-effect
  [context namespace cluster instance]
  (effect/plan
   (effect/note (format "promoting %s discards any WAL the other instances received beyond it"
                        instance))
   (effect/kubectl context "cnpg" "promote" (str "--namespace=" namespace) cluster instance)))

(defn- promote!
  [context namespace cluster instance proposal]
  (if-not (command/installed? "kubectl-cnpg")
    (do (stdout/error "the cnpg kubectl plugin is not installed, so this book will not promote")
        (stdout/warn (format "install it, or promote by hand: kubectl cnpg promote --namespace=%s %s %s"
                             namespace cluster instance))
        false)
    (flow/change!
     {:confirmation {:action "promote a Postgres instance to primary"
                     :target (format "%s/%s" namespace cluster)
                     :items (into [(format "promote %s" instance)]
                                  (console/proposal-lines proposal))
                     :destructive? true}
      :effect (promote-effect context namespace cluster instance)
      :post-checks [{:description (format "%s is the cluster's currentPrimary" instance)
                     :timeout 120
                     :check (fn []
                              (= instance
                                 (console/current-primary
                                  (kubectl/items-of (kubectl/get-all context cnpg/cluster-type))
                                  (format "%s/%s" namespace cluster))))}]})))

(defn- console
  [opts _ctx]
  (let [context (kubectl/current-context)
        clusters (kubectl/items-of (kubectl/get-all context cnpg/cluster-type))
        chosen (interact/choose-namespaced "Cluster" (mapv kubectl/namespace-name-of clusters)
                                           (:namespace opts) (:cluster opts))]
    (if (nil? chosen)
      (do (stdout/error "no cluster selected") false)
      (let [{:keys [namespace name]} (kubectl/split-namespace-name chosen)
            cluster (kubectl/find-item clusters chosen)
            pods (kubectl/items-of (kubectl/get-all context "pods"))
            instances (mapv kubectl/name-of (inst/instance-pods pods namespace name))]
        (if (empty? instances)
          (do (stdout/error (format "no instance pods found for cluster %s" chosen)) false)
          (let [readings (mapv #(read-instance context namespace %) instances)
                slots (mapcat #(read-slots context namespace %)
                              (map :instance (filter :readable? readings)))
                current (-> cluster :status :currentPrimary)
                proposal (console/proposal
                          readings
                          {:name current
                           :healthy? (boolean (some #{current}
                                                    (-> cluster :status :instancesStatus :healthy)))})]
            (stdout/print-data-table {:cluster chosen
                                      :phase (-> cluster :status :phase)
                                      :current-primary (-> cluster :status :currentPrimary)
                                      :target-primary (-> cluster :status :targetPrimary)
                                      :instances (str/join ", " instances)})
            (print-readings! readings)
            (when (seq slots)
              (stdout/print-section "Replication slots")
              (doseq [{:keys [instance slot-name active wal-status safe-wal-size]} slots]
                (stdout/print-data-table {:on instance :slot slot-name
                                          :active (str active) :wal-status wal-status
                                          :safe-wal-size (if (str/blank? safe-wal-size)
                                                           "unbounded" safe-wal-size)}))
              (doseq [line (console/slot-lines slots)] (stdout/warn line)))
            (stdout/print-section "Promotion proposal")
            (doseq [line (console/proposal-lines proposal)] (stdout/warn line))
            (if-let [instance (:promote opts)]
              (if-not (some #{instance} instances)
                (do (stdout/error (format "%s is not an instance of %s" instance chosen)) false)
                (promote! context namespace name instance proposal))
              (do (stdout/ok "read-only run - nothing was changed")
                  true))))))))

(runbook/execute!
 {:description "Reads a CloudNativePG cluster's instances and proposes a promotion target"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action console})
