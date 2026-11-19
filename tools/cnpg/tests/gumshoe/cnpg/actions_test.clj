;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.actions-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.cnpg.actions :as actions]))

(defn- cluster [& {:keys [phase instances ready annotations conditions backup plugins primary-update]
                   :or {phase "Cluster in healthy state" instances 3 ready 3}}]
  {:metadata (cond-> {:namespace "db" :name "pg"} annotations (assoc :annotations annotations))
   :spec (cond-> {:instances instances}
           backup (assoc :backup backup)
           plugins (assoc :plugins plugins)
           primary-update (assoc :primaryUpdateMethod primary-update))
   :status {:phase phase :readyInstances ready :conditions (vec conditions)}})

(def ^:private hibernated-condition {:type "cnpg.io/hibernation" :status "True" :reason "Hibernated"})

(deftest picks
  (is (= {:namespace "db" :name "pg"} (actions/parse-pick " db/pg ")))
  (doseq [bad [nil "" "pg" "db/pg extra" "a/b/c"]]
    (is (nil? (actions/parse-pick bad)) (pr-str bad))))

(deftest healthy-needs-the-phase-and-every-instance
  (is (actions/healthy? (cluster)))
  (is (not (actions/healthy? (cluster :ready 2))))
  (is (not (actions/healthy? (cluster :phase "Upgrading cluster"))))
  (is (not (actions/healthy? nil))))

;; --- hibernation -----------------------------------------------------------------

(deftest hibernation-refusals
  (is (nil? (actions/hibernation-refusal (cluster) true)))
  (is (= "hibernation is requested already" (actions/hibernation-refusal (cluster :annotations {:cnpg.io/hibernation "on"}) true)))
  (is (= "the cluster is not hibernated" (actions/hibernation-refusal (cluster) false)))
  (is (= "the cluster is not hibernated" (actions/hibernation-refusal (cluster :annotations {:cnpg.io/hibernation "off"}) false)))
  (is (nil? (actions/hibernation-refusal (cluster :phase "Cluster in healthy state" :annotations {:cnpg.io/hibernation "on"}) false)))
  (is (= "the cluster is not healthy (Failing over), and the operator defers hibernation until it is"
         (actions/hibernation-refusal (cluster :phase "Failing over") true)))
  (is (= "the cluster no longer exists" (actions/hibernation-refusal nil true))))

(deftest hibernation-confirmation-and-annotation
  (is (= ["hibernate cluster db/pg - the operator deletes its 3 instance pod(s), the primary first, and keeps their volumes"
          "no instance runs until it is rehydrated, so nothing accepts connections, archives WAL or takes a backup"]
         (actions/hibernation-items (cluster) true)))
  (is (= ["rehydrate cluster db/pg - the operator recreates its instance pods on the kept volumes"]
         (actions/hibernation-items (cluster) false)))
  (is (= "cnpg.io/hibernation=on" (actions/hibernation-arg true)))
  (is (= "cnpg.io/hibernation=off" (actions/hibernation-arg false))))

(deftest hibernation-completes-on-the-operator-condition
  (is (actions/hibernation-applied? (cluster :conditions [hibernated-condition]) true))
  (is (not (actions/hibernation-applied? (cluster :conditions [{:type "cnpg.io/hibernation" :status "False" :reason "DeletingPods"}]) true)))
  (is (not (actions/hibernation-applied? (cluster) true)))
  (testing "rehydrated once the condition is gone and every instance is ready"
    (is (actions/hibernation-applied? (cluster) false))
    (is (not (actions/hibernation-applied? (cluster :conditions [hibernated-condition]) false)))
    (is (not (actions/hibernation-applied? (cluster :ready 1) false)))))

;; --- restart and reload ----------------------------------------------------------

(deftest restart-and-reload-refusals
  (doseq [refusal [actions/restart-refusal actions/reload-refusal]]
    (is (nil? (refusal (cluster))))
    (is (= "the cluster no longer exists" (refusal nil)))
    (is (str/includes? (refusal (cluster :annotations {:cnpg.io/hibernation "on"})) "rehydrate it first"))
    (is (= "the cluster is not healthy (Upgrading cluster) - wait until it is, or investigate first"
           (refusal (cluster :phase "Upgrading cluster"))))))

(deftest the-restart-confirmation-names-the-primary-update-method
  (is (= ["restart every instance of cluster db/pg, one at a time, replicas first"
          "then the primary in place (primaryUpdateMethod restart) - it accepts no writes while it restarts"]
         (actions/restart-items (cluster))))
  (is (str/includes? (second (actions/restart-items (cluster :primary-update "switchover"))) "by a switchover")))

(defn- pod [name & {:keys [restarted cluster-name pooler] :or {cluster-name "pg"}}]
  {:metadata (cond-> {:namespace "db" :name name
                      :labels (cond-> {:cnpg.io/cluster cluster-name :cnpg.io/podRole "instance"}
                                pooler (assoc :cnpg.io/poolerName pooler))
                      :ownerReferences [{:kind "Cluster" :name cluster-name}]}
               restarted (assoc :annotations {:kubectl.kubernetes.io/restartedAt restarted}))})

(deftest restarted-reads-every-instance-pod
  (let [stamp "2026-11-19T12:00:00Z"
        done {:items [(pod "pg-1" :restarted stamp) (pod "pg-2" :restarted stamp) (pod "pg-3" :restarted stamp)
                      (pod "pg-pooler-x" :pooler "pg-pooler") (pod "other-1" :cluster-name "other")]}]
    (is (actions/restarted? (cluster) done stamp) "poolers and other clusters do not count")
    (is (not (actions/restarted? (cluster) {:items [(pod "pg-1" :restarted stamp) (pod "pg-2" :restarted stamp) (pod "pg-3" :restarted "earlier")]} stamp)))
    (is (not (actions/restarted? (cluster) {:items [(pod "pg-1" :restarted stamp) (pod "pg-2" :restarted stamp)]} stamp)) "one instance missing")
    (is (not (actions/restarted? (cluster :ready 2) done stamp)) "not healthy yet")
    (is (not (actions/restarted? (cluster) {:items []} stamp)))))

(deftest reloads
  (is (= ["reload cluster db/pg - the operator reconciles every instance again and rolls out configuration that changed, such as a renewed Secret or ConfigMap"]
         (actions/reload-items (cluster))))
  (is (= "cnpg.io/reloadedAt=s1" (actions/stamp-arg actions/reload-annotation "s1")))
  (is (= "kubectl.kubernetes.io/restartedAt=s1" (actions/stamp-arg actions/restart-annotation "s1")))
  (is (actions/reloaded? (cluster :annotations {:cnpg.io/reloadedAt "s1"}) "s1"))
  (is (not (actions/reloaded? (cluster :annotations {:cnpg.io/reloadedAt "s0"}) "s1")))
  (is (not (actions/reloaded? (cluster :annotations {:cnpg.io/reloadedAt "s1"} :ready 1) "s1"))))

;; --- backups ---------------------------------------------------------------------

(def ^:private object-store {:barmanObjectStore {:destinationPath "s3://backups/pg"}})

(deftest backup-methods-follow-the-spec
  (is (= ["barmanObjectStore"] (actions/backup-methods (cluster :backup object-store))))
  (is (= ["volumeSnapshot"] (actions/backup-methods (cluster :backup {:volumeSnapshot {:className "csi"}}))))
  (is (= ["plugin"] (actions/backup-methods (cluster :plugins [{:name "barman-cloud.cloudnative-pg.io" :isWALArchiver true}]))))
  (is (= [] (actions/backup-methods (cluster :plugins [{:name "x" :isWALArchiver true :enabled false}]))) "a disabled plugin")
  (is (= [] (actions/backup-methods (cluster :plugins [{:name "x"}]))) "a plugin that archives no WAL")
  (is (= ["barmanObjectStore" "volumeSnapshot"]
         (actions/backup-methods (cluster :backup (assoc object-store :volumeSnapshot {}))))))

(deftest backup-refusals
  (is (nil? (actions/backup-refusal (cluster :backup object-store) nil)))
  (is (nil? (actions/backup-refusal (cluster :backup object-store) "barmanObjectStore")))
  (is (= "the cluster does not configure volumeSnapshot backups (it configures barmanObjectStore)"
         (actions/backup-refusal (cluster :backup object-store) "volumeSnapshot")))
  (is (str/starts-with? (actions/backup-refusal (cluster) nil) "the cluster configures no backup destination"))
  (is (str/includes? (actions/backup-refusal (cluster :backup object-store :annotations {:cnpg.io/hibernation "on"}) nil) "hibernated"))
  (is (str/starts-with? (actions/backup-refusal (cluster :backup object-store :phase "Failing over") nil) "the cluster is not healthy (Failing over)"))
  (is (= "the cluster no longer exists" (actions/backup-refusal nil nil))))

(deftest the-backup-object
  (let [now (java.time.Instant/parse "2026-11-19T12:34:56Z")
        name (actions/backup-name (cluster) now)]
    (is (= "pg-20261119123456" name))
    (is (= {:apiVersion "postgresql.cnpg.io/v1" :kind "Backup"
            :metadata {:namespace "db" :name "pg-20261119123456" :labels {:cnpg.io/cluster "pg"}}
            :spec {:cluster {:name "pg"} :method "barmanObjectStore"}}
           (actions/backup-manifest (cluster :backup object-store) nil name)))
    (is (= {:cluster {:name "pg"} :method "plugin" :pluginConfiguration {:name "barman-cloud.cloudnative-pg.io"}}
           (:spec (actions/backup-manifest (cluster :plugins [{:name "barman-cloud.cloudnative-pg.io" :isWALArchiver true}]) nil name))))
    (is (= "volumeSnapshot" (-> (actions/backup-manifest (cluster :backup (assoc object-store :volumeSnapshot {})) "volumeSnapshot" name) :spec :method)))
    (is (= "Backup" (get (json/parse-string (actions/backup-json (actions/backup-manifest (cluster :backup object-store) nil name))) "kind")))))

(deftest the-backup-confirmation-names-method-and-target
  (is (= ["start Backup db/pg-1 of cluster pg with method barmanObjectStore"
          "it runs where spec.backup.target says (prefer-standby, the default - a standby when one is ready)"]
         (actions/backup-items (cluster :backup object-store) nil "pg-1")))
  (is (str/ends-with? (second (actions/backup-items (cluster :backup (assoc object-store :target "primary")) nil "pg-1")) "(primary)")))

(deftest backup-completion-and-failure
  (is (actions/backup-completed? {:status {:phase "completed"}}))
  (is (not (actions/backup-completed? {:status {:phase "running"}})))
  (is (not (actions/backup-completed? nil)))
  (is (= ["the Backup failed: can't upload"] (actions/backup-signals {:status {:phase "failed" :error "can't upload"}})))
  (is (= ["the Backup failed: no error recorded"] (actions/backup-signals {:status {:phase "failed"}})))
  (is (nil? (actions/backup-signals {:status {:phase "running"}}))))

;; --- scheduled backups -------------------------------------------------------------

(defn- scheduled [& {:keys [suspend]}]
  {:metadata {:namespace "db" :name "nightly"}
   :spec (cond-> {:schedule "0 0 3 * * *" :cluster {:name "pg"}} (some? suspend) (assoc :suspend suspend))})

(deftest scheduled-backup-suspension
  (is (actions/suspended? (scheduled :suspend true)))
  (is (not (actions/suspended? (scheduled))))
  (is (nil? (actions/suspend-refusal (scheduled) true)))
  (is (nil? (actions/suspend-refusal (scheduled :suspend true) false)))
  (is (= "it is suspended already" (actions/suspend-refusal (scheduled :suspend true) true)))
  (is (= "it is not suspended" (actions/suspend-refusal (scheduled :suspend false) false)))
  (is (= "the ScheduledBackup no longer exists" (actions/suspend-refusal nil true)))
  (is (= ["suspend ScheduledBackup db/nightly - no backup of cluster pg starts on its schedule 0 0 3 * * * until it is resumed"
          "the cluster's newest recovery point ages from now on; WAL archiving continues"]
         (actions/suspend-items (scheduled) true)))
  (is (= ["resume ScheduledBackup db/nightly - backups of cluster pg start again on its schedule 0 0 3 * * *"]
         (actions/suspend-items (scheduled :suspend true) false)))
  (is (= "{\"spec\":{\"suspend\":true}}" (actions/suspend-patch true)))
  (is (= "{\"spec\":{\"suspend\":false}}" (actions/suspend-patch false))))
