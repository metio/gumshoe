;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation-books-test
  "Characterises the mutation books end to end against a recorded cluster:
   for each book and scenario, what the confirmation shows, the plan, the
   announcement, what the precondition decides and prints, and the
   post-checks. The transcripts are kept in tests/golden/mutation_books.edn,
   so a refactoring of a book or of the engine keeps every one of them.

   A scenario names the objects the cluster holds, the pick and flags, and
   what happens between the confirmation and the precondition: nothing, a
   spec change (the generation moves), or a kubectl that stops answering.
   Each book also keeps its surface: the options, prerequisites, selection,
   messages and --help text its spec declares, and the candidates its select
   offers over the permitted and the refused cluster.

   Regenerate the file after an intended change with
   (gumshoe.mutation-books-test/write-golden!) and review its diff."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.flow :as flow]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.shell :as shell]))

(def golden-path "tests/golden/mutation_books.edn")

;; --- the recorded cluster ----------------------------------------------------------

(defn- meta-of
  [namespace name & {:keys [generation uid] :or {generation 1 uid (str "uid-" name)}}]
  {:namespace namespace :name name :uid uid :generation generation :resourceVersion "100"})

(def ^:private unanswered
  {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 10.0.0.1:6443: i/o timeout"})

(defn- not-found
  [type name]
  {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" type name)})

(defn- arg-value
  [argv prefix]
  (some #(when (str/starts-with? % prefix) (subs % (count prefix))) argv))

(defn- helm-listing
  "The lines kubectl prints for helm-releases' jsonpath over the Secrets of
   objects that match and, when the selector names one, belong to that
   release."
  [objects matches argv]
  (let [release (some->> (arg-value argv "--selector=") (re-find #"name=([^,]+)") second)]
    (str/join "\n" (for [[t o] objects
                         :when (matches [t o])
                         :let [{:keys [namespace labels creationTimestamp]} (:metadata o)]
                         :when (or (nil? release) (= release (:name labels)))]
                     (str/join "\t" [namespace (:name labels) (:status labels) (:version labels)
                                     (:modifiedAt labels) (:createdAt labels) creationTimestamp])))))

(defn- answer
  "What kubectl answers for argv against state {:objects [[type object]..]
   :answering? :dry-run}."
  [{:keys [objects answering? dry-run]} argv]
  (let [namespace (arg-value argv "--namespace=")
        [_ type name] (drop-while #(not= "get" %) argv)
        name (when (and name (not (str/starts-with? name "-"))) name)
        matches (fn [[t o]] (and (= t type)
                                 (or (nil? namespace) (= namespace (-> o :metadata :namespace)))
                                 (or (nil? name) (= name (-> o :metadata :name)))))]
    (cond
      (not answering?) unanswered
      (some #{"--dry-run=server"} argv) (or dry-run {:exit 0 :out "deleted (server dry run)" :err ""})
      (not (some #{"get"} argv)) {:exit 0 :out "" :err ""}
      (and (= "secrets" type) (arg-value argv "--output=jsonpath="))
      {:exit 0 :out (helm-listing objects matches argv) :err ""}
      (some #{"--output=name"} argv)
      {:exit 0 :out (str/join "\n" (for [[t o] objects :when (matches [t o])] (str t "/" (-> o :metadata :name)))) :err ""}
      name (if-let [[_ o] (first (filter matches objects))]
             {:exit 0 :out (json/generate-string o) :err ""}
             (not-found type name))
      :else {:exit 0 :out (json/generate-string {:items (map second (filter matches objects))}) :err ""})))

(defn- bump-generation
  [objects]
  (mapv (fn [[t o]] [t (update-in o [:metadata :generation] (fnil inc 0))]) objects))

;; --- running a book ------------------------------------------------------------------

(defn- load-spec
  "The spec a book file hands to mutation/book."
  [path]
  (with-redefs [mutation/book identity]
    (load-file path)))

(defn- normalise
  "x with every instant and every yyyyMMddHHmmss stamp replaced, so a
   transcript does not depend on the clock."
  [x]
  (edn/read-string (-> (pr-str x)
                       (str/replace #"\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d+)?Z" "<instant>")
                       (str/replace #"\d{14}" "<stamp>"))))

(defn- capture
  [thunk]
  (let [out (java.io.StringWriter.)
        result (binding [*out* out *err* out] (thunk))]
    {:result result :out (str/trim (str out))}))

(defn- attempt
  [f]
  (try (f) (catch Exception e (str "threw: " (ex-message e)))))

(defn transcript
  "What the book does in one scenario."
  [spec {:keys [objects target opts dry-run between]}]
  (let [state (atom {:objects objects :answering? true :dry-run dry-run})
        seen (atom nil)]
    (with-redefs [shell/execute (fn [& argv] (answer @state (vec argv)))
                  kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  announce/announce! (fn [_ _ message] message)
                  flow/change! (fn [config] (reset! seen config) true)]
      (let [{:keys [out]} (capture #(try (mutation/run-on! spec target (or opts {}) nil)
                                         (catch Exception e (println "threw:" (type e) (ex-message e)))))
            {:keys [confirmation precondition announce! effect step post-checks]} @seen]
        (case between
          :spec-changed (swap! state update :objects bump-generation)
          :unanswered (swap! state assoc :answering? false)
          nil)
        (normalise
         (if-not @seen
           {:stopped out}
           (cond-> {:action (:action confirmation)
                    :destructive? (boolean (:destructive? confirmation))
                    :items (vec (:items confirmation))
                    :step step
                    :plan effect
                    :precondition (when precondition (capture precondition))}
             (seq out) (assoc :out out)
             announce! (assoc :announce (announce!))
             post-checks (assoc :checks (vec (for [{:keys [description timeout interval check abort]} post-checks]
                                               (cond-> {:description description :timeout timeout :interval interval
                                                        :holds (attempt check)}
                                                 abort (assoc :abort (attempt abort)))))))))))))

(defn surface
  "What the book declares outside a run: its flags, prerequisites, selection,
   messages and --help text, and the candidates its select offers over each
   of clusters, a map of a name to the objects that cluster holds."
  [spec clusters]
  (normalise
   {:description (:description spec)
    :options (:options spec)
    :prerequisites (:prerequisites spec)
    :select (dissoc (:select spec) :candidates)
    :empty-message (:empty-message spec)
    :confirm (:confirm spec)
    :details (vec (mutation/details spec))
    :candidates (into (sorted-map)
                      (for [[k objects] clusters]
                        [k (with-redefs [shell/execute (fn [& argv] (answer {:objects objects :answering? true} (vec argv)))]
                             (attempt #(vec ((:candidates (:select spec)) "ctx"))))]))}))

;; --- the books and their scenarios ---------------------------------------------------

(defn- cronjob [suspend] {:metadata (meta-of "batch" "nightly") :spec {:suspend suspend :schedule "0 3 * * *"}})

(defn- deployment
  [& {:keys [paused replicas revision] :or {replicas 2 revision 7}}]
  {:metadata (assoc (meta-of "shop" "web") :annotations {:deployment.kubernetes.io/revision (str revision)})
   :spec (cond-> {:replicas replicas :template {:spec {:containers [{:name "app" :image (str "web:" revision)}]}}}
           paused (assoc :paused true))
   :status {:observedGeneration 1 :replicas replicas :updatedReplicas replicas :availableReplicas replicas :readyReplicas replicas}})

(defn- replicaset
  [name revision]
  {:metadata {:namespace "shop" :name name :annotations {:deployment.kubernetes.io/revision (str revision)}
              :ownerReferences [{:kind "Deployment" :name "web" :uid "uid-web"}]}
   :spec {:template {:spec {:containers [{:name "app" :image (str "web:" revision)}]}}}})

(def ^:private hpa
  {:metadata (meta-of "shop" "web-hpa") :spec {:scaleTargetRef {:kind "Deployment" :name "web"}}})

(def ^:private flux-type "kustomizations.kustomize.toolkit.fluxcd.io")
(def ^:private application-type "applications.argoproj.io")

(defn- kustomization [suspend] {:metadata (meta-of "flux-system" "apps") :spec {:suspend suspend :interval "10m"}})

(defn- application
  [& {:keys [refresh operation]}]
  {:metadata (cond-> (meta-of "argocd" "shop")
               refresh (assoc :annotations {:argocd.argoproj.io/refresh refresh}))
   :spec {:source {:repoURL "https://git.example/shop.git" :targetRevision "main"}}
   :status (cond-> {:sync {:status "OutOfSync"} :health {:status "Healthy"}}
             operation (assoc :operationState {:phase "Running"}))
   :operation (when operation {:sync {}})})

(def ^:private scheduled-backup-type "scheduledbackups.postgresql.cnpg.io")
(def ^:private schedule-type "schedules.velero.io")

(defn- scheduled-backup
  [suspend]
  {:metadata (meta-of "db" "nightly") :spec {:suspend suspend :schedule "0 0 3 * * *" :cluster {:name "pg"}}})

(defn- schedule [paused] {:metadata (meta-of "velero" "daily") :spec {:paused paused :schedule "0 2 * * *"}})

;; CloudNativePG: a Cluster db/pg of two instances, its pods, and Velero
;; Schedules, Backups and storage locations.

(def ^:private pg-type "clusters.postgresql.cnpg.io")
(def ^:private velero-backup-type "backups.velero.io")
(def ^:private location-type "backupstoragelocations.velero.io")
(def ^:private restore-type "restores.velero.io")

(defn- pg-cluster
  [& {:keys [phase fenced hibernation primary target backup?]
      :or {phase "Cluster in healthy state" primary "pg-1"}}]
  {:metadata (assoc (meta-of "db" "pg")
                    :annotations (cond-> {}
                                   fenced (assoc :cnpg.io/fencedInstances fenced)
                                   hibernation (assoc :cnpg.io/hibernation hibernation)))
   :spec (cond-> {:instances 2}
           backup? (assoc :backup {:barmanObjectStore {:destinationPath "s3://backups/pg"}}))
   :status (cond-> {:phase phase :readyInstances 2 :currentPrimary primary :targetPrimary (or target primary)
                    :instancesStatus {:healthy ["pg-1" "pg-2"]}}
             (= "on" hibernation) (assoc :conditions [{:type "cnpg.io/hibernation" :status "True"}]))})

(defn- pg-pod
  [name]
  {:metadata {:namespace "db" :name name :uid (str "uid-" name)
              :labels {:cnpg.io/cluster "pg" :cnpg.io/podRole "instance"}
              :annotations {:kubectl.kubernetes.io/restartedAt "2026-01-01T00:00:00Z"}}})

(defn- velero-schedule
  [& {:keys [phase] :or {phase "Enabled"}}]
  {:metadata (meta-of "velero" "daily")
   :spec {:schedule "0 2 * * *" :template {:storageLocation "default" :includedNamespaces ["shop"] :ttl "720h0m0s"}}
   :status {:phase phase}})

(defn- location
  [phase]
  {:metadata (meta-of "velero" "default") :spec {:default true :accessMode "ReadWrite"} :status {:phase phase}})

(defn- velero-backup
  [phase]
  {:metadata (meta-of "velero" "daily-1") :spec {:storageLocation "default"} :status {:phase phase}})

(def ^:private restore
  {:metadata (meta-of "velero" "daily-1-restore") :spec {:backupName "daily-1"}})

;; A Deployment stuck terminating on a controller's finalizer.

(defn- terminating
  [& {:keys [deleting?] :or {deleting? true}}]
  (update (deployment) :metadata merge
          (cond-> {:finalizers ["example.com/cleanup" "foregroundDeletion"]}
            deleting? (assoc :deletionTimestamp "2026-01-01T00:00:00Z"))))

;; Helm keeps each revision of release shop/web as a Secret, read from its labels.

(defn- helm-revision
  [version status]
  {:metadata {:namespace "shop" :name (str "sh.helm.release.v1.web.v" version) :uid (str "uid-web-v" version)
              :creationTimestamp "2026-01-01T00:00:00Z"
              :labels {:owner "helm" :name "web" :status status :version (str version)
                       :modifiedAt "1767225600" :createdAt "1767225600"}}})

(defn- standard
  "The scenarios every book runs: permitted, refused, absent, a spec change and
   an unanswered read between confirmation and precondition, and a pick that
   does not parse when the book takes a free-form one."
  [{:keys [target opts permitted refused invalid dry-run]}]
  (cond-> {:permitted {:objects permitted :target target :opts opts :dry-run dry-run}
           :refused {:objects refused :target target :opts opts :dry-run dry-run}
           :absent {:objects [] :target target :opts opts :dry-run dry-run}
           :spec-changed {:objects permitted :target target :opts opts :dry-run dry-run :between :spec-changed}
           :unanswered {:objects permitted :target target :opts opts :dry-run dry-run :between :unanswered}}
    invalid (assoc :invalid {:objects permitted :target invalid :opts opts :dry-run dry-run})))

(def books
  {"runbooks/kubernetes/cronjobs/suspend.clj"
   (standard {:target "batch/nightly" :permitted [["cronjobs" (cronjob false)]] :refused [["cronjobs" (cronjob true)]]})

   "runbooks/kubernetes/cronjobs/resume.clj"
   (standard {:target "batch/nightly" :permitted [["cronjobs" (cronjob true)]] :refused [["cronjobs" (cronjob false)]]})

   "runbooks/kubernetes/workloads/pause.clj"
   (standard {:target "Deployment shop/web" :invalid "StatefulSet shop/web"
              :permitted [["deployments" (deployment)] ["horizontalpodautoscalers" hpa]]
              :refused [["deployments" (deployment :paused true)]]})

   "runbooks/kubernetes/workloads/resume.clj"
   (standard {:target "Deployment shop/web" :invalid "StatefulSet shop/web"
              :permitted [["deployments" (deployment :paused true)]]
              :refused [["deployments" (deployment)]]})

   "runbooks/kubernetes/workloads/restart.clj"
   (standard {:target "Deployment shop/web" :invalid "Deployment web"
              :permitted [["deployments" (deployment)]]
              :refused [["deployments" (deployment :paused true)]]})

   "runbooks/kubernetes/workloads/scale.clj"
   (standard {:target "Deployment shop/web" :invalid "DaemonSet shop/web" :opts {:replicas 3}
              :permitted [["deployments" (deployment)]]
              :refused [["deployments" (deployment)] ["horizontalpodautoscalers" hpa]]})

   "runbooks/kubernetes/workloads/rollback.clj"
   (standard {:target "Deployment shop/web" :invalid "Deployment web"
              :permitted [["deployments" (deployment)] ["replicasets" (replicaset "web-b" 6)] ["replicasets" (replicaset "web-c" 7)]]
              :refused [["deployments" (deployment)] ["replicasets" (replicaset "web-c" 7)]]})

   "runbooks/kubernetes/objects/delete.clj"
   (assoc (standard {:target "deployments shop/web" :invalid "secrets shop/web"
                     :permitted [["deployments" (deployment)]]
                     :refused [["deployments" (deployment)]]
                     :dry-run nil})
          :refused {:objects [["deployments" (deployment)]] :target "deployments shop/web"
                    :dry-run {:exit 1 :out "" :err "admission webhook denied the request"}})

   "tools/flux/runbooks/flux/suspend.clj"
   (standard {:target "Kustomization flux-system/apps" :invalid "Bucket flux-system/apps"
              :permitted [[flux-type (kustomization false)]] :refused [[flux-type (kustomization true)]]})

   "tools/flux/runbooks/flux/resume.clj"
   (standard {:target "Kustomization flux-system/apps" :invalid "Bucket flux-system/apps"
              :permitted [[flux-type (kustomization true)]] :refused [[flux-type (kustomization false)]]})

   "tools/cnpg/runbooks/suspend_scheduled_backup.clj"
   (standard {:target "db/nightly" :invalid "nightly"
              :permitted [[scheduled-backup-type (scheduled-backup false)]]
              :refused [[scheduled-backup-type (scheduled-backup true)]]})

   "tools/cnpg/runbooks/resume_scheduled_backup.clj"
   (standard {:target "db/nightly" :invalid "nightly"
              :permitted [[scheduled-backup-type (scheduled-backup true)]]
              :refused [[scheduled-backup-type (scheduled-backup false)]]})

   "tools/velero/runbooks/pause_schedule.clj"
   (standard {:target "velero/daily" :invalid "daily"
              :permitted [[schedule-type (schedule false)]] :refused [[schedule-type (schedule true)]]})

   "tools/velero/runbooks/resume_schedule.clj"
   (standard {:target "velero/daily" :invalid "daily"
              :permitted [[schedule-type (schedule true)]] :refused [[schedule-type (schedule false)]]})

   "tools/argocd/runbooks/refresh.clj"
   (standard {:target "argocd/shop" :invalid "shop" :opts {:hard true}
              :permitted [[application-type (application)]]
              :refused [[application-type (application :refresh "normal")]]})

   "tools/argocd/runbooks/sync.clj"
   (standard {:target "argocd/shop" :invalid "shop" :opts {:prune true}
              :permitted [[application-type (application)]]
              :refused [[application-type (application :operation true)]]})

   "tools/cnpg/runbooks/fence.clj"
   (standard {:target "db/pg pg-2" :invalid "db/pg"
              :permitted [[pg-type (pg-cluster)]]
              :refused [[pg-type (pg-cluster :fenced "[\"pg-2\"]")]]})

   "tools/cnpg/runbooks/unfence.clj"
   (standard {:target "db/pg pg-2" :invalid "db/pg"
              :permitted [[pg-type (pg-cluster :fenced "[\"pg-2\"]")]]
              :refused [[pg-type (pg-cluster :fenced "[\"*\"]")]]})

   "tools/cnpg/runbooks/switchover.clj"
   (standard {:target "db/pg 2" :invalid "pg 2"
              :permitted [[pg-type (pg-cluster)]]
              :refused [[pg-type (pg-cluster :primary "pg-1" :target "pg-2")]]})

   "tools/cnpg/runbooks/hibernate.clj"
   (standard {:target "db/pg" :invalid "pg"
              :permitted [[pg-type (pg-cluster)]]
              :refused [[pg-type (pg-cluster :hibernation "on")]]})

   "tools/cnpg/runbooks/rehydrate.clj"
   (standard {:target "db/pg" :invalid "pg"
              :permitted [[pg-type (pg-cluster :hibernation "on")]]
              :refused [[pg-type (pg-cluster)]]})

   "tools/cnpg/runbooks/restart.clj"
   (standard {:target "db/pg" :invalid "pg"
              :permitted [[pg-type (pg-cluster)] ["pods" (pg-pod "pg-1")] ["pods" (pg-pod "pg-2")]]
              :refused [[pg-type (pg-cluster :phase "Setting up primary")]]})

   "tools/cnpg/runbooks/reload.clj"
   (standard {:target "db/pg" :invalid "pg"
              :permitted [[pg-type (pg-cluster)]]
              :refused [[pg-type (pg-cluster :hibernation "on")]]})

   "tools/cnpg/runbooks/backup.clj"
   (standard {:target "db/pg" :invalid "pg"
              :permitted [[pg-type (pg-cluster :backup? true)]]
              :refused [[pg-type (pg-cluster)]]})

   "tools/velero/runbooks/backup.clj"
   (standard {:target "velero/daily" :invalid "daily"
              :permitted [[schedule-type (velero-schedule)] [location-type (location "Available")]]
              :refused [[schedule-type (velero-schedule)] [location-type (location "Unavailable")]]})

   "tools/velero/runbooks/delete_backup.clj"
   (standard {:target "velero/daily-1" :invalid "daily-1"
              :permitted [[velero-backup-type (velero-backup "Completed")] [location-type (location "Available")]
                          [restore-type restore]]
              :refused [[velero-backup-type (velero-backup "InProgress")] [location-type (location "Available")]]})

   "runbooks/kubernetes/finalizers/remove.clj"
   (standard {:target "deployments shop/web example.com/cleanup" :invalid "deployments shop/web"
              :permitted [["deployments" (terminating)]]
              :refused [["deployments" (terminating :deleting? false)]]})

   "runbooks/kubernetes/helm/rollback.clj"
   (standard {:target "shop/web" :invalid "web"
              :permitted [["secrets" (helm-revision 1 "superseded")] ["secrets" (helm-revision 2 "deployed")]]
              :refused [["secrets" (helm-revision 1 "superseded")] ["secrets" (helm-revision 2 "pending-upgrade")]]})})

(defn- placed
  "[type object] moved to namespace/name, uid and all."
  [[type object] namespace name]
  [type (update object :metadata merge {:namespace namespace :name name :uid (str "uid-" name)})])

(defn- mixed
  "A cluster holding the objects of a permitted and a refused cluster several
   times over, under names out of order, so the candidates show their order
   and their filter."
  [permitted refused]
  (vec (concat (map #(placed % "zeta" "b") permitted)
               (map #(placed % "alpha" "c") refused)
               (map #(placed % "alpha" "a") permitted)
               (map #(placed % "beta" "d") refused))))

(def ^:private mixed-books
  #{"runbooks/kubernetes/cronjobs/suspend.clj" "runbooks/kubernetes/cronjobs/resume.clj"
    "runbooks/kubernetes/workloads/pause.clj" "runbooks/kubernetes/workloads/resume.clj"
    "tools/flux/runbooks/flux/suspend.clj" "tools/flux/runbooks/flux/resume.clj"
    "tools/cnpg/runbooks/suspend_scheduled_backup.clj" "tools/cnpg/runbooks/resume_scheduled_backup.clj"
    "tools/velero/runbooks/pause_schedule.clj" "tools/velero/runbooks/resume_schedule.clj"})

(defn transcripts
  []
  (into (sorted-map)
        (for [[path scenarios] books]
          (let [spec (load-spec path)]
            [path (assoc (into (sorted-map) (for [[k scenario] scenarios] [k (transcript spec scenario)]))
                         :surface (surface spec (let [permitted (get-in scenarios [:permitted :objects])
                                                      refused (get-in scenarios [:refused :objects])]
                                                  (cond-> {:permitted permitted :refused refused}
                                                    (mixed-books path) (assoc :mixed (mixed permitted refused))))))]))))

(defn write-golden!
  []
  (spit golden-path
        ;; split, so a REUSE scan does not read these strings as this file's header
        (str ";; SPDX" "-FileCopyrightText: The gumshoe Authors\n"
             ";; SPDX" "-License-Identifier: 0BSD\n"
             ";; Generated by (gumshoe.mutation-books-test/write-golden!); review its diff.\n"
             (with-out-str (pprint/pprint (transcripts))))))

(deftest every-book-keeps-its-transcripts
  (let [golden (edn/read-string (slurp golden-path))
        actual (transcripts)]
    (is (= (set (keys golden)) (set (keys actual))) "the same books")
    (doseq [[path scenarios] actual
            [scenario transcript] scenarios]
      (testing (str path " " scenario)
        (is (= (get-in golden [path scenario]) transcript))))))

(deftest restart-refuses-a-pick-that-does-not-parse
  (let [{:keys [stopped precondition plan]}
        (transcript (load-spec "runbooks/kubernetes/workloads/restart.clj")
                    {:objects [["deployments" (deployment)]] :target "Deployment web"})]
    (is (nil? stopped) "the book shows its confirmation instead of throwing")
    (is (= {:result false :out "refusing: the pick is not \"<Kind> <namespace>/<name>\""} precondition))
    (is (not-any? #(= :kubectl (first %)) plan) "the plan restarts nothing")))
