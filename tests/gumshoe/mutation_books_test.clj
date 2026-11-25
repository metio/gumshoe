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

   A few books also run for real, with flow/change! and the effect
   interpreter in place and an effect or a post-check failing after an
   earlier effect applied, or rehearsing under --dry-run, and keep the banner
   and the recovery lines the harness prints under it. Some also pick their
   own subject through mutation/run!: a candidate the flags name, one they do
   not, a cluster without candidates and one that does not answer.

   A scenario's :ssh answers the commands a book runs on a host over SSH, by
   the remote command line.

   Regenerate the file after an intended change with
   (gumshoe.mutation-books-test/write-golden!) and review its diff."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.announce :as announce]
            [gumshoe.flow :as flow]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.outcome :as outcome]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]
            [gumshoe.verify :as verify]))

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

(defn- answer-ssh
  "What a host answers for an ssh argv: the entry of `ssh` (remote command
   line -> result) for the command run on the far end, or a clean exit with
   no output."
  [ssh argv]
  (let [command (str/join " " (drop 2 (drop-while #(not= "--" %) argv)))]
    (merge {:exit 0 :out "" :err ""} (get ssh command))))

(defn- answer
  "What kubectl answers for argv against state {:objects [[type object]..]
   :answering? :dry-run :ssh}. A pod read with a spec.nodeName field selector
   lists the pods on that node only."
  [{:keys [objects answering? dry-run ssh]} argv]
  (let [namespace (arg-value argv "--namespace=")
        node (arg-value (filter string? argv) "--field-selector=spec.nodeName=")
        [_ type name] (drop-while #(not= "get" %) argv)
        name (when (and name (not (str/starts-with? name "-"))) name)
        matches (fn [[t o]] (and (= t type)
                                 (or (nil? namespace) (= namespace (-> o :metadata :namespace)))
                                 (or (nil? name) (= name (-> o :metadata :name)))
                                 (or (nil? node) (= node (-> o :spec :nodeName)))))]
    (cond
      (not answering?) unanswered
      (= "ssh" (first argv)) (answer-ssh ssh argv)
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

(defn- reach-change
  "(f out config state) after the book ran in one scenario up to its change,
   with flow/change! captured and the recorded cluster still stubbed: what
   the run printed, the config it handed flow/change! (nil when it stopped
   before) and the cluster's state atom."
  [spec {:keys [objects target opts dry-run ssh]} f]
  (let [state (atom {:objects objects :answering? true :dry-run dry-run :ssh ssh})
        seen (atom nil)]
    (with-redefs [shell/execute (fn [& argv] (answer @state (vec argv)))
                  fuzzy/select-single (constantly nil)
                  fuzzy/select-multi (constantly nil)
                  kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  announce/announce! (fn [_ _ message] message)
                  flow/change! (fn [config] (reset! seen config) true)]
      (let [{:keys [out]} (capture #(try (mutation/run-on! spec target (or opts {}) nil)
                                         (catch Exception e (println "threw:" (type e) (ex-message e)))))]
        (f out @seen state)))))

(defn transcript
  "What the book does in one scenario. A post-check holds as
   verify/eventually would judge it, with strict reads."
  [spec {:keys [between] :as scenario}]
  (reach-change
   spec scenario
   (fn [out seen state]
     (let [{:keys [confirmation precondition announce! effect step post-checks]} seen]
       (case between
         :spec-changed (swap! state update :objects bump-generation)
         :unanswered (swap! state assoc :answering? false)
         nil)
       (normalise
        (if-not seen
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
                                                       :holds (attempt #(true? (verify/attempt check)))}
                                                abort (assoc :abort (attempt abort)))))))))))))

(defn failed-transcript
  "What the book prints when it runs for real against objects and the
   `fail-at`th effect fails, or with every effect applied and nothing on the
   recorded cluster changing, so a post-check does not hold: what the run
   printed, its banner and the recovery lines under it. Post-checks run once,
   with a timeout of 0. With `dry-run?` the run rehearses under --dry-run."
  [spec {:keys [objects target opts fail-at ssh dry-run?]}]
  (let [record (outcome/fresh)
        effects (atom 0)
        exit (fn [] (if (= fail-at (swap! effects inc)) 1 0))
        eventually verify/eventually]
    (with-redefs [shell/execute (fn [& argv] (answer {:objects objects :answering? true :ssh ssh} (vec argv)))
                  shell/run-with-output (fn [& _] (exit))
                  shell/execute-with-stdin (fn [& _] (let [code (exit)]
                                                       {:exit code :out "" :err (if (zero? code) "" "Error from server (Forbidden): refused")}))
                  kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  interact/confirm! (constantly true)
                  announce/announce! (fn [_ _ message] message)
                  verify/eventually (fn [check] (eventually (assoc check :timeout 0)))]
      (let [{:keys [result out]} (capture #(binding [outcome/*record* record
                                                     flow/*dry-run* (boolean dry-run?)]
                                             (try (mutation/run-on! spec target (or opts {}) nil)
                                                  (catch Exception e (println "threw:" (type e) (ex-message e))))))
            {:keys [tone text]} (outcome/banner {:changes? true :outcome (if result :ok :failed)} @record)]
        (normalise
         {:result result
          :out (str/split-lines (stdout/strip-colors out))
          :banner text
          :recovery (vec (outcome/recovery-lines tone @record))})))))

(defn pick-transcript
  "What the book does when it picks its own subject from opts through
   mutation/run!, with the fuzzy finder answering as an operator who picks
   nothing, or answering `chosen`, and flow/change! captured: the result,
   what it printed, and the confirmation and plan of the change it reached."
  [spec {:keys [objects opts answering? ssh chosen] :or {answering? true}}]
  (let [seen (atom nil)]
    (with-redefs [shell/execute (fn [& argv] (answer {:objects objects :answering? answering? :ssh ssh} (vec argv)))
                  fuzzy/select-single (constantly chosen)
                  fuzzy/select-multi (constantly chosen)
                  kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  announce/announce! (fn [_ _ message] message)
                  flow/change! (fn [config] (reset! seen config) true)]
      (let [{:keys [result out]} (capture #(attempt (fn [] (mutation/run! spec (or opts {}) nil))))
            {:keys [confirmation effect]} @seen]
        (normalise
         (cond-> {:result result :out (stdout/strip-colors out)}
           @seen (assoc :action (:action confirmation)
                        :items (vec (:items confirmation))
                        :plan effect)))))))

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

;; Flux HelmReleases: apps/podinfo Ready, failing, or parked after its upgrade
;; retries ran out.

(def ^:private helmrelease-type "helmreleases.helm.toolkit.fluxcd.io")

(defn- helmrelease
  [name & {:keys [ready parked?] :or {ready "True"}}]
  {:metadata (meta-of "apps" name)
   :spec (cond-> {:interval "10m" :chart {:spec {:chart name}}}
           parked? (assoc :upgrade {:retries 0}))
   :status (cond-> {:conditions [{:type "Ready" :status ready :reason (if (= "True" ready) "Succeeded" "UpgradeFailed")
                                  :message "release reconciliation"}]}
             parked? (assoc :upgradeFailures 1))})

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

(defn- ready
  [status]
  {:conditions [{:type "Ready" :status status :reason (if (= "True" status) "Ready" "Failed")}]})

;; cert-manager: Certificate shop/web-tls and ACME Order shop/web-1.

(def ^:private certificate-type "certificates.cert-manager.io")
(def ^:private order-type "orders.acme.cert-manager.io")

(defn- certificate [status] {:metadata (meta-of "shop" "web-tls") :status (ready status)})

(defn- order [state] {:metadata (meta-of "shop" "web-1") :status {:state state}})

;; The Job cronjobs/start creates from batch/nightly, under the suffix the
;; harness fixes.

(defn- job
  [& {:keys [failed?]}]
  {:metadata (meta-of "batch" "nightly-abcde")
   :status (if failed? {:conditions [{:type "Failed" :status "True"}]} {:active 1})})

;; Namespace stuck terminating on the kubernetes finalizer.

(defn- namespace-object
  [phase]
  {:metadata {:name "stuck" :uid "uid-stuck" :resourceVersion "100"}
   :spec {:finalizers ["kubernetes"]}
   :status {:phase phase}})

;; Nodes, the pods on them, and a control-plane node reached over SSH.

(defn- node
  [name & {:keys [unschedulable control-plane?]}]
  {:metadata (cond-> {:name name :uid (str "uid-" name) :resourceVersion "100"}
               control-plane? (assoc :labels {:node-role.kubernetes.io/control-plane ""}))
   :spec (cond-> {} unschedulable (assoc :unschedulable true))})

(defn- pod-on
  [node-name name owner-kind]
  {:metadata {:namespace "shop" :name name :uid (str "uid-" name)
              :ownerReferences [{:kind owner-kind :name (str name "-owner")}]}
   :spec {:nodeName node-name}})

(def ^:private apiserver-container "sudo crictl ps --name kube-apiserver -q")
(def ^:private apiserver-readyz "curl -sk https://localhost:6443/readyz")
(def ^:private apiserver-started "sudo crictl inspect -o go-template --template '{{.status.startedAt}}' abc123")

(defn- apiserver-host
  "What the control-plane node cp-1 answers over SSH, with kube-apiserver
   container abc123 answering /readyz with `readyz`."
  [readyz & {:keys [container] :or {container "abc123"}}]
  {apiserver-container {:out container}
   apiserver-readyz {:out readyz}
   apiserver-started {:out "2026-01-01T00:00:00Z"}})

;; PersistentVolume pv-1 and the claim shop/data.

(defn- volume
  [phase]
  {:metadata {:name "pv-1" :uid "uid-pv-1" :resourceVersion "100"}
   :spec {:persistentVolumeReclaimPolicy "Delete"}
   :status {:phase phase}})

(def ^:private claim {:metadata (meta-of "shop" "data")})

(def ^:private claiming-pod
  {:metadata {:namespace "shop" :name "web-0" :uid "uid-web-0"}
   :spec {:volumes [{:name "data" :persistentVolumeClaim {:claimName "data"}}]}})

;; JaaS JsonnetSnippet apps/site and the StageSet apps/site.

(def ^:private snippet-type "jsonnetsnippets.jaas.metio.wtf")
(def ^:private stageset-type "stagesets.stages.metio.wtf")

(defn- snippet
  [suspend reason]
  {:metadata (meta-of "apps" "site")
   :spec {:suspend suspend}
   :status {:conditions [{:type "Ready" :status (if (= "Succeeded" reason) "True" "False") :reason reason}]}})

(defn- stageset
  [status]
  {:metadata (meta-of "apps" "site")
   :spec {:stages [{:name "dev"} {:name "prod"}]}
   :status (ready status)})

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

(defn- over-ssh
  "The standard scenarios with the host answering `ssh` in each of them, and
   `refused-ssh` in the refused one."
  [scenarios ssh refused-ssh]
  (-> (update-vals scenarios #(assoc % :ssh ssh))
      (assoc-in [:refused :ssh] refused-ssh)))

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

   "tools/flux/runbooks/flux/reconcile.clj"
   (standard {:target "apps/podinfo"
              :permitted [[helmrelease-type (helmrelease "podinfo")]]
              :refused [[helmrelease-type (helmrelease "podinfo" :ready "False")]]})

   "tools/flux/runbooks/flux/reset.clj"
   (standard {:target "apps/podinfo"
              :permitted [[helmrelease-type (helmrelease "podinfo" :ready "False" :parked? true)]
                          [helmrelease-type (helmrelease "alpha")]]
              :refused [[helmrelease-type (helmrelease "podinfo" :ready "False")]]})

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
              :refused [["secrets" (helm-revision 1 "superseded")] ["secrets" (helm-revision 2 "pending-upgrade")]]})

   "runbooks/kubernetes/certificates/renew.clj"
   (standard {:target ["shop/web-tls"]
              :permitted [[certificate-type (certificate "True")]]
              :refused [[certificate-type (certificate "False")]]})

   "runbooks/kubernetes/cronjobs/start.clj"
   (standard {:target "batch/nightly" :invalid "nightly"
              :permitted [["cronjobs" (cronjob false)] ["jobs" (job)]]
              :refused [["cronjobs" (cronjob true)] ["jobs" (job :failed? true)]]})

   "runbooks/kubernetes/namespaces/remove_finalizers.clj"
   (standard {:target "stuck"
              :permitted [["namespaces" (namespace-object "Terminating")]]
              :refused [["namespaces" (namespace-object "Active")]]})

   "runbooks/kubernetes/nodes/cordon.clj"
   (standard {:target "n1"
              :permitted [["nodes" (node "n1")]]
              :refused [["nodes" (node "n1" :unschedulable true)]]})

   "runbooks/kubernetes/nodes/drain.clj"
   (standard {:target "n1" :opts {:grace-period -1}
              :permitted [["nodes" (node "n1" :unschedulable true)] ["pods" (pod-on "n1" "agent" "DaemonSet")]
                          ["pods" (pod-on "n2" "web" "ReplicaSet")]]
              :refused [["nodes" (node "n1")] ["pods" (pod-on "n1" "web" "ReplicaSet")]]})

   "runbooks/kubernetes/nodes/uncordon.clj"
   (standard {:target "n1"
              :permitted [["nodes" (node "n1" :unschedulable true)]]
              :refused [["nodes" (node "n1")]]})

   "runbooks/kubernetes/nodes/restart_apiserver.clj"
   (assoc (over-ssh (standard {:target "cp-1"
                               :permitted [["nodes" (node "cp-1" :control-plane? true)]]
                               :refused [["nodes" (node "cp-1" :control-plane? true)]]})
                    (apiserver-host "ok") (apiserver-host "[-]etcd failed"))
          :no-container {:objects [["nodes" (node "cp-1" :control-plane? true)]] :target "cp-1"
                         :ssh (apiserver-host "ok" :container "")}
          :unreachable {:objects [["nodes" (node "cp-1" :control-plane? true)]] :target "cp-1"
                        :ssh (assoc (apiserver-host "ok") "exit 0" {:exit 255 :err "ssh: connect to host cp-1 port 22: Connection timed out"})}
          :no-sudo {:objects [["nodes" (node "cp-1" :control-plane? true)]] :target "cp-1"
                    :ssh (assoc (apiserver-host "ok") "sudo -n true" {:exit 1 :err "sudo: a password is required"})})

   "runbooks/kubernetes/orders/recreate.clj"
   (standard {:target ["shop/web-1"]
              :permitted [[order-type (order "invalid")]]
              :refused [[order-type (order "valid")]]})

   "runbooks/kubernetes/volumes/delete_released.clj"
   (standard {:target ["pv-1"]
              :permitted [["persistentvolumes" (volume "Released")]]
              :refused [["persistentvolumes" (volume "Bound")]]})

   "runbooks/kubernetes/volumes/delete_unused.clj"
   (standard {:target "shop/data" :invalid "data"
              :permitted [["persistentvolumeclaims" claim]]
              :refused [["persistentvolumeclaims" claim] ["pods" claiming-pod]]})

   "tools/jaas/runbooks/jaas/reconcile.clj"
   (standard {:target "apps/site" :invalid "site"
              :permitted [[snippet-type (snippet false "Succeeded")]]
              :refused [[snippet-type (snippet true "Suspended")]]})

   "tools/jaas/runbooks/jaas/suspend.clj"
   (standard {:target "apps/site" :invalid "site"
              :permitted [[snippet-type (snippet false "Succeeded")]]
              :refused [[snippet-type (snippet true "Suspended")]]})

   "tools/jaas/runbooks/jaas/resume.clj"
   (standard {:target "apps/site" :invalid "site"
              :permitted [[snippet-type (snippet true "Suspended")]]
              :refused [[snippet-type (snippet false "Succeeded")]]})

   "tools/stageset/runbooks/stageset/promote.clj"
   (assoc (standard {:target "apps/site" :invalid "site" :opts {:stage "prod"}
                     :permitted [[stageset-type (stageset "True")]]
                     :refused [[stageset-type (stageset "False")]]})
          :unknown-stage {:objects [[stageset-type (stageset "True")]] :target "apps/site" :opts {:stage "qa"}})

   "tools/stageset/runbooks/stageset/reconcile.clj"
   (standard {:target "apps/site" :invalid "site" :opts {:update-now true}
              :permitted [[stageset-type (stageset "True")]]
              :refused [[stageset-type (stageset "False")]]})})

(def ^:private book-redefs
  "Vars a book defines that the harness fixes, by the book's path: the
   random suffix cronjobs/start gives the Job it creates."
  {"runbooks/kubernetes/cronjobs/start.clj"
   {'runbooks.kubernetes.cronjobs.start/random-suffix (constantly "abcde")}})

(defn- pick-scenarios
  "The scenarios a book runs through its own selection: opts that pick a
   candidate of objects, opts that pick none, a cluster without candidates and
   a cluster that does not answer."
  [objects picked unpicked]
  {:picked {:objects objects :opts picked}
   :unpicked {:objects objects :opts unpicked}
   :empty {:objects [] :opts picked}
   :unanswered-pick {:objects objects :opts picked :answering? false}})

(defn- with-unparsable-pick
  "scenarios and one more: no flags, with the fuzzy finder answering `chosen`,
   a line that does not parse as the book's pick."
  [scenarios chosen]
  (assoc scenarios :unparsable-pick {:objects (get-in scenarios [:picked :objects]) :opts {} :chosen chosen}))

(def ^:private picks
  "Books that also pick their subject through mutation/run!, by path."
  {"runbooks/kubernetes/certificates/renew.clj"
   (pick-scenarios [[certificate-type (certificate "True")]] {:certificates ["shop/web-tls"]} {:certificates ["shop/nope"]})

   "runbooks/kubernetes/cronjobs/start.clj"
   (with-unparsable-pick
    (pick-scenarios [["cronjobs" (cronjob false)]] {:namespace "batch" :job "nightly"} {:namespace "batch" :job "nope"})
    "nightly")

   "runbooks/kubernetes/namespaces/remove_finalizers.clj"
   (pick-scenarios [["namespaces" (namespace-object "Terminating")]] {:namespace "stuck"} {:namespace "nope"})

   "runbooks/kubernetes/nodes/cordon.clj"
   (pick-scenarios [["nodes" (node "n1")] ["nodes" (node "n2" :unschedulable true)]] {:node "n1"} {:node "n2"})

   "runbooks/kubernetes/nodes/drain.clj"
   (pick-scenarios [["nodes" (node "n1")]] {:node "n1" :grace-period -1} {:node "n9" :grace-period -1})

   "runbooks/kubernetes/nodes/uncordon.clj"
   (pick-scenarios [["nodes" (node "n1")] ["nodes" (node "n2" :unschedulable true)]] {:node "n2"} {:node "n1"})

   "runbooks/kubernetes/nodes/restart_apiserver.clj"
   (update-vals (pick-scenarios [["nodes" (node "cp-1" :control-plane? true)] ["nodes" (node "w-1")]] {:node "cp-1"} {:node "w-1"})
                #(assoc % :ssh (apiserver-host "ok")))

   "runbooks/kubernetes/orders/recreate.clj"
   (pick-scenarios [[order-type (order "invalid")]] {:orders ["shop/web-1"]} {:orders ["shop/web-2"]})

   "runbooks/kubernetes/volumes/delete_released.clj"
   (pick-scenarios [["persistentvolumes" (volume "Released")]] {:volumes ["pv-1"]} {:volumes ["pv-2"]})

   "runbooks/kubernetes/volumes/delete_unused.clj"
   (with-unparsable-pick
    (pick-scenarios [["persistentvolumeclaims" claim]] {:namespace "shop" :pvc "data"} {:namespace "shop" :pvc "nope"})
    "data")

   "tools/jaas/runbooks/jaas/reconcile.clj"
   (with-unparsable-pick
    (pick-scenarios [[snippet-type (snippet false "Succeeded")]] {:namespace "apps" :name "site"} {:namespace "apps" :name "nope"})
    "site")

   "tools/jaas/runbooks/jaas/suspend.clj"
   (with-unparsable-pick
    (pick-scenarios [[snippet-type (snippet false "Succeeded")]] {:namespace "apps" :name "site"} {:namespace "apps" :name "nope"})
    "site")

   "tools/jaas/runbooks/jaas/resume.clj"
   (with-unparsable-pick
    (pick-scenarios [[snippet-type (snippet true "Suspended")]] {:namespace "apps" :name "site"} {:namespace "apps" :name "nope"})
    "site")

   "tools/stageset/runbooks/stageset/promote.clj"
   (with-unparsable-pick
    (pick-scenarios [[stageset-type (stageset "True")]] {:namespace "apps" :name "site" :stage "prod"} {:namespace "apps" :name "nope" :stage "prod"})
    "site")

   "tools/stageset/runbooks/stageset/reconcile.clj"
   (with-unparsable-pick
    (pick-scenarios [[stageset-type (stageset "True")]] {:namespace "apps" :name "site"} {:namespace "apps" :name "nope"})
    "site")})

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

(def ^:private failing
  "Books that also run for real, by the scenario name their failing run is
   kept under."
  {"tools/flux/runbooks/flux/resume.clj"
   {:failed-after-applying {:objects [[flux-type (kustomization true)]] :target "Kustomization flux-system/apps" :fail-at 2}}

   "runbooks/kubernetes/cronjobs/suspend.clj"
   {:failed-after-applying {:objects [["cronjobs" (cronjob false)]] :target "batch/nightly"}}

   "runbooks/kubernetes/workloads/scale.clj"
   {:failed-after-applying {:objects [["deployments" (deployment)]] :target "Deployment shop/web" :opts {:replicas 3}}}

   "runbooks/kubernetes/cronjobs/resume.clj"
   {:failed-at-the-first-effect {:objects [["cronjobs" (cronjob true)]] :target "batch/nightly" :fail-at 1}}

   "runbooks/kubernetes/certificates/renew.clj"
   {:failed-after-applying {:objects [[certificate-type (certificate "False")]] :target ["shop/web-tls"]}
    :dry-run {:objects [[certificate-type (certificate "True")]] :target ["shop/web-tls"] :dry-run? true}}

   "runbooks/kubernetes/cronjobs/start.clj"
   {:failed-at-the-first-effect {:objects [["cronjobs" (cronjob false)]] :target "batch/nightly" :fail-at 1}
    :dry-run {:objects [["cronjobs" (cronjob false)]] :target "batch/nightly" :dry-run? true}}

   "runbooks/kubernetes/namespaces/remove_finalizers.clj"
   {:failed-at-the-first-effect {:objects [["namespaces" (namespace-object "Terminating")]] :target "stuck" :fail-at 1}
    :failed-after-applying {:objects [["namespaces" (namespace-object "Terminating")]] :target "stuck"}
    :dry-run {:objects [["namespaces" (namespace-object "Terminating")]] :target "stuck" :dry-run? true}}

   "runbooks/kubernetes/nodes/cordon.clj"
   {:failed-after-applying {:objects [["nodes" (node "n1")]] :target "n1"}
    :dry-run {:objects [["nodes" (node "n1")]] :target "n1" :dry-run? true}}

   "runbooks/kubernetes/nodes/drain.clj"
   {:failed-at-the-first-effect {:objects [["nodes" (node "n1")]] :target "n1" :opts {:grace-period -1} :fail-at 1}
    :failed-after-applying {:objects [["nodes" (node "n1" :unschedulable true)] ["pods" (pod-on "n1" "web" "ReplicaSet")]]
                            :target "n1" :opts {:grace-period 30}}
    :dry-run {:objects [["nodes" (node "n1")]] :target "n1" :opts {:grace-period -1} :dry-run? true}}

   "runbooks/kubernetes/nodes/uncordon.clj"
   {:failed-after-applying {:objects [["nodes" (node "n1" :unschedulable true)]] :target "n1"}}

   "runbooks/kubernetes/nodes/restart_apiserver.clj"
   {:failed-after-applying {:objects [["nodes" (node "cp-1" :control-plane? true)]] :target "cp-1" :ssh (apiserver-host "ok")}
    :failed-at-the-second-effect {:objects [["nodes" (node "cp-1" :control-plane? true)]] :target "cp-1"
                                  :ssh (apiserver-host "ok") :fail-at 2}
    :dry-run {:objects [["nodes" (node "cp-1" :control-plane? true)]] :target "cp-1" :ssh (apiserver-host "ok") :dry-run? true}}

   "runbooks/kubernetes/orders/recreate.clj"
   {:failed-after-applying {:objects [[order-type (order "invalid")]] :target ["shop/web-1"]}}

   "runbooks/kubernetes/volumes/delete_released.clj"
   {:failed-after-applying {:objects [["persistentvolumes" (volume "Released")]] :target ["pv-1"]}
    :dry-run {:objects [["persistentvolumes" (volume "Released")]] :target ["pv-1"] :dry-run? true}}

   "runbooks/kubernetes/volumes/delete_unused.clj"
   {:failed-after-applying {:objects [["persistentvolumeclaims" claim]] :target "shop/data"}}

   "tools/jaas/runbooks/jaas/reconcile.clj"
   {:failed-after-applying {:objects [[snippet-type (snippet false "Succeeded")]] :target "apps/site"}}

   "tools/jaas/runbooks/jaas/suspend.clj"
   {:failed-after-applying {:objects [[snippet-type (snippet false "Succeeded")]] :target "apps/site"}
    :failed-at-the-first-effect {:objects [[snippet-type (snippet false "Succeeded")]] :target "apps/site" :fail-at 1}}

   "tools/jaas/runbooks/jaas/resume.clj"
   {:failed-after-applying {:objects [[snippet-type (snippet true "Suspended")]] :target "apps/site"}}

   "tools/stageset/runbooks/stageset/promote.clj"
   {:failed-at-the-first-effect {:objects [[stageset-type (stageset "True")]] :target "apps/site" :opts {:stage "prod"} :fail-at 1}
    :dry-run {:objects [[stageset-type (stageset "True")]] :target "apps/site" :opts {:stage "prod"} :dry-run? true}}

   "tools/stageset/runbooks/stageset/reconcile.clj"
   {:failed-after-applying {:objects [[stageset-type (stageset "False")]] :target "apps/site" :opts {:update-now true}}}})

(defn- with-book-redefs
  "(f) with the vars book-redefs names for the book at path fixed; the book
   is loaded already, so its vars resolve."
  [path f]
  (if-let [redefs (get book-redefs path)]
    (with-redefs-fn (into {} (for [[sym value] redefs] [(resolve sym) value])) f)
    (f)))

(defn transcripts
  []
  (binding [ssh/*user-config* nil]
    (into (sorted-map)
          (for [[path scenarios] books]
            (let [spec (load-spec path)]
              [path (with-book-redefs
                      path
                      #(assoc (into (sorted-map) (concat (for [[k scenario] scenarios] [k (transcript spec scenario)])
                                                         (for [[k scenario] (get failing path)] [k (failed-transcript spec scenario)])
                                                         (for [[k scenario] (get picks path)] [k (pick-transcript spec scenario)])))
                              :surface (surface spec (let [permitted (get-in scenarios [:permitted :objects])
                                                           refused (get-in scenarios [:refused :objects])]
                                                       (cond-> {:permitted permitted :refused refused}
                                                         (mixed-books path) (assoc :mixed (mixed permitted refused)))))))])))))

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
  (is (= {:stopped "refusing: the pick is not \"<Kind> <namespace>/<name>\""}
         (transcript (load-spec "runbooks/kubernetes/workloads/restart.clj")
                     {:objects [["deployments" (deployment)]] :target "Deployment web"}))
      "the book refuses before it shows a confirmation or computes a plan"))

(defn- unparsable-targets
  "Picks no book of the select's mode accepts: nothing, blank text and, with
   `invalid`, the book's own unparsable pick."
  [{:keys [mode]} invalid]
  (cond-> (if (= :many mode) [nil [] [nil] [""] ["  "]] [nil "" "  "])
    invalid (conj invalid)))

(def ^:private judged-by-the-book
  "Books whose :invalid pick has the shape of a pick and is refused by the
   book's own :refusal, after the confirmation: helm/rollback reads its
   free-form :one pick itself, and workloads/scale parses a DaemonSet it then
   refuses to scale."
  #{"runbooks/kubernetes/helm/rollback.clj" "runbooks/kubernetes/workloads/scale.clj"})

(deftest every-book-refuses-a-pick-that-does-not-parse-before-its-confirmation
  (binding [ssh/*user-config* nil]
    (doseq [[path scenarios] books
            :let [spec (load-spec path)
                  {:keys [objects opts]} (:permitted scenarios)
                  invalid (when-not (judged-by-the-book path) (get-in scenarios [:invalid :target]))]
            target (unparsable-targets (:select spec) invalid)]
      (testing (str path " run-on! " (pr-str target))
        (let [{:keys [stopped] :as t} (with-book-redefs path #(transcript spec {:objects objects :target target :opts opts}))]
          (is (and stopped (str/starts-with? stopped "refusing: the pick")) (pr-str t)))))
    (doseq [path judged-by-the-book
            :let [scenario (get-in books [path :invalid])]]
      (testing (str path " judges its own pick")
        (is (false? (get-in (transcript (load-spec path) scenario) [:precondition :result])))))))

(deftest every-book-refuses-an-unparsable-pick-through-its-own-selection
  (binding [ssh/*user-config* nil]
    (doseq [[path scenarios] picks
            :let [spec (load-spec path)]
            :when (:unparsable-pick scenarios)]
      (testing path
        (let [{:keys [result out action]} (with-book-redefs path #(pick-transcript spec (:unparsable-pick scenarios)))]
          (is (false? result))
          (is (nil? action) "no confirmation is shown")
          (is (re-find #"refusing: the pick \"\w+\" does not name a [\w ]+$" out) out))))))

(defn- post-checks-of
  "The post-checks the book hands flow/change! in its permitted scenario."
  [spec scenarios]
  (reach-change spec (:permitted scenarios) (fn [_ seen _] (vec (:post-checks seen)))))

(deftest every-post-check-fails-closed
  (binding [ssh/*user-config* nil]
    (doseq [[path scenarios] books
            :when (:permitted scenarios)
            :let [spec (load-spec path)
                  checks (with-book-redefs path #(post-checks-of spec scenarios))]]
      (testing path
        (is (seq checks) "the permitted scenario reaches the change and its post-checks")
        (is (= [] (verify/fail-closed-findings checks))
            "no post-check holds on a cluster that does not answer, nor on one holding nothing unless it verifies an absence")))))
