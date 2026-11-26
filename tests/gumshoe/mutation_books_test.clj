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
   spec change (the generation moves), a kubectl that stops answering, or
   {:objects [..]}, the cluster holding those objects instead (the state the
   change rests on moved).
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
            [gumshoe.change :as change]
            [gumshoe.announce :as announce]
            [gumshoe.flow :as flow]
            [gumshoe.streams-support :as streams]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.outcome :as outcome]
            [gumshoe.select :as select]
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

(defn- discovery
  "What kubectl api-resources lists as namespaced types: configmaps and the
   type of every namespaced object the cluster holds."
  [objects]
  {:exit 0 :err ""
   :out (str/join "\n" (distinct (cons "configmaps" (for [[t o] objects :when (-> o :metadata :namespace)] t))))})

(defn- answer
  "What kubectl answers for argv against state {:objects [[type object]..]
   :answering? :dry-run :ssh :api-resources}. A pod read with a spec.nodeName
   field selector lists the pods on that node only. A get may name several
   types joined by commas. api-resources lists the namespaced types, or
   answers :api-resources when the state holds one."
  [{:keys [objects answering? dry-run ssh api-resources]} argv]
  (let [namespace (arg-value argv "--namespace=")
        node (arg-value (filter string? argv) "--field-selector=spec.nodeName=")
        [_ type name] (drop-while #(not= "get" %) argv)
        name (when (and name (not (str/starts-with? name "-"))) name)
        types (set (str/split (str type) #","))
        matches (fn [[t o]] (and (contains? types t)
                                 (or (nil? namespace) (= namespace (-> o :metadata :namespace)))
                                 (or (nil? name) (= name (-> o :metadata :name)))
                                 (or (nil? node) (= node (-> o :spec :nodeName)))))]
    (cond
      (not answering?) unanswered
      (= "ssh" (first argv)) (answer-ssh ssh argv)
      (some #{"--dry-run=server"} argv) (or dry-run {:exit 0 :out "deleted (server dry run)" :err ""})
      (some #{"api-resources"} argv) (or api-resources (discovery objects))
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
  "objects after a write that raised their generation and changed nothing
   else: a status write, for a type whose CRD has no status subresource."
  [objects]
  (mapv (fn [[t o]] [t (update-in o [:metadata :generation] (fnil inc 0))]) objects))

(defn- change-spec
  "objects after a spec change: their spec gained a field and the API server
   raised their generation, as it does for every spec change."
  [objects]
  (mapv (fn [[t o]] [t (assoc-in o [:spec :changedWhileConfirming] true)]) (bump-generation objects)))

;; --- running a book ------------------------------------------------------------------

(defn load-spec
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
  (let [{:keys [result err out]} (streams/separated thunk)]
    (streams/with-stdout {:result result :out (str/trim err)} out str/trim)))

(defn- threw
  "Prints a harness's note of an exception on stderr, among the run's other
   diagnostics."
  [& parts]
  (binding [*out* *err*] (apply println "threw:" parts)))

(defn- attempt
  [f]
  (try (f) (catch Exception e (str "threw: " (ex-message e)))))

(defn- reach-change
  "(f out config state stdout) after the book ran in one scenario up to its
   change, with flow/change! captured and the recorded cluster still stubbed:
   what the run printed on stderr, the config it handed flow/change! (nil
   when it stopped before), the cluster's state atom and what reached stdout
   (nil when nothing did)."
  [spec {:keys [objects target opts dry-run ssh answering? api-resources] :or {answering? true}} f]
  (let [state (atom {:objects objects :answering? answering? :dry-run dry-run :ssh ssh
                     :api-resources api-resources :commands []})
        seen (atom nil)]
    (with-redefs [shell/execute (fn [& argv]
                                  (swap! state update :commands conj (vec argv))
                                  (answer @state (vec argv)))
                  fuzzy/select-single (constantly nil)
                  fuzzy/select-multi (constantly nil)
                  kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  announce/announce! (fn [_ _ message] message)
                  flow/change! (fn [config] (reset! seen config) true)]
      (let [{:keys [out stdout]} (capture #(try (mutation/run-on! spec target (or opts {}) nil)
                                                (catch Exception e (threw (type e) (ex-message e)))))]
        (f out @seen state stdout)))))

(defn transcript
  "What the book does in one scenario. A post-check holds as
   verify/eventually would judge it, with strict reads."
  [spec {:keys [between] :as scenario}]
  (reach-change
   spec scenario
   (fn [out seen state stdout]
     (let [{:keys [confirmation precondition announce! effect step post-checks]} seen]
       (cond
         (= :spec-changed between) (swap! state update :objects change-spec)
         (= :status-written between) (swap! state update :objects bump-generation)
         (= :unanswered between) (swap! state assoc :answering? false)
         (map? between) (swap! state assoc :objects (:objects between)))
       (normalise
        (if-not seen
          (cond-> {:stopped out} stdout (assoc :stdout stdout))
          (cond-> {:action (:action confirmation)
                   :destructive? (boolean (:destructive? confirmation))
                   :items (vec (:items confirmation))
                   :step step
                   :plan effect
                   :precondition (when precondition (capture precondition))}
            (seq out) (assoc :out out)
            stdout (assoc :stdout stdout)
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
                  shell/run-attributed (fn [& _] (exit))
                  shell/execute-with-stdin (fn [& _] (let [code (exit)]
                                                       {:exit code :out "" :err (if (zero? code) "" "Error from server (Forbidden): refused")}))
                  kubectl/current-context (constantly "ctx")
                  kubectl/current-cluster (constantly "cluster")
                  interact/confirm! (constantly true)
                  announce/announce! (fn [_ _ message] message)
                  verify/eventually (fn [check] (eventually (assoc check :timeout 0)))]
      (let [{:keys [result out stdout]} (capture #(binding [outcome/*record* record
                                                            flow/*dry-run* (boolean dry-run?)]
                                                    (try (mutation/run-on! spec target (or opts {}) nil)
                                                         (catch Exception e (threw (type e) (ex-message e))))))
            {:keys [tone text]} (outcome/banner {:changes? true :outcome (if result :ok :failed)} @record)]
        (normalise
         (cond-> {:result result
                  :out (str/split-lines (stdout/strip-colors out))
                  :banner text
                  :recovery (vec (outcome/recovery-lines tone @record))}
           stdout (assoc :stdout (str/split-lines (stdout/strip-colors stdout)))))))))

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
      (let [{:keys [result out stdout]} (capture #(attempt (fn [] (mutation/run! spec (or opts {}) nil))))
            {:keys [confirmation effect]} @seen]
        (normalise
         (cond-> {:result result :out (stdout/strip-colors out)}
           stdout (assoc :stdout (stdout/strip-colors stdout))
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

(defn- certificate
  "Certificate shop/web-tls with its Ready condition at status, at revision
   (none by default); failed-at stamps a failed issuance with an Issuing
   condition False carrying its message."
  [status & {:keys [revision failed-at]}]
  {:metadata (meta-of "shop" "web-tls")
   :status (cond-> (ready status)
             revision (assoc :revision revision)
             failed-at (-> (assoc :lastFailureTime failed-at)
                           (update :conditions conj {:type "Issuing" :status "False" :reason "Failed"
                                                     :message "The certificate request has failed to complete and will be retried: rate limited"})))})

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

(defn- heartbeat
  "[type node] as the kubelet leaves it after a status write: a higher
   resourceVersion and a newer Ready condition, the spec untouched."
  [[type node]]
  [type (-> node
            (assoc-in [:metadata :resourceVersion] "101")
            (assoc-in [:status :conditions] [{:type "Ready" :status "True" :lastHeartbeatTime "2026-11-25T10:00:40Z"}]))])

(defn- tainted
  "[type node] with a NoSchedule taint added to its spec."
  [[type node]]
  [type (-> node
            (assoc-in [:metadata :resourceVersion] "101")
            (update-in [:spec :taints] (fnil conj []) {:key "node.kubernetes.io/maintenance" :effect "NoSchedule"}))])

(defn- node-moves
  "The scenarios of a node book in which its node moves during the
   confirmation: a status write, which leaves the pin alone, and a taint,
   which refuses the change. The node is the first of permitted."
  [{:keys [target opts permitted]}]
  (let [moved (fn [f] (into [(f (first permitted))] (rest permitted)))]
    {:status-updated {:objects permitted :target target :opts opts :between {:objects (moved heartbeat)}}
     :tainted {:objects permitted :target target :opts opts :between {:objects (moved tainted)}}}))

(defn- pod-on
  [node-name name owner-kind]
  {:metadata {:namespace "shop" :name name :uid (str "uid-" name)
              :ownerReferences [{:kind owner-kind :name (str name "-owner")}]}
   :spec {:nodeName node-name}})

(def ^:private apiserver-container "sudo crictl ps --name kube-apiserver -q")
(def ^:private apiserver-readyz "curl -sk --max-time 10 https://localhost:6443/readyz")
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

(defn- held-stageset
  "StageSet apps/site whose stage prod stands in promotion phase `phase` at
   the revision the rollout is on, with the Ready condition a held gate
   writes."
  [phase]
  {:metadata (meta-of "apps" "site")
   :spec {:stages [{:name "dev" :sourceRef {:name "site"}}
                   {:name "prod" :sourceRef {:name "site"} :promotion {:requireManualPromotion true}}]}
   :status {:observedGeneration 1
            :conditions [{:type "Ready" :status "False" :reason "AwaitingPromotion"}]
            :lastAttemptedRevisions {:apps/site "sha256:abc"}
            :stages [{:name "dev" :appliedRevision "sha256:abc"}
                     {:name "prod" :appliedRevision "sha256:abc" :promotionState {:phase phase}}]}})

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
   (assoc (standard {:target "Deployment shop/web" :invalid "DaemonSet shop/web" :opts {:replicas 3}
                     :permitted [["deployments" (deployment)]]
                     :refused [["deployments" (deployment)] ["horizontalpodautoscalers" hpa]]})
          ;; three ready replicas already: done, even with an autoscaler the refusal would name
          :done {:objects [["deployments" (deployment :replicas 3)] ["horizontalpodautoscalers" hpa]]
                 :target "Deployment shop/web" :opts {:replicas 3}})

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

   ;; the Application CRD has no status subresource, so the controller's
   ;; status writes raise the generation; :status-written shows that alone
   ;; does not refuse the change
   "tools/argocd/runbooks/refresh.clj"
   (assoc (standard {:target "argocd/shop" :invalid "shop" :opts {:hard true}
                     :permitted [[application-type (application)]]
                     :refused [[application-type (application :refresh "normal")]]})
          :status-written {:objects [[application-type (application)]] :target "argocd/shop" :opts {:hard true}
                           :between :status-written})

   "tools/argocd/runbooks/sync.clj"
   (assoc (standard {:target "argocd/shop" :invalid "shop" :opts {:prune true}
                     :permitted [[application-type (application)]]
                     :refused [[application-type (application :operation true)]]})
          :status-written {:objects [[application-type (application)]] :target "argocd/shop" :opts {:prune true}
                           :between :status-written})

   "tools/cnpg/runbooks/fence.clj"
   (standard {:target "db/pg pg-2" :invalid "db/pg"
              :permitted [[pg-type (pg-cluster)]]
              :refused [[pg-type (pg-cluster :fenced "[\"pg-2\"]")]]})

   "tools/cnpg/runbooks/unfence.clj"
   (assoc (standard {:target "db/pg pg-2" :invalid "db/pg"
                     :permitted [[pg-type (pg-cluster :fenced "[\"pg-2\"]")]]
                     :refused [[pg-type (pg-cluster :fenced "[\"*\"]")]]})
          :done {:objects [[pg-type (pg-cluster :fenced "[]")]] :target "db/pg pg-2"})

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
   (let [before [[certificate-type (certificate "True" :revision 1)]]]
     (assoc (standard {:target ["shop/web-tls"]
                       :permitted before
                       :refused [[certificate-type (certificate "False")]]})
            ;; cert-manager stored a new certificate after the renewal
            :reissued {:objects before :target ["shop/web-tls"]
                       :between {:objects [[certificate-type (certificate "True" :revision 2)]]}}
            ;; still Ready on the old certificate: no evidence of a renewal
            :not-reissued {:objects before :target ["shop/web-tls"]}
            :issuance-failed {:objects before :target ["shop/web-tls"]
                              :between {:objects [[certificate-type (certificate "True" :revision 1
                                                                                 :failed-at "2026-11-25T10:00:00Z")]]}}))

   "runbooks/kubernetes/cronjobs/start.clj"
   (standard {:target "batch/nightly" :invalid "nightly"
              :permitted [["cronjobs" (cronjob false)] ["jobs" (job)]]
              :refused [["cronjobs" (cronjob true)] ["jobs" (job :failed? true)]]})

   "runbooks/kubernetes/namespaces/remove_finalizers.clj"
   (standard {:target "stuck"
              :permitted [["namespaces" (namespace-object "Terminating")]]
              :refused [["namespaces" (namespace-object "Active")]]})

   "runbooks/kubernetes/nodes/cordon.clj"
   (let [book {:target "n1"
               :permitted [["nodes" (node "n1")]]
               :refused [["nodes" (node "n1" :unschedulable true)]]}]
     (merge (standard book) (node-moves book)))

   "runbooks/kubernetes/nodes/drain.clj"
   (let [book {:target "n1" :opts {:grace-period -1}
               :permitted [["nodes" (node "n1" :unschedulable true)] ["pods" (pod-on "n1" "agent" "DaemonSet")]
                           ["pods" (pod-on "n1" "api" "ReplicaSet")] ["pods" (pod-on "n2" "web" "ReplicaSet")]]
               :refused [["nodes" (node "n1")] ["pods" (pod-on "n1" "web" "ReplicaSet")]]}]
     (assoc (merge (standard book) (node-moves book))
            ;; cordoned, with nothing a drain evicts left on it: already done
            :done {:objects [["nodes" (node "n1" :unschedulable true)] ["pods" (pod-on "n1" "agent" "DaemonSet")]
                             ["pods" (pod-on "n2" "web" "ReplicaSet")]]
                   :target "n1" :opts {:grace-period -1}}))

   "runbooks/kubernetes/nodes/uncordon.clj"
   (let [book {:target "n1"
               :permitted [["nodes" (node "n1" :unschedulable true)]]
               :refused [["nodes" (node "n1")]]}]
     (merge (standard book) (node-moves book)))

   "runbooks/kubernetes/nodes/restart_apiserver.clj"
   (assoc (over-ssh (let [book {:target "cp-1"
                                :permitted [["nodes" (node "cp-1" :control-plane? true)]]
                                :refused [["nodes" (node "cp-1" :control-plane? true)]]}]
                      (merge (standard book) (node-moves book)))
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
                     :permitted [[stageset-type (held-stageset "AwaitingManual")]]
                     :refused [[stageset-type (held-stageset "Promoted")]]})
          :unknown-stage {:objects [[stageset-type (held-stageset "AwaitingManual")]] :target "apps/site" :opts {:stage "qa"}})

   "tools/stageset/runbooks/stageset/reconcile.clj"
   (standard {:target "apps/site" :invalid "site" :opts {:update-now true}
              :permitted [[stageset-type (stageset "True")]]
              :refused [[stageset-type (assoc-in (stageset "True") [:spec :suspend] true)]]})})

(def ^:private book-redefs
  "Vars a book defines or calls that the harness fixes, by the book's path:
   the random suffix cronjobs/start gives the Job it creates, and the token
   argocd sync writes into its operation."
  {"runbooks/kubernetes/cronjobs/start.clj"
   {'runbooks.kubernetes.cronjobs.start/random-suffix (constantly "abcde")}
   "tools/argocd/runbooks/sync.clj"
   {'gumshoe.argocd-actions/request-token (constantly "request-token")}})

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
    (pick-scenarios [[stageset-type (held-stageset "AwaitingManual")]] {:namespace "apps" :name "site" :stage "prod"} {:namespace "apps" :name "nope" :stage "prod"})
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
    :failed-at-the-first-effect {:objects [["nodes" (node "cp-1" :control-plane? true)]] :target "cp-1"
                                 :ssh (apiserver-host "ok") :fail-at 1}
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
   {:failed-at-the-first-effect {:objects [[stageset-type (held-stageset "AwaitingManual")]] :target "apps/site" :opts {:stage "prod"} :fail-at 1}
    :dry-run {:objects [[stageset-type (held-stageset "AwaitingManual")]] :target "apps/site" :opts {:stage "prod"} :dry-run? true}}

   "tools/stageset/runbooks/stageset/reconcile.clj"
   {:failed-after-applying {:objects [[stageset-type (stageset "False")]] :target "apps/site" :opts {:update-now true}}}})

(defn with-book-redefs
  "(f) with the vars book-redefs names for the book at path fixed; the book
   is loaded already, so its vars resolve."
  [path f]
  (if-let [redefs (get book-redefs path)]
    (with-redefs-fn (into {} (for [[sym value] redefs] [(resolve sym) value])) f)
    (f)))

(defn book-transcripts
  "Every transcript the book at path keeps, scenario by scenario, with spec
   run in its place: the spec its file holds, or another spec - a
   gumshoe.dsl form of it - that must keep the same transcripts."
  [path spec]
  (let [scenarios (get books path)]
    (binding [ssh/*user-config* nil]
      (with-book-redefs
        path
        #(assoc (into (sorted-map) (concat (for [[k scenario] scenarios] [k (transcript spec scenario)])
                                           (for [[k scenario] (get failing path)] [k (failed-transcript spec scenario)])
                                           (for [[k scenario] (get picks path)] [k (pick-transcript spec scenario)])))
                :surface (surface spec (let [permitted (get-in scenarios [:permitted :objects])
                                             refused (get-in scenarios [:refused :objects])]
                                         (cond-> {:permitted permitted :refused refused}
                                           (mixed-books path) (assoc :mixed (mixed permitted refused))))))))))

(defn transcripts
  []
  (into (sorted-map)
        (for [path (keys books)]
          [path (book-transcripts path (load-spec path))])))

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
   book's own :refusal, judged before the confirmation: helm/rollback reads
   its free-form :one pick itself, and workloads/scale parses a DaemonSet it
   then refuses to scale."
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
        (let [{:keys [stopped] :as t} (transcript (load-spec path) scenario)]
          (is (and stopped (re-find #"(?m)^refusing: " stopped)) (pr-str t)))))))

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

(defn post-checks-of
  "The post-checks the book hands flow/change! in its permitted scenario."
  [spec scenarios]
  (reach-change spec (:permitted scenarios) (fn [_ seen & _] (vec (:post-checks seen)))))

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

;; --- a target whose state moved before the confirmation was answered -------------

(defn- judged
  "What the book at path decides in scenario: {:result <the verdict, :none
   without a precondition> :out <what the judgement printed> :asked? <whether
   a confirmation was shown>} with :plan, :items and the :commands the run
   sent, in order. Once the operator confirmed, the verdict is the
   precondition's; a change refused before its confirmation has the verdict
   false, :asked? false and prints its reason before anything is asked."
  [path scenario]
  (binding [ssh/*user-config* nil]
    (let [spec (load-spec path)]
      (with-book-redefs
        path
        #(reach-change
          spec scenario
          (fn [out {:keys [confirmation precondition effect] :as config} state & _]
            (if (nil? config)
              {:result false :out out :before out :asked? false :plan nil :items nil
               :commands (:commands @state)}
              (do
                (when-let [between (:between scenario)]
                  (swap! state assoc :objects (:objects between)))
                (let [{:keys [result] :as verdict} (if precondition (capture precondition) {:result :none :out ""})]
                  (assoc verdict
                         :result result
                         :asked? true
                         :before out
                         :plan effect
                         :items (vec (:items confirmation))
                         :commands (:commands @state)))))))))))

(defn- refused-before-asking?
  "Whether the verdict refused the change before its confirmation, printing
   a reason matching re."
  [{:keys [result out asked?]} re]
  (and (false? result) (false? asked?) (boolean (re-find re out))))

(defn- refused-with?
  "Whether the verdict refused the change and printed a reason matching re."
  [{:keys [result out]} re]
  (and (false? result) (boolean (re-find re out))))

(defn- replaced
  "[type object] deleted and created again under the same name."
  [[type object]]
  [type (assoc-in object [:metadata :uid] (str (-> object :metadata :uid) "-again"))])

(deftest orders-recreate-refuses-an-order-that-became-valid
  (let [path "runbooks/kubernetes/orders/recreate.clj"
        start [[order-type (order "invalid")]]]
    (is (refused-with? (judged path {:objects start :target ["shop/web-1"]
                                     :between {:objects [[order-type (order "valid")]]}})
                       #"refusing: .*shop/web-1.* valid"))
    (is (refused-before-asking? (judged path {:objects [] :target ["shop/web-1"]}) #"refusing: .*shop/web-1"))
    (is (refused-before-asking? (judged path {:objects [[order-type (order "valid")]] :target ["shop/web-1"]})
                                #"refusing: .*shop/web-1.* valid"))
    (is (true? (:result (judged path {:objects start :target ["shop/web-1"]}))))))

(deftest namespaces-remove-finalizers-refuses-a-namespace-no-longer-terminating
  (let [path "runbooks/kubernetes/namespaces/remove_finalizers.clj"
        start [["namespaces" (namespace-object "Terminating")]]]
    (is (refused-with? (judged path {:objects start :target "stuck"
                                     :between {:objects [["namespaces" (namespace-object "Active")]]}})
                       #"refusing: .*stuck.* Active"))
    (is (true? (:result (judged path {:objects start :target "stuck"}))))
    (is (refused-before-asking? (judged path {:objects [["namespaces" (namespace-object "Active")]] :target "stuck"})
                                #"refusing: .*stuck.* Active"))
    (testing "a namespace no read found"
      (let [{:keys [plan] :as verdict} (judged path {:objects [] :target "stuck"})]
        (is (refused-before-asking? verdict #"refusing: .*stuck"))
        (is (not-any? #(= :kubectl-stdin (first %)) plan)
            "the plan writes nothing back, so no empty finalizer list is ever sent")))))

(deftest volumes-delete-released-refuses-a-volume-bound-again
  (let [path "runbooks/kubernetes/volumes/delete_released.clj"
        start [["persistentvolumes" (volume "Released")]]]
    (is (refused-with? (judged path {:objects start :target ["pv-1"]
                                     :between {:objects [["persistentvolumes" (volume "Bound")]]}})
                       #"refusing: .*pv-1.* Bound"))
    (is (true? (:result (judged path {:objects start :target ["pv-1"]}))))
    (is (refused-before-asking? (judged path {:objects [["persistentvolumes" (volume "Bound")]] :target ["pv-1"]})
                                #"refusing: .*pv-1.* Bound"))
    (testing "a volume no read found is refused before its confirmation lists it"
      (let [{:keys [items] :as verdict} (judged path {:objects [] :target ["pv-1"]})]
        (is (refused-before-asking? verdict #"refusing: .*pv-1"))
        (is (nil? items))))))

(deftest stageset-promote-refuses-a-stage-the-stageset-does-not-declare
  (let [path "tools/stageset/runbooks/stageset/promote.clj"
        start [[stageset-type (held-stageset "AwaitingManual")]]
        stageless [[stageset-type (-> (held-stageset "AwaitingManual") (assoc-in [:spec :stages] []) (assoc-in [:status :stages] []))]]
        no-nil? (fn [{:keys [plan]}] (not-any? #(some nil? %) plan))]
    (let [verdict (judged path {:objects start :target "apps/site" :opts {:stage "qa"}})]
      (is (refused-before-asking? verdict #"refusing: .*stage"))
      (is (no-nil? verdict) "no command is planned with a nil argument"))
    (let [verdict (judged path {:objects stageless :target "apps/site" :opts {:stage "prod"}})]
      (is (refused-before-asking? verdict #"refusing: .*apps/site.* no stages"))
      (is (no-nil? verdict)))
    (is (refused-with? (judged path {:objects start :target "apps/site" :opts {:stage "prod"}
                                     :between {:objects stageless}})
                       #"refusing: "))
    (is (true? (:result (judged path {:objects start :target "apps/site" :opts {:stage "prod"}}))))))

(deftest flux-reconcile-and-reset-refuse-a-missing-helmrelease
  (doseq [path ["tools/flux/runbooks/flux/reconcile.clj" "tools/flux/runbooks/flux/reset.clj"]]
    (testing path
      (is (refused-before-asking? (judged path {:objects [] :target "apps/podinfo"}) #"refusing: .*HelmRelease"))
      (is (refused-with? (judged path {:objects [[helmrelease-type (helmrelease "podinfo")]] :target "apps/podinfo"
                                       :between {:objects []}})
                         #"refusing: .*HelmRelease"))
      (is (true? (:result (judged path {:objects [[helmrelease-type (helmrelease "podinfo")]] :target "apps/podinfo"})))))))

(deftest volumes-delete-unused-names-the-pod-that-started-using-the-claim
  (let [path "runbooks/kubernetes/volumes/delete_unused.clj"]
    (is (refused-with? (judged path {:objects [["persistentvolumeclaims" claim]] :target "shop/data"
                                     :between {:objects [["persistentvolumeclaims" claim] ["pods" claiming-pod]]}})
                       #"refusing: .*web-0"))
    (is (refused-before-asking? (judged path {:objects [] :target "shop/data"}) #"refusing: .*shop/data"))
    (testing "a pod already using the claim refuses before the confirmation, judged over :recheck's read"
      (is (refused-before-asking? (judged path {:objects [["persistentvolumeclaims" claim] ["pods" claiming-pod]]
                                                :target "shop/data"})
                                  #"refusing: .*web-0")))))

(deftest node-steps-refuse-a-node-that-no-longer-exists-or-was-replaced
  (doseq [[path start] [["runbooks/kubernetes/nodes/cordon.clj" [["nodes" (node "n1")]]]
                        ["runbooks/kubernetes/nodes/uncordon.clj" [["nodes" (node "n1" :unschedulable true)]]]
                        ["runbooks/kubernetes/nodes/drain.clj" [["nodes" (node "n1" :unschedulable true)]
                                                                ["pods" (pod-on "n1" "api" "ReplicaSet")]]]]
          :let [scenario {:objects start :target "n1" :opts {:grace-period -1}}]]
    (testing path
      (is (true? (:result (judged path scenario))))
      (is (refused-with? (judged path (assoc scenario :between {:objects []})) #"refusing: .*n1"))
      (is (refused-with? (judged path (assoc scenario :between {:objects (mapv replaced start)}))
                         #"refusing: .*(deleted|created)")))))

(deftest node-books-pin-the-spec-of-their-node
  ;; a Node has no generation and the kubelet writes its status every few
  ;; seconds, so a prompt left open while it does must not refuse the change
  (doseq [[path target start ssh] [["runbooks/kubernetes/nodes/cordon.clj" "n1" [["nodes" (node "n1")]] nil]
                                   ["runbooks/kubernetes/nodes/uncordon.clj" "n1" [["nodes" (node "n1" :unschedulable true)]] nil]
                                   ["runbooks/kubernetes/nodes/drain.clj" "n1" [["nodes" (node "n1" :unschedulable true)]
                                                                                ["pods" (pod-on "n1" "api" "ReplicaSet")]] nil]
                                   ["runbooks/kubernetes/nodes/restart_apiserver.clj" "cp-1"
                                    [["nodes" (node "cp-1" :control-plane? true)]] (apiserver-host "ok")]]
          :let [scenario {:objects start :target target :opts {:grace-period -1} :ssh ssh}
                moved (fn [f] (into [(f (first start))] (rest start)))]]
    (testing path
      (is (true? (:result (judged path (assoc scenario :between {:objects (moved heartbeat)}))))
          "a status write during the confirmation")
      (is (refused-with? (judged path (assoc scenario :between {:objects (moved tainted)}))
                         #"refusing: its spec changed after the confirmation: taints")
          "a taint added during the confirmation"))))

(deftest restart-apiserver-never-reaches-a-node-the-cluster-no-longer-lists
  (let [path "runbooks/kubernetes/nodes/restart_apiserver.clj"
        start [["nodes" (node "cp-1" :control-plane? true)]]
        ssh? (fn [{:keys [commands]}] (some #(= "ssh" (first %)) commands))]
    (testing "a node absent before the confirmation is never connected to"
      (let [verdict (judged path {:objects [] :target "cp-1" :ssh (apiserver-host "ok")})]
        (is (refused-before-asking? verdict #"refusing: .*cp-1"))
        (is (not (ssh? verdict)) (pr-str (:commands verdict)))))
    (testing "a node removed after the confirmation is refused before any SSH connection is checked"
      (let [verdict (judged path {:objects start :target "cp-1" :ssh (apiserver-host "ok") :between {:objects []}})]
        (is (refused-with? verdict #"refusing: .*cp-1"))
        (is (not (re-find #"can connect" (:out verdict))))))
    (testing "a node replaced after the confirmation"
      (is (refused-with? (judged path {:objects start :target "cp-1" :ssh (apiserver-host "ok")
                                       :between {:objects (mapv replaced start)}})
                         #"refusing: .*(deleted|created)")))
    (testing "a node that is not a control-plane node"
      (is (refused-with? (judged path {:objects [["nodes" (node "cp-1")]] :target "cp-1" :ssh (apiserver-host "ok")})
                         #"refusing: .*control-plane")))
    (is (true? (:result (judged path {:objects start :target "cp-1" :ssh (apiserver-host "ok")}))))))

(deftest every-destructive-book-refuses-a-target-that-does-not-exist
  ;; the behaviour behind conformance/guard-problems: a guard is declared, and
  ;; it holds against a cluster where the confirmed target is gone
  (binding [ssh/*user-config* nil]
    (doseq [[path scenarios] books
            :let [spec (load-spec path)]
            :when (get-in spec [:confirm :destructive?])]
      (testing path
        (let [{:keys [precondition stopped] :as t} (with-book-redefs path #(transcript spec (:absent scenarios)))]
          (is (or (some? stopped) (false? (:result precondition))) (pr-str t)))))))

;; --- already done ---------------------------------------------------------------

(def ^:private skipping
  "The scenario in which each book that declares :done finds it done; in
   every other scenario it is judged not done."
  {"runbooks/kubernetes/cronjobs/suspend.clj" :refused
   "runbooks/kubernetes/cronjobs/resume.clj" :refused
   "runbooks/kubernetes/workloads/pause.clj" :refused
   "runbooks/kubernetes/workloads/resume.clj" :refused
   "runbooks/kubernetes/workloads/scale.clj" :done
   "runbooks/kubernetes/nodes/cordon.clj" :refused
   "runbooks/kubernetes/nodes/drain.clj" :done
   "runbooks/kubernetes/nodes/uncordon.clj" :refused
   "tools/flux/runbooks/flux/suspend.clj" :refused
   "tools/flux/runbooks/flux/resume.clj" :refused
   "tools/cnpg/runbooks/suspend_scheduled_backup.clj" :refused
   "tools/cnpg/runbooks/resume_scheduled_backup.clj" :refused
   "tools/cnpg/runbooks/fence.clj" :refused
   "tools/cnpg/runbooks/unfence.clj" :done
   "tools/velero/runbooks/pause_schedule.clj" :refused
   "tools/velero/runbooks/resume_schedule.clj" :refused})

(defn- done-book?
  [path]
  (some? (change/done (change/normalize (load-spec path)))))

(deftest the-books-that-declare-done-are-the-ones-listed
  (is (= (set (keys skipping)) (set (filter done-book? (keys books))))))

(deftest a-done-book-skips-exactly-where-its-post-checks-already-hold
  (binding [ssh/*user-config* nil]
    (doseq [[path skips-in] skipping
            :let [spec (load-spec path)
                  scenarios (get books path)
                  checks (with-book-redefs path #(post-checks-of spec scenarios))]
            [k scenario] scenarios]
      (testing (str path " " k)
        (let [{:keys [stopped]} (with-book-redefs path #(transcript spec scenario))
              skipped? (boolean (some-> stopped (str/includes? "skipped, already done")))]
          (is (= (= k skips-in) skipped?) (str stopped))
          (when skipped?
            (is (= (concat (map #(str "✅ already holds: " (:description %)) checks)
                           [(str "✅ skipped, already done: " (get-in spec [:confirm :action]))])
                   (map stdout/strip-colors (str/split-lines stopped)))
                "every post-check of the permitted scenario held, and the skip names the step")))))))

(deftest a-done-book-never-skips-on-a-cluster-that-does-not-answer
  (binding [ssh/*user-config* nil]
    (doseq [[path skips-in] skipping
            :let [spec (load-spec path)
                  scenario (assoc (get-in books [path skips-in]) :answering? false)]]
      (testing path
        (let [{:keys [result out asked?]} (judged path scenario)]
          (is (false? result))
          (is (false? asked?))
          (is (not (str/includes? out "skipped")) out))))))

;; --- a named target the candidates leave out ------------------------------------

(defn- opts-naming
  "The flags that name `target` in the book's select, over the scenario's
   own opts."
  [{:keys [mode flag namespace-flag name-flag]} target opts]
  (merge opts
         (case mode
           :many {flag (if (string? target) [target] (vec target))}
           :namespaced (let [[namespace name] (str/split target #"/" 2)]
                         {namespace-flag namespace name-flag name})
           {flag target})))

(defn- left-out?
  "Whether the select of spec, over objects, leaves out what opts name."
  [spec objects opts]
  (let [select (:select spec)]
    (with-redefs [shell/execute (fn [& argv] (answer {:objects objects :answering? true} (vec argv)))]
      (not (select/offered? select (select/named-target select opts)
                            (:names (select/listed select "ctx")))))))

(deftest a-named-target-the-candidates-leave-out-is-skipped-only-when-done
  (binding [ssh/*user-config* nil]
    (doseq [[path scenarios] (sort-by key books)
            :let [spec (change/normalize (load-spec path))
                  {:keys [objects target opts ssh]} (:refused scenarios)
                  done? (= :refused (get skipping path))
                  named (opts-naming (:select spec) target opts)]
            :when (and (if (string? target) (not (str/blank? target)) (seq target))
                       (with-book-redefs path #(left-out? spec objects named)))]
      (testing path
        (let [{:keys [result out action]} (with-book-redefs path #(pick-transcript spec {:objects objects :opts named :ssh ssh}))]
          (is (nil? action) "a target the candidates leave out never reaches a confirmation")
          (is (not (str/includes? out "please select")) out)
          (if done?
            (do (is (true? result) out)
                (is (str/includes? out "skipped, already done") out))
            (is (false? result) out))
          (when (and (change/pick-subject spec) (not= :many (get-in spec [:select :mode])))
            (is (not (str/includes? out "has no typed subject")) (str "a book with a :subject reads the named object\n" out))))))))
