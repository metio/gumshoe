;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.playbook-books-test
  "Characterises the playbooks end to end against a recorded cluster that
   applies what the run writes: node maintenance, clear-helm-storage, and a
   casebook-style extension of clear-helm-storage built with playbook-with.

   Each scenario runs the whole playbook, its selection included, with the
   real flow/change! and effect interpreter. gumshoe.shell answers kubectl
   reads from the recorded objects and applies kubectl writes to them: patch,
   annotate, delete, cordon, uncordon and drain. A scenario's :react plays the
   controllers, such as helm-controller installing once the HelmRelease
   resumes. A frozen cluster accepts every write and changes nothing, so a
   post-check does not hold; :fail names the write that fails.

   The operator is recorded: every confirmation the run asks for, with its
   items, answered by the scenario's :answers in turn; the fuzzy finder picks
   nothing; a pause waits at a terminal when the scenario is :attended?.
   Post-checks run once, with a timeout of 0.

   A transcript keeps the run's result, what it printed, the confirmations,
   the writes it made in order, its banner and the recovery lines under it.
   The transcripts are kept in tests/golden/playbook_books.edn, so a
   refactoring of a playbook, its steps or the engine keeps every one of them.

   Regenerate the file after an intended change with
   (gumshoe.playbook-books-test/write-golden!) and review its diff."
  (:require [babashka.cli :as cli]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [gumshoe.announce :as announce]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.streams-support :as streams]
            [gumshoe.fuzzy-finder :as fuzzy]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.outcome :as outcome]
            [gumshoe.playbook :as playbook]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]
            [gumshoe.steps.objects :as objects]
            [gumshoe.stdout :as stdout]
            [gumshoe.tools.flux-storage :as storage]
            [gumshoe.verify :as verify]))

(def golden-path "tests/golden/playbook_books.edn")

;; --- the recorded cluster ----------------------------------------------------------

(def ^:private unanswered
  {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 10.0.0.1:6443: i/o timeout"})

(defn- arg-value
  [argv prefix]
  (some #(when (and (string? %) (str/starts-with? % prefix)) (subs % (count prefix))) argv))

(defn- namespace-of-argv
  "The namespace argv names, as --namespace=x, -n x or --namespace x."
  [argv]
  (or (arg-value argv "--namespace=")
      (second (drop-while #(not (#{"-n" "--namespace"} %)) argv))))

(defn- positional
  "argv without kubectl, its flags and the values of -n and --namespace."
  [argv]
  (loop [[a & more] (rest argv) acc []]
    (cond
      (nil? a) acc
      (#{"-n" "--namespace" "--patch"} a) (recur (rest more) acc)
      (str/starts-with? a "-") (recur more acc)
      :else (recur more (conj acc a)))))

(defn- labels-match?
  [selector object]
  (every? (fn [term]
            (let [[k v] (str/split term #"=" 2)]
              (= v (get-in object [:metadata :labels (keyword k)]))))
          (when selector (str/split selector #","))))

(defn- matcher
  "Whether [type object] is one argv selects: its type, namespace, name, label
   selector and spec.nodeName field selector."
  [argv type name]
  (let [namespace (namespace-of-argv argv)
        selector (arg-value argv "--selector=")
        node (arg-value argv "--field-selector=spec.nodeName=")]
    (fn [[t o]]
      (and (= t type)
           (or (nil? namespace) (= namespace (-> o :metadata :namespace)))
           (or (nil? name) (= name (-> o :metadata :name)))
           (labels-match? selector o)
           (or (nil? node) (= node (-> o :spec :nodeName)))))))

(defn- helm-listing
  "The lines kubectl prints for helm-releases' jsonpath over the Secrets argv
   selects."
  [objects matches]
  (str/join "\n" (for [[_ o :as entry] objects
                       :when (matches entry)
                       :let [{:keys [namespace labels creationTimestamp]} (:metadata o)]]
                   (str/join "\t" [namespace (:name labels) (:status labels) (:version labels)
                                   (:modifiedAt labels) (:createdAt labels) creationTimestamp]))))

(defn- read-answer
  [{:keys [objects]} argv]
  (let [[_ type name] (drop-while #(not= "get" %) argv)
        name (when (and name (not (str/starts-with? name "-"))) name)
        matches (matcher argv type name)
        found (filter matches objects)]
    (cond
      (arg-value argv "--output=jsonpath=") {:exit 0 :out (helm-listing objects matches) :err ""}
      (some #{"--output=name"} argv) {:exit 0 :out (str/join "\n" (for [[t o] found] (str t "/" (-> o :metadata :name)))) :err ""}
      name (if-let [[_ o] (first found)]
             {:exit 0 :out (json/generate-string o) :err ""}
             {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" type name)})
      :else {:exit 0 :out (json/generate-string {:items (map second found)}) :err ""})))

(defn- deep-merge
  "A JSON merge patch: maps merge, a nil value removes the key."
  [a b]
  (if (and (map? a) (map? b))
    (reduce-kv (fn [acc k v] (if (nil? v) (dissoc acc k) (assoc acc k (deep-merge (get acc k) v)))) a b)
    b))

(defn- update-object
  [objects type namespace name f]
  (mapv (fn [[t o :as entry]]
          (if (and (= t type) (= name (-> o :metadata :name))
                   (or (nil? namespace) (= namespace (-> o :metadata :namespace))))
            [t (f o)]
            entry))
        objects))

(defn- daemonset-pod?
  [o]
  (some #(= "DaemonSet" (:kind %)) (-> o :metadata :ownerReferences)))

(defn- apply-write
  "objects after the kubectl write argv."
  [objects argv]
  (let [namespace (namespace-of-argv argv)
        [verb type name & more] (positional argv)]
    (case verb
      "patch" (let [patch (or (arg-value argv "--patch=")
                              (second (drop-while #(not= "--patch" %) argv)))]
                (update-object objects type namespace name #(deep-merge % (json/parse-string patch true))))
      "annotate" (update-object objects type namespace name
                                (fn [o] (reduce (fn [o kv]
                                                  (let [[k v] (str/split kv #"=" 2)]
                                                    (assoc-in o [:metadata :annotations (keyword k)] v)))
                                                o (filter #(str/includes? % "=") more))))
      "delete" (vec (remove (matcher argv type name) objects))
      "cordon" (update-object objects "nodes" nil type #(assoc-in % [:spec :unschedulable] true))
      "uncordon" (update-object objects "nodes" nil type #(update % :spec dissoc :unschedulable))
      "drain" (->> (update-object objects "nodes" nil type #(assoc-in % [:spec :unschedulable] true))
                   (remove (fn [[t o]] (and (= "pods" t) (= type (-> o :spec :nodeName)) (not (daemonset-pod? o)))))
                   vec)
      objects)))

(defn- write!
  "Applies the write argv to the cluster unless it is frozen, lets the
   scenario's controllers react, and answers with the exit code; the write
   the scenario's :fail matches fails and changes nothing."
  [cluster argv]
  (let [{:keys [frozen? fail react]} @cluster
        argv (vec (remove nil? argv))]
    (swap! cluster update :writes conj (str/join " " (drop 2 argv)))
    (if (and fail (fail argv))
      1
      (do (when-not frozen?
            (swap! cluster update :objects apply-write argv)
            (when react (swap! cluster update :objects react argv)))
          0))))

(defn- execute
  [cluster & argv]
  (let [argv (vec argv)
        {:keys [answering?]} @cluster]
    (cond
      (not answering?) unanswered
      (some #{"get"} argv) (read-answer @cluster argv)
      ;; kubectl auth can-i asks for rights and writes nothing: yes, unless
      ;; the scenario's :denied matches
      (some #{"auth"} argv) {:exit (if (some-> (:denied @cluster) (apply [argv])) 1 0) :out "" :err ""}
      :else {:exit (write! cluster argv) :out "" :err ""})))

;; --- running a playbook --------------------------------------------------------------

(defn- normalise
  "x with every instant and every yyyyMMddHHmmss stamp replaced, and every
   function replaced by :fn, so a transcript does not depend on the clock."
  [x]
  (edn/read-string (-> (pr-str (walk/postwalk #(if (fn? %) :fn %) x))
                       (str/replace #"\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d+)?Z" "<instant>")
                       (str/replace #"\d{14}" "<stamp>"))))

(defn- printed
  [out]
  (vec (remove str/blank? (str/split-lines (stdout/strip-colors (str out))))))

(defn- book-config
  "The runbook config playbook/book builds from spec: its options, --help
   details and action."
  [spec]
  (binding [runbook/*loading* true]
    (playbook/book spec)))

(defn transcript
  "What the playbook does in one scenario: argv its command line, parsed by
   the options the book declares, :objects the cluster, :answers what the
   operator answers to each confirmation in turn (yes when they run out),
   :denied a predicate over the argv of a kubectl auth can-i it answers no."
  [spec {:keys [objects argv answers attended? answering? frozen? fail react dry-run? denied]
         :or {answering? true}}]
  (let [{:keys [options action]} (book-config spec)
        opts (cli/parse-opts argv {:spec options})
        cluster (atom {:objects objects :answering? answering? :frozen? frozen? :fail fail :react react :writes []
                       :denied denied})
        asked (atom [])
        answers (atom answers)
        waited (atom [])
        record (outcome/fresh)
        eventually verify/eventually
        out (java.io.StringWriter.)
        err (java.io.StringWriter.)
        result (with-redefs-fn
                 {#'gumshoe.playbook/attended? (constantly (boolean attended?))
                  #'gumshoe.playbook/wait-for-operator! (fn [what] (swap! waited conj what) true)}
                 #(with-redefs [shell/execute (partial execute cluster)
                                shell/execute-with-stdin (fn [_ & argv] (apply execute cluster argv))
                                shell/run-with-output (fn [& argv] (write! cluster argv))
                                shell/run-attributed (fn [& argv] (write! cluster argv))
                                kubectl/current-context (constantly "ctx")
                                kubectl/current-cluster (constantly "cluster")
                                fuzzy/select-single (constantly nil)
                                fuzzy/select-multi (constantly nil)
                                interact/confirm! (fn [{:keys [action items destructive?]}]
                                                    (swap! asked conj {:action action :destructive? (boolean destructive?)
                                                                       :items (vec items)})
                                                    (let [answer (if (seq @answers) (first @answers) true)]
                                                      (swap! answers rest)
                                                      answer))
                                announce/announce! (fn [_ _ message] message)
                                verify/eventually (fn [check] (eventually (assoc check :timeout 0)))]
                    (binding [*out* out *err* err
                              outcome/*record* record
                              flow/*dry-run* (boolean dry-run?)]
                      (try (action opts {:announcement-data nil})
                           (catch Exception e (binding [*out* *err*] (println "threw:" (ex-message e))) :threw)))))
        {:keys [tone text]} (outcome/banner {:changes? true :outcome (if (true? result) :ok :failed)} @record)]
    (normalise
     (cond-> {:result result
              :out (printed err)
              :confirmations @asked
              :writes (:writes @cluster)
              :banner text
              :recovery (vec (outcome/recovery-lines tone @record))}
       (seq @waited) (assoc :waited @waited)
       (seq (printed out)) (assoc :stdout (printed out))))))

(defn surface
  "What the playbook declares outside a run: its options, prerequisites,
   selection, confirmation and --help text."
  [spec]
  (let [{:keys [description options prerequisites details]} (book-config spec)]
    (normalise
     {:description description
      :options options
      :prerequisites prerequisites
      :select (dissoc (:select spec) :candidates)
      :confirm (:confirm spec)
      :details (vec details)})))

;; --- node maintenance -----------------------------------------------------------------

(defn- node
  [name & {:keys [unschedulable ready] :or {ready "True"}}]
  {:metadata {:name name :uid (str "uid-" name) :labels {:kubernetes.io/hostname name}}
   :spec (cond-> {} unschedulable (assoc :unschedulable true))
   :status {:conditions [{:type "Ready" :status ready :reason "KubeletReady"}]
            :nodeInfo {:kubeletVersion "v1.31.2"}}})

(defn- pod
  [node-name name owner-kind & {:keys [labels]}]
  {:metadata {:namespace "shop" :name name :uid (str "uid-" name) :labels (or labels {})
              :ownerReferences [{:kind owner-kind :name (str name "-owner")}]}
   :spec {:nodeName node-name}
   :status {:phase "Running"}})

(def ^:private budget
  {:metadata {:namespace "shop" :name "web" :uid "uid-pdb-web"}
   :spec {:minAvailable 1 :selector {:matchLabels {:app "web"}}}
   :status {:currentHealthy 1 :desiredHealthy 1 :disruptionsAllowed 0 :expectedPods 1}})

(def ^:private node-cluster
  [["nodes" (node "n1")] ["nodes" (node "n2")]
   ["pods" (pod "n1" "web-1" "ReplicaSet" :labels {:app "web"})]
   ["pods" (pod "n1" "agent-1" "DaemonSet")]
   ["pods" (pod "n2" "web-2" "ReplicaSet" :labels {:app "web"})]
   ["poddisruptionbudgets" budget]])

(def ^:private drained-cluster
  "node-cluster once n1 is cordoned and drained: web-1 runs on n2 now, and
   only the DaemonSet's pod, which a drain leaves, is on n1."
  [["nodes" (node "n1" :unschedulable true)] ["nodes" (node "n2")]
   ["pods" (pod "n1" "agent-1" "DaemonSet")]
   ["pods" (pod "n2" "web-2" "ReplicaSet" :labels {:app "web"})]
   ["pods" (pod "n2" "web-1" "ReplicaSet" :labels {:app "web"})]
   ["poddisruptionbudgets" budget]])

(defn- writes-matching
  [re]
  (fn [argv] (boolean (re-find re (str/join " " argv)))))

(def ^:private node-scenarios
  {:unattended {:objects node-cluster :argv ["--node" "n1"]}
   :attended {:objects node-cluster :argv ["--node" "n1"] :attended? true}
   :grace-period {:objects node-cluster :argv ["-n" "n1" "--grace-period" "30"] :attended? true}
   :from-segment-2 {:objects (update-object node-cluster "nodes" nil "n1" #(assoc-in % [:spec :unschedulable] true))
                    :argv ["--node" "n1" "--from-segment" "2"]}
   :from-segment-9 {:objects node-cluster :argv ["--node" "n1" "--from-segment" "9"]}
   :declined {:objects node-cluster :argv ["--node" "n1"] :answers [false]}
   :dry-run {:objects node-cluster :argv ["--node" "n1"] :dry-run? true}
   :unpicked {:objects node-cluster :argv ["--node" "n9"]}
   :empty {:objects [] :argv ["--node" "n1"]}
   :unanswered {:objects node-cluster :argv ["--node" "n1"] :answering? false}
   :failing-drain {:objects node-cluster :argv ["--node" "n1"] :attended? true :fail (writes-matching #"drain n1")}
   :frozen {:objects node-cluster :argv ["--node" "n1"] :attended? true :frozen? true}
   :unhealthy-after {:objects (update-object node-cluster "nodes" nil "n2" #(assoc-in % [:status :conditions 0 :status] "False"))
                     :argv ["--node" "n1"] :attended? true}
   ;; a run stopped after the cordon, started again from the top
   :resumed-after-cordon {:objects (update-object node-cluster "nodes" nil "n1" #(assoc-in % [:spec :unschedulable] true))
                          :argv ["--node" "n1"] :attended? true}
   ;; a run stopped at the pause, started again from the top: cordon and
   ;; drain are done, the pause waits, and the uncordon runs
   :resumed-after-drain {:objects drained-cluster :argv ["--node" "n1"] :attended? true}
   :resumed-after-drain-unattended {:objects drained-cluster :argv ["--node" "n1"]}
   :resumed-after-drain-dry-run {:objects drained-cluster :argv ["--node" "n1"] :dry-run? true}})

;; --- clear-helm-storage ---------------------------------------------------------------

(def ^:private helmrelease-type "helmreleases.helm.toolkit.fluxcd.io")
(def ^:private kustomization-type "kustomizations.kustomize.toolkit.fluxcd.io")
(def ^:private resourceset-type "resourcesets.fluxcd.controlplane.io")

(defn- condition [status reason] {:conditions [{:type "Ready" :status status :reason reason :message reason}]})

(def ^:private release
  {:metadata {:namespace "apps" :name "podinfo" :uid "uid-podinfo" :generation 3}
   :spec {:interval "10m" :timeout "5m" :releaseName "podinfo" :chart {:spec {:chart "podinfo"}}}
   :status (condition "False" "UpgradeFailed")})

(defn- owner
  [kind]
  {:metadata (cond-> {:namespace "flux-system" :name "apps" :uid "uid-apps"}
               (= "ResourceSet" kind) (assoc :annotations {:fluxcd.controlplane.io/reconcileEvery "30m"}))
   :spec {:interval "10m"}
   :status (merge (condition "True" "ReconciliationSucceeded")
                  {:inventory {:entries [{:id "apps_podinfo_helm.toolkit.fluxcd.io_HelmRelease" :v "v2"}]}})})

(defn- stamped
  [object]
  (update object :metadata merge {:annotations {:meta.helm.sh/release-name "podinfo"
                                                 :meta.helm.sh/release-namespace "apps"}}))

(def ^:private now-seconds (quot (System/currentTimeMillis) 1000))

(defn- revision
  "Helm's Secret for revision `version` of apps/podinfo, last changed at
   epoch second `at`."
  [version status at]
  {:metadata {:namespace "apps" :name (str "sh.helm.release.v1.podinfo.v" version)
              :uid (str "uid-podinfo-v" version) :creationTimestamp "2026-01-01T00:00:00Z"
              :labels {:owner "helm" :name "podinfo" :status status :version (str version)
                       :modifiedAt (str at) :createdAt (str at)}}
   :type "helm.sh/release.v1"})

(def ^:private parked-at 1767225600)

(defn- release-cluster
  "apps/podinfo parked in pending-upgrade, rendered by the Kustomization
   flux-system/apps or by `owner`, with a Deployment, a StatefulSet whose
   template made a claim, a stamped claim, and an unrelated Deployment."
  ([] (release-cluster [kustomization-type (owner "Kustomization")]))
  ([owner-entry]
   [[helmrelease-type release]
    owner-entry
    ["deployments" (stamped {:metadata {:namespace "apps" :name "podinfo" :uid "uid-deploy-podinfo"}})]
    ["deployments" {:metadata {:namespace "apps" :name "other" :uid "uid-deploy-other"}}]
    ["statefulsets" (stamped {:metadata {:namespace "apps" :name "podinfo-db" :uid "uid-sts-podinfo-db"}
                              :spec {:volumeClaimTemplates [{:metadata {:name "data"}}]}})]
    ["persistentvolumeclaims" {:metadata {:namespace "apps" :name "data-podinfo-db-0" :uid "uid-pvc-data"}}]
    ["persistentvolumeclaims" (stamped {:metadata {:namespace "apps" :name "podinfo-cache" :uid "uid-pvc-cache"}})]
    ["persistentvolumeclaims" {:metadata {:namespace "apps" :name "unrelated" :uid "uid-pvc-unrelated"}}]
    ["secrets" (revision 1 "superseded" parked-at)]
    ["secrets" (revision 2 "pending-upgrade" parked-at)]]))

(def ^:private requested-at "reconcile.fluxcd.io/requestedAt=")

(defn- handles-requests
  "Flux controllers acting on a reconcile request: an annotate stamping
   requestedAt on an object that is not suspended writes the token to its
   status.lastHandledReconcileAt."
  [objects argv]
  (let [[verb type name] (positional argv)
        token (some #(when (and (string? %) (str/starts-with? % requested-at)) (subs % (count requested-at))) argv)]
    (if (and (= "annotate" verb) token)
      (update-object objects type (namespace-of-argv argv) name
                     (fn [o] (if (or (true? (-> o :spec :suspend))
                                     (= "disabled" (get-in o [:metadata :annotations :fluxcd.controlplane.io/reconcile])))
                               o
                               (assoc-in o [:status :lastHandledReconcileAt] token))))
      objects)))

(defn- helm-controller
  "Installs apps/podinfo afresh once its HelmRelease is resumed: revision 1,
   deployed, and Ready. Every Flux controller handles the reconcile requests
   stamped on its objects."
  [objects argv]
  (handles-requests
   (if (and (some #{"patch"} argv) (some #{helmrelease-type} argv)
            (some #(and (string? %) (str/includes? % "\"suspend\":false")) argv))
     (-> (update-object objects helmrelease-type "apps" "podinfo" #(update % :status merge (condition "True" "InstallSucceeded")))
         (conj ["secrets" (revision 1 "deployed" now-seconds)]))
     objects)
   argv))

(def ^:private storage-scenarios
  {:cleared {:objects (release-cluster) :argv ["--namespace" "apps" "--name" "podinfo"] :react helm-controller}
   :resourceset-owner {:objects (release-cluster [resourceset-type (owner "ResourceSet")])
                       :argv ["--namespace" "apps" "--name" "podinfo"] :react helm-controller}
   :no-owner {:objects (vec (remove #(= kustomization-type (first %)) (release-cluster)))
              :argv ["--namespace" "apps" "--name" "podinfo"] :react helm-controller}
   :two-owners {:objects (conj (release-cluster) [resourceset-type (owner "ResourceSet")])
                :argv ["--namespace" "apps" "--name" "podinfo"]}
   :in-flight {:objects (conj (vec (remove #(= "sh.helm.release.v1.podinfo.v2" (-> % second :metadata :name)) (release-cluster)))
                              ["secrets" (revision 2 "pending-upgrade" now-seconds)])
               :argv ["--namespace" "apps" "--name" "podinfo"]}
   :declined {:objects (release-cluster) :argv ["--namespace" "apps" "--name" "podinfo"] :answers [false]}
   :volumes-declined {:objects (release-cluster) :argv ["--namespace" "apps" "--name" "podinfo"] :answers [true false]}
   :dry-run {:objects (release-cluster) :argv ["--namespace" "apps" "--name" "podinfo"] :dry-run? true}
   :unpicked {:objects (release-cluster) :argv ["--namespace" "apps" "--name" "nope"]}
   :empty {:objects [] :argv ["--namespace" "apps" "--name" "podinfo"]}
   :unanswered {:objects (release-cluster) :argv ["--namespace" "apps" "--name" "podinfo"] :answering? false}
   :failing-storage-delete {:objects (release-cluster) :argv ["--namespace" "apps" "--name" "podinfo"]
                            :fail (writes-matching #"delete secrets")}
   :frozen {:objects (release-cluster) :argv ["--namespace" "apps" "--name" "podinfo"] :frozen? true}
   :no-install {:objects (release-cluster) :argv ["--namespace" "apps" "--name" "podinfo"]}})

;; A casebook extends the playbook by position, as infra.run's moodle reset
;; does: its own select and confirmation, a step that deletes objects Helm
;; never stamped after the volumes, and a closing note after the resume.

(def ^:private database-type "databases.kinda.rocks")

(def ^:private casebook-db-step
  (merge {:title "🗑  Delete the db-operator objects Helm never stamped"
          :prerequisites {:kubectl-can-delete [database-type "secrets"]}
          :confirm {:action "delete the Database and the credentials db-operator rendered from it" :destructive? true}
          :recovery "Some of the Database and its Secret may be gone and the rest not."}
         (objects/delete {:objects (fn [{:keys [target]}]
                                     (let [{:keys [namespace]} (kubectl/namespace-name-map target)]
                                       [{:type database-type :namespace namespace :name "podinfo"}
                                        {:type "secrets" :namespace namespace :name "podinfo-database"}]))
                          :none "no database"
                          :timeout 120})))

(def ^:private casebook-note-step
  {:title "📋 What to do once the install has written the credentials"
   :confirm {:action "print the follow-up the install leaves to a human"}
   :items (fn [_] ["print what to restart once the new credentials exist"])
   :effect (fn [_] (effect/plan (effect/note "restart the poolers once the new credentials exist")))})

(defn- casebook-spec
  []
  (merge (storage/playbook-with {:after-volumes [casebook-db-step] :after-resume [casebook-note-step]})
         {:description "Resets a podinfo instance to a clean install"
          :confirm {:action "suspend its owner, delete this instance's workloads, volumes, database and Helm storage, then resume it"
                    :destructive? true}}))

(def ^:private casebook-cluster
  (conj (release-cluster)
        [database-type {:metadata {:namespace "apps" :name "podinfo" :uid "uid-db"}}]
        ["secrets" {:metadata {:namespace "apps" :name "podinfo-database" :uid "uid-db-secret"}}]))

(def ^:private casebook-scenarios
  {:cleared {:objects casebook-cluster :argv ["--namespace" "apps" "--name" "podinfo"] :react helm-controller}
   :failing-db-delete {:objects casebook-cluster :argv ["--namespace" "apps" "--name" "podinfo"]
                       :fail (writes-matching #"delete databases.kinda.rocks")}})

;; --- the playbooks ---------------------------------------------------------------------

(defn- load-book
  "The spec a playbook file hands to playbook/book."
  [path]
  (with-redefs [playbook/book identity]
    (load-file path)))

(def playbooks
  "Each playbook by name: how to build a fresh spec, and its scenarios. The
   flux-storage steps carry one run's observations between steps, so each
   scenario builds its steps afresh, as each process running the book does."
  {"playbooks/kubernetes/node_maintenance.clj"
   {:spec #(load-book "playbooks/kubernetes/node_maintenance.clj")
    :scenarios node-scenarios}

   "tools/flux/runbooks/flux/clear_helm_storage.clj"
   {:spec #(assoc (load-book "tools/flux/runbooks/flux/clear_helm_storage.clj") :steps (storage/steps))
    :scenarios storage-scenarios}

   "casebook: clear_helm_storage playbook-with"
   {:spec casebook-spec
    :scenarios casebook-scenarios}})

(defn transcripts
  []
  (into (sorted-map)
        (for [[name {:keys [spec scenarios]}] playbooks]
          [name (assoc (into (sorted-map) (for [[k scenario] scenarios] [k (transcript (spec) scenario)]))
                       :surface (surface (spec)))])))

(defn write-golden!
  []
  (spit golden-path
        ;; split, so a REUSE scan does not read these strings as this file's header
        (str ";; SPDX" "-FileCopyrightText: The gumshoe Authors\n"
             ";; SPDX" "-License-Identifier: 0BSD\n"
             ";; Generated by (gumshoe.playbook-books-test/write-golden!); review its diff.\n"
             (with-out-str (pprint/pprint (transcripts))))))

(deftest every-playbook-keeps-its-transcripts
  (let [golden (edn/read-string (slurp golden-path))
        actual (transcripts)]
    (is (= (set (keys golden)) (set (keys actual))) "the same playbooks")
    (doseq [[name scenarios] actual
            [scenario transcript] scenarios]
      (testing (str name " " scenario)
        (is (= (get-in golden [name scenario]) transcript))))))

(deftest the-clear-helm-storage-book-runs-the-flux-storage-steps
  (let [book (load-book "tools/flux/runbooks/flux/clear_helm_storage.clj")]
    (is (= (map :title (:steps book)) (map :title (storage/steps)))
        "the harness's fresh steps are the book's")))
