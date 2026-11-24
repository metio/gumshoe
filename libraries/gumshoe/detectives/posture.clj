;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.posture
  "Detectives for risks that are not failures yet: a workload that runs fine
   today and breaks on the next node drain, image push or upgrade. They form
   their own scope, left out of whole-cluster health scans, because every
   finding here is a choice someone may have made on purpose."
  (:require [clojure.string :as str]
            [gumshoe.conditions :as conditions]
            [gumshoe.detectives.pod-security :as pod-security]
            [gumshoe.kubectl :as kubectl]))

(def ^:private replicated-types
  [["deployments" "Deployment"] ["statefulsets" "StatefulSet"]])

(def ^:private template-types
  (conj replicated-types ["daemonsets" "DaemonSet"]))

(defn- templates
  "Each workload of the given types with its kind and pod template."
  [evidence types]
  (for [[type kind] types
        workload (kubectl/items-of (get evidence type))]
    {:kind kind :workload workload :template (-> workload :spec :template)}))

(defn detect-missing-readiness-probes
  [evidence]
  (for [{:keys [kind workload template]} (templates evidence template-types)
        container (-> template :spec :containers)
        :when (and (seq (:ports container)) (nil? (:readinessProbe container)))]
    {:severity :info
     :component (kubectl/namespace-name-of workload)
     :summary (format "%s container %s serves ports without a readiness probe" kind (:name container))
     :hint "a Service sends traffic to it as soon as it starts, before it can answer - add a readinessProbe"}))

(defn mutable-tag
  "The tag that makes an image reference mutable - latest, or none at all -
   or nil for a pinned tag or a digest."
  [image]
  (let [image (str image)]
    (when-not (str/includes? image "@")
      (let [last-part (last (str/split image #"/"))
            tag (second (str/split last-part #":" 2))]
        (cond
          (nil? tag) "no tag"
          (= "latest" tag) "latest"
          :else nil)))))

(defn- pod-specs
  "Each workload's pod spec, CronJobs included."
  [evidence]
  (concat
   (for [{:keys [kind workload template]} (templates evidence template-types)]
     [kind workload (:spec template)])
   (for [cronjob (kubectl/items-of (get evidence "cronjobs"))]
     ["CronJob" cronjob (-> cronjob :spec :jobTemplate :spec :template :spec)])))

(defn detect-mutable-images
  [evidence]
  (for [[kind workload spec] (pod-specs evidence)
        container (concat (:initContainers spec) (:containers spec))
        :let [tag (mutable-tag (:image container))]
        :when (and (:image container) tag)]
    {:severity :warning
     :component (kubectl/namespace-name-of workload)
     :summary (format "%s container %s uses a mutable image tag (%s)" kind (:name container) tag)
     :hint (format "%s can change under the same name, so two nodes may run different code and a restart may pull a new version - pin a version tag or a digest"
                   (:image container))}))

(defn- hpa-floor
  "The lowest replica count an HPA keeps each workload at, by kind/namespace/name."
  [evidence]
  (into {} (for [hpa (kubectl/items-of (get evidence "horizontalpodautoscalers"))
                 :let [target (-> hpa :spec :scaleTargetRef)]]
             [[(:kind target) (kubectl/namespace-of hpa) (:name target)] (or (-> hpa :spec :minReplicas) 1)])))

(defn- replicas
  [workload]
  (or (-> workload :spec :replicas) 1))

(defn detect-single-replicas
  [evidence]
  (let [floors (hpa-floor evidence)]
    (for [{:keys [kind workload]} (templates evidence replicated-types)
          :let [floor (get floors [kind (kubectl/namespace-of workload) (kubectl/name-of workload)])]
          :when (and (= 1 (replicas workload)) (or (nil? floor) (= 1 floor)))]
      {:severity :info
       :component (kubectl/namespace-name-of workload)
       :summary (format "%s runs a single replica" kind)
       :hint "every node drain, eviction or rollout takes it down - run two or more replicas if it serves anything"})))

(defn detect-unprotected-workloads
  [evidence]
  (let [budgets (kubectl/items-of (get evidence "poddisruptionbudgets"))]
    (for [{:keys [kind workload template]} (templates evidence replicated-types)
          :when (< 1 (replicas workload))
          :let [labels (-> template :metadata :labels)]
          :when (not-any? #(and (= (kubectl/namespace-of %) (kubectl/namespace-of workload))
                                (kubectl/label-selector-matches? (-> % :spec :selector) labels))
                          budgets)]
      {:severity :info
       :component (kubectl/namespace-name-of workload)
       :summary (format "%s with %d replicas has no PodDisruptionBudget" kind (replicas workload))
       :hint "a node drain may evict all of its pods at once - add a PodDisruptionBudget with maxUnavailable: 1"})))

;; --- rollout capacity -------------------------------------------------------------

(defn- resolve-int-or-percent
  "A rolling-update bound as a pod count: an integer as is, a percentage of
   replicas rounded up for maxSurge and down for maxUnavailable, as the
   Deployment controller does."
  [value replicas round-up?]
  (cond
    (integer? value) value
    (and (string? value) (re-matches #"\d+%" value))
    (let [exact (/ (* replicas (parse-long (subs value 0 (dec (count value))))) 100)]
      (long (if round-up? (Math/ceil exact) (Math/floor exact))))
    :else nil))

(defn rolling-update-bounds
  "{:surge :unavailable} for a Deployment's rolling update, defaults 25%
   each, or nil for the Recreate strategy."
  [deployment]
  (let [strategy (-> deployment :spec :strategy)
        n (replicas deployment)]
    (when (not= "Recreate" (:type strategy))
      (let [{:keys [maxSurge maxUnavailable]} (:rollingUpdate strategy)
            surge (resolve-int-or-percent (if (some? maxSurge) maxSurge "25%") n true)
            unavailable (resolve-int-or-percent (if (some? maxUnavailable) maxUnavailable "25%") n false)]
        ;; the controller lets one pod go when both resolve to zero
        {:surge surge :unavailable (if (and (= 0 surge) (= 0 unavailable)) 1 unavailable)}))))

(defn detect-rollouts-without-capacity
  [evidence]
  (for [deployment (kubectl/items-of (get evidence "deployments"))
        :let [n (replicas deployment)
              {:keys [surge unavailable]} (rolling-update-bounds deployment)]
        :when (and (pos? n) surge unavailable (= 0 surge) (>= unavailable n))]
    {:severity :warning
     :component (kubectl/namespace-name-of deployment)
     :summary (format "Deployment's rolling update may take all %d pod(s) down at once (maxSurge 0, maxUnavailable %d)" n unavailable)
     :hint "no new pod starts before old ones stop - allow a surge or lower maxUnavailable below the replica count"}))

;; --- replicas on one node ------------------------------------------------------------

(defn- ready-pod?
  [pod]
  (and (= "Running" (-> pod :status :phase))
       (nil? (-> pod :metadata :deletionTimestamp))
       (conditions/ready? pod)))

(defn- schedulable-nodes
  [nodes]
  (remove #(true? (-> % :spec :unschedulable)) (kubectl/items-of nodes)))

(defn detect-replicas-on-one-node
  [evidence]
  (when (< 1 (count (schedulable-nodes (get evidence "nodes"))))
    (let [wanted (set (for [{:keys [kind workload]} (templates evidence replicated-types)
                            :when (< 1 (replicas workload))]
                        [(kubectl/namespace-of workload) (str kind "/" (kubectl/name-of workload))]))]
      (for [[[namespace workload] pods] (sort-by key (group-by (juxt kubectl/namespace-of kubectl/workload-of)
                                                               (filter ready-pod? (kubectl/items-of (get evidence "pods")))))
            :when (contains? wanted [namespace workload])
            :let [nodes (distinct (map #(-> % :spec :nodeName) pods))]
            :when (and (< 1 (count pods)) (= 1 (count nodes)))]
        {:severity :warning
         :component (str namespace "/" (subs workload (inc (str/index-of workload "/"))))
         :summary (format "%s runs all %d ready replicas on node %s" (first (str/split workload #"/")) (count pods) (first nodes))
         :hint "one node failure or drain takes every replica down - add a topologySpreadConstraint or podAntiAffinity on kubernetes.io/hostname"}))))

;; --- container hardening ---------------------------------------------------------

(defn- hardening-specs
  "The pod specs the hardening checks judge: every workload outside the
   platform namespaces, whose components need these powers."
  [evidence]
  (remove (fn [[_ workload _]] (contains? pod-security/platform-namespaces (kubectl/namespace-of workload)))
          (pod-specs evidence)))

(defn- containers-of
  [spec]
  (concat (:initContainers spec) (:containers spec)))

(defn may-run-as-root?
  "Whether nothing keeps the container from running as root: neither
   runAsNonRoot nor a non-zero runAsUser, on the container or inherited from
   the pod. The image's USER is not visible here, so this is a may."
  [pod-spec container]
  (let [effective (fn [k] (let [own (get-in container [:securityContext k])]
                            (if (some? own) own (get-in pod-spec [:securityContext k]))))]
    (not (or (true? (effective :runAsNonRoot))
             (pos? (or (effective :runAsUser) 0))))))

(defn detect-root-containers
  [evidence]
  (for [[kind workload spec] (hardening-specs evidence)
        :let [names (map :name (filter #(may-run-as-root? spec %) (containers-of spec)))]
        :when (seq names)]
    {:severity :info
     :component (kubectl/namespace-name-of workload)
     :summary (format "%s containers may run as root: %s" kind (str/join ", " names))
     :hint "a process escaping the container is root on the node's kernel - set runAsNonRoot: true (and runAsUser if the image has no USER)"}))

(defn detect-writable-root-filesystems
  [evidence]
  (for [[kind workload spec] (hardening-specs evidence)
        :let [names (map :name (remove #(true? (get-in % [:securityContext :readOnlyRootFilesystem])) (containers-of spec)))]
        :when (seq names)]
    {:severity :info
     :component (kubectl/namespace-name-of workload)
     :summary (format "%s containers keep a writable root filesystem: %s" kind (str/join ", " names))
     :hint "an attacker can drop tools into it - set readOnlyRootFilesystem: true and mount an emptyDir where the process writes"}))

(defn- automount?
  "Whether the pod mounts its ServiceAccount's token: the pod's setting wins,
   then the ServiceAccount's, and the default is to mount."
  [spec account]
  (let [own (:automountServiceAccountToken spec)]
    (if (some? own) own (not (false? (:automountServiceAccountToken account))))))

(defn detect-default-account-tokens
  [evidence]
  (let [accounts (into {} (for [account (kubectl/items-of (get evidence "serviceaccounts"))]
                            [[(kubectl/namespace-of account) (kubectl/name-of account)] account]))]
    (for [[kind workload spec] (hardening-specs evidence)
          :let [name (or (not-empty (:serviceAccountName spec)) "default")]
          :when (and (= "default" name)
                     (automount? spec (get accounts [(kubectl/namespace-of workload) name])))]
      {:severity :info
       :component (kubectl/namespace-name-of workload)
       :summary (format "%s mounts the default ServiceAccount's token" kind)
       :hint "a workload on the default account rarely talks to the API, yet anyone inside it can - set automountServiceAccountToken: false on the pod or the account"})))

;; --- requests and limits -------------------------------------------------------------

(defn- limit-range-defaults
  "The resources a namespace's LimitRanges give containers by default, as
   #{[namespace :requests :cpu] ...}. A LimitRange's default limit also
   becomes the request when no default request is set."
  [limitranges]
  (set (for [range (kubectl/items-of limitranges)
             limit (-> range :spec :limits)
             :when (= "Container" (:type limit))
             [kind source] [[:requests (merge (:default limit) (:defaultRequest limit))] [:limits (:default limit)]]
             resource (keys source)]
         [(kubectl/namespace-of range) kind resource])))

(defn missing-resources
  "What a container leaves unset: CPU and memory requests, and the memory
   limit. A CPU limit is left out on purpose - throttling a container that
   could use idle CPU is a choice, not a gap. A container's limit stands in
   for its missing request, which the API server copies it into."
  [container namespace defaults]
  (let [has? (fn [kind resource] (or (some? (get-in container [:resources kind resource]))
                                     (contains? defaults [namespace kind resource])))
        requested? (fn [resource] (or (has? :requests resource) (has? :limits resource)))]
    (cond-> []
      (not (requested? :cpu)) (conj "cpu request")
      (not (requested? :memory)) (conj "memory request")
      (not (has? :limits :memory)) (conj "memory limit"))))

(defn detect-missing-resources
  [evidence]
  (let [defaults (limit-range-defaults (get evidence "limitranges"))]
    (for [[kind workload spec] (hardening-specs evidence)
          :let [gaps (for [container (containers-of spec)
                           :let [missing (missing-resources container (kubectl/namespace-of workload) defaults)]
                           :when (seq missing)]
                       (format "%s (%s)" (:name container) (str/join ", " missing)))]
          :when (seq gaps)]
      {:severity :info
       :component (kubectl/namespace-name-of workload)
       :summary (format "%s containers lack resource settings: %s" kind (str/join "; " gaps))
       :hint "without requests the scheduler packs pods blind and they are evicted first; without a memory limit one leak can starve the node"})))

;; --- secrets in ConfigMaps -------------------------------------------------------

(def ^:private secret-key
  #"(?i)(passw(or)?d|passwd|secret|token|api[_-]?key|access[_-]?key|private[_-]?key|credential)")

(defn- placeholder?
  "Values a secret-sounding key holds when it is configuration, not a secret:
   booleans, numbers and durations, references, paths, and empty strings."
  [value]
  (let [v (str/trim (str value))]
    (or (str/blank? v)
        (re-matches #"(?i)true|false|yes|no|on|off|none|null|changeme|<.*>|\*+" v)
        (re-matches #"\d+(\.\d+)?\s*(ms|s|m|h|d|w)?" v)
        (re-matches #"\$\{?[\w.-]+\}?|\{\{.*\}\}" v)
        (str/starts-with? v "/")
        (str/starts-with? v "file:"))))

(defn secret-reasons
  "Why a ConfigMap entry looks like a credential, or nil. Only the reason is
   ever reported - never the value."
  [key value]
  (let [v (str value)]
    (cond
      (re-find #"-----BEGIN [A-Z ]*PRIVATE KEY-----" v) "a private key"
      (re-find #"\bAKIA[0-9A-Z]{16}\b" v) "an AWS access key"
      (re-find #"[a-zA-Z][a-zA-Z0-9+.-]*://[^/\s:@]+:[^/\s@]+@" v) "a URL with a password"
      (and (re-find secret-key (name key)) (not (placeholder? v))) "a secret-sounding key with a value"
      :else nil)))

(def ^:private exempt-configmaps
  "ConfigMaps the control plane publishes: certificates and public settings."
  #{"kube-root-ca.crt" "extension-apiserver-authentication" "cluster-info"})

(defn detect-configmap-secrets
  [evidence]
  (for [configmap (kubectl/items-of (get evidence "configmaps"))
        :when (not (contains? pod-security/platform-namespaces (kubectl/namespace-of configmap)))
        :when (not (contains? exempt-configmaps (kubectl/name-of configmap)))
        :let [hits (sort-by first (keep (fn [[k v]] (when-let [why (secret-reasons k v)] [(name k) why]))
                                        (:data configmap)))]
        :when (seq hits)]
    {:severity :warning
     :component (kubectl/namespace-name-of configmap)
     :summary (format "ConfigMap may hold credentials in %s" (str/join ", " (map first hits)))
     :hint (str (str/join "; " (for [[k why] hits] (str k ": " why)))
                " - anyone who can read ConfigMaps reads these; move them to a Secret and rotate them")}))

;; --- deprecated APIs ------------------------------------------------------------

(defn parse-deprecated-requests
  "The deprecated APIs the apiserver has served since it started, read from its
   apiserver_requested_deprecated_apis metric: one map per group, version,
   resource and subresource, with the release that removes it."
  [metrics]
  (vec (distinct
        (for [line (str/split-lines (str metrics))
              :when (str/starts-with? line "apiserver_requested_deprecated_apis{")
              :let [labels (into {} (for [[_ k v] (re-seq #"(\w+)=\"([^\"]*)\"" line)] [(keyword k) v]))]]
          (select-keys labels [:group :version :resource :subresource :removed_release])))))

(defn detect-deprecated-api-requests
  [evidence]
  (for [{:keys [group version resource subresource removed_release]} (get evidence "deprecated-api-requests")
        :let [api (if (str/blank? group) version (str group "/" version))]]
    {:severity :warning
     :component (str api " " resource (when-not (str/blank? subresource) (str "/" subresource)))
     :summary (if (str/blank? removed_release)
                (format "clients request the deprecated API %s" api)
                (format "clients request the deprecated API %s, removed in %s" api removed_release))
     :hint "the apiserver counts requests since it started; the audit log or `kubectl get --raw /metrics` with the user agent names the client - move it to the API that replaces this one before upgrading"}))

(defn detect-deprecated-stored-versions
  [evidence]
  (for [crd (kubectl/items-of (get evidence "customresourcedefinitions"))
        :let [versions (into {} (map (juxt :name identity)) (-> crd :spec :versions))]
        stored (-> crd :status :storedVersions)
        :let [version (get versions stored)]
        :when (or (nil? version) (not (:served version)) (:deprecated version))]
    {:severity :warning
     :component (kubectl/name-of crd)
     :summary (format "CustomResourceDefinition still stores objects at %s version %s"
                      (cond (nil? version) "the removed" (not (:served version)) "the unserved" :else "the deprecated")
                      stored)
     :hint (str (or (:deprecationWarning version) "")
                (when (:deprecationWarning version) " - ")
                "rewrite every object at the storage version (a no-op update of each does it), then remove the old version from status.storedVersions before an upgrade drops it")}))

(def detectives
  [{:name "readiness-probes"
    :description "Workload containers that serve ports without a readiness probe"
    :requires ["deployments" "statefulsets" "daemonsets"]
    :detect detect-missing-readiness-probes}
   {:name "mutable-images"
    :description "Containers whose image uses latest or no tag"
    :requires ["deployments" "statefulsets" "daemonsets" "cronjobs"]
    :detect detect-mutable-images}
   {:name "single-replicas"
    :description "Deployments and StatefulSets running a single replica"
    :requires ["deployments" "statefulsets" "horizontalpodautoscalers"]
    :detect detect-single-replicas}
   {:name "unprotected-workloads"
    :description "Replicated Deployments and StatefulSets no PodDisruptionBudget covers"
    :requires ["deployments" "statefulsets" "poddisruptionbudgets"]
    :detect detect-unprotected-workloads}
   {:name "rollouts-without-capacity"
    :description "Deployments whose rolling update may take every pod down at once"
    :requires ["deployments"]
    :detect detect-rollouts-without-capacity}
   {:name "replicas-on-one-node"
    :description "Replicated workloads whose ready pods all run on one node of several"
    :requires ["deployments" "statefulsets" "pods" "nodes"]
    :detect detect-replicas-on-one-node}
   {:name "missing-resources"
    :description "Containers without CPU or memory requests or a memory limit"
    :requires ["deployments" "statefulsets" "daemonsets" "cronjobs" "limitranges"]
    :detect detect-missing-resources}
   {:name "root-containers"
    :description "Workloads whose containers may run as root"
    :requires ["deployments" "statefulsets" "daemonsets" "cronjobs"]
    :detect detect-root-containers}
   {:name "writable-root-filesystems"
    :description "Workloads whose containers keep a writable root filesystem"
    :requires ["deployments" "statefulsets" "daemonsets" "cronjobs"]
    :detect detect-writable-root-filesystems}
   {:name "default-account-tokens"
    :description "Workloads that mount the default ServiceAccount's token"
    :requires ["deployments" "statefulsets" "daemonsets" "cronjobs" "serviceaccounts"]
    :detect detect-default-account-tokens}
   {:name "configmap-secrets"
    :description "ConfigMaps whose entries look like credentials"
    :requires ["configmaps"]
    :detect detect-configmap-secrets}
   {:name "deprecated-api-requests"
    :description "Deprecated APIs that clients still request"
    :requires ["deprecated-api-requests"]
    :detect detect-deprecated-api-requests}
   {:name "deprecated-stored-versions"
    :description "CustomResourceDefinitions that still store objects at a deprecated, unserved or removed version"
    :requires ["customresourcedefinitions"]
    :detect detect-deprecated-stored-versions}])
