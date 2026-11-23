;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.kubectl
  "kubectl wrappers plus pure helpers over parsed resource lists.

   Functions that talk to a cluster take an explicit context; everything else
   is a pure transformation over already-fetched data, so it stays testable."
  (:require [babashka.process :as process]
            [cheshire.core :as cheshire]
            [clojure.string :as str]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

;; ---------------------------------------------------------------------------
;; pure helpers over parsed resources

(defn items-of
  "The item seq of a resource list, accepting both parsed lists and plain seqs."
  [resource-list]
  (or (:items resource-list) resource-list))

(defn name-of [item] (-> item :metadata :name))
(defn namespace-of [item] (-> item :metadata :namespace))

(defn namespace-name-of
  [item]
  (format "%s/%s" (namespace-of item) (name-of item)))

(defn names-of
  [resource-list]
  (vec (distinct (map name-of (items-of resource-list)))))

(defn namespaces-of
  [resource-list]
  (vec (distinct (map namespace-of (items-of resource-list)))))

(defn namespaces-names
  [resource-list]
  (vec (distinct (map namespace-name-of (items-of resource-list)))))

(defn filter-list
  [resource-list predicate]
  (filter predicate (items-of resource-list)))

(defn find-item
  "The item with the given namespace/name (or plain name for cluster-scoped)."
  [resource-list namespace-name]
  (first (filter-list resource-list #(or (= namespace-name (namespace-name-of %))
                                         (= namespace-name (name-of %))))))

(defn workload-of
  "The workload that owns a pod, as \"Kind/name\". A Deployment's pods are owned
   by a ReplicaSet named after the Deployment plus the pod-template-hash, so the
   hash is stripped to count every revision of one Deployment as one workload."
  [pod]
  (let [owner (first (filter :controller (-> pod :metadata :ownerReferences)))
        hash (get-in pod [:metadata :labels :pod-template-hash])]
    (cond
      (nil? owner) (str "Pod/" (name-of pod))
      (and (= "ReplicaSet" (:kind owner)) hash (str/ends-with? (:name owner) (str "-" hash)))
      (str "Deployment/" (subs (:name owner) 0 (- (count (:name owner)) (inc (count hash)))))
      :else (str (:kind owner) "/" (:name owner)))))

(defn split-namespace-name
  [namespace-name]
  (let [[namespace name] (str/split namespace-name #"/" 2)]
    {:namespace namespace :name name}))


(defn taint->str
  [{:keys [key value effect]}]
  (if (str/blank? (str value))
    (format "%s:%s" key effect)
    (format "%s=%s:%s" key value effect)))

(defn taints-of
  [node]
  (mapv taint->str (-> node :spec :taints)))

(defn taint-removal-spec
  "kubectl removes a taint by its 'key:effect' (the value must not appear),
   derived here from the 'key[=value]:effect' display form."
  [taint]
  (let [[key-value effect] (str/split (str taint) #":" 2)
        key (first (str/split key-value #"=" 2))]
    (str key ":" effect)))

(defn nodes-with-taints
  [nodes-list]
  (names-of (filter-list nodes-list #(seq (-> % :spec :taints)))))

(defn unschedulable-nodes
  [nodes-list]
  (names-of (filter-list nodes-list #(true? (-> % :spec :unschedulable)))))

(defn schedulable-nodes
  [nodes-list]
  (names-of (filter-list nodes-list #(not (true? (-> % :spec :unschedulable))))))

(defn claimed-pvcs
  "Set of namespace/name for every PersistentVolumeClaim referenced by a pod."
  [pods-list]
  (into #{}
        (for [pod (items-of pods-list)
              volume (-> pod :spec :volumes)
              :let [claim (-> volume :persistentVolumeClaim :claimName)]
              :when claim]
          (format "%s/%s" (namespace-of pod) claim))))

(defn unused-pvcs
  "Namespace/name of every PersistentVolumeClaim no pod refers to."
  [pvcs-list pods-list]
  (let [claimed (claimed-pvcs pods-list)]
    (vec (remove claimed (namespaces-names pvcs-list)))))

(defn terminating-namespaces
  [namespaces-list]
  (names-of (filter-list namespaces-list #(= "Terminating" (-> % :status :phase)))))

(defn remove-kubernetes-finalizer
  "Drops the 'kubernetes' finalizer that blocks namespace termination."
  [namespace-resource]
  (update-in namespace-resource [:spec :finalizers]
             (fn [finalizers] (vec (remove #{"kubernetes"} finalizers)))))

(defn service-port
  "Numeric port of the named port of a service."
  [service port-name]
  (->> (-> service :spec :ports)
       (filter #(= port-name (:name %)))
       first
       :port))

(defn service-port-or-first
  "The named port when present, otherwise the service's first port - so a
   port-forward always has something to target."
  [service port-name]
  (or (service-port service port-name)
      (-> service :spec :ports first :port)))

(defn image-counts
  "Frequencies of every container image used by the given pods."
  [pods-list]
  (frequencies (for [pod (items-of pods-list)
                     container (-> pod :spec :containers)]
                 (:image container))))

(defn pods-with-image-pull-back-off
  [pods-list]
  (namespaces-names
   (filter-list pods-list
                (fn [pod]
                  (some #(= "ImagePullBackOff" (-> % :state :waiting :reason))
                        (-> pod :status :containerStatuses))))))

(defn daemonset-pod?
  [pod]
  (boolean (some #(= "DaemonSet" (:kind %)) (-> pod :metadata :ownerReferences))))

(defn mirror-pod?
  [pod]
  (contains? (-> pod :metadata :annotations) (keyword "kubernetes.io/config.mirror")))

(defn drainable-pods
  "The pods a drain must evict: everything that is neither a daemonset pod
   nor a static mirror pod."
  [pods-list]
  (vec (remove #(or (daemonset-pod? %) (mirror-pod? %)) (items-of pods-list))))

(defn namespaces-with-ingress-host
  "The namespaces whose Ingresses serve the given host."
  [ingresses-list host]
  (vec (distinct (for [ingress (items-of ingresses-list)
                       rule (-> ingress :spec :rules)
                       :when (= host (:host rule))]
                   (namespace-of ingress)))))

(defn hpa-scaling-metrics
  "'<metric>/<target-type> (<metric-type>)' for every HPA metric. Resource
   metrics are named directly, Pods/Object/External metrics carry a nested
   metric name."
  [hpas-list]
  (vec (sort (distinct
              (for [hpa (items-of hpas-list)
                    metric (-> hpa :spec :metrics)
                    :let [type (:type metric)
                          spec (get metric (keyword (str/lower-case (str type))))
                          name (or (:name spec) (-> spec :metric :name))
                          target (-> spec :target :type)]
                    :when name]
                (format "%s/%s (%s)" name target type))))))

;; ---------------------------------------------------------------------------
;; talking to the cluster

(defn parse-json
  "Parses kubectl JSON output with keyword keys. Empty output (a missing CRD, a
   failed command) is nil, and truncated or malformed output is nil rather than
   an exception - so one unreadable resource type degrades to no-evidence
   instead of crashing a whole investigation. Every read here keeps this
   contract; a caller that must tell a failed read from an empty one uses a
   -result read instead."
  [json-string]
  (try
    (cheshire/parse-string json-string true)
    (catch Exception _ nil)))


(defn- current-config
  []
  (parse-json (shell/stdout-of "kubectl" "config" "view" "--raw" "--output" "json")))

(defn current-context
  []
  (:current-context (current-config)))

(defn current-cluster
  "The name of the cluster the current kubectl context targets, verbatim from the
   kubeconfig. This is the operator's real cluster name; env.edn matches
   environments and capabilities against it."
  []
  (let [config (current-config)
        context-name (:current-context config)]
    (-> (first (filter #(= (:name %) context-name) (:contexts config)))
        :context
        :cluster)))

(defn can-i?
  "Whether the current user may perform verb on a single resource type, cluster
   wide."
  [verb resource]
  (zero? (shell/exit-code-of "kubectl" "auth" "can-i" verb resource "--all-namespaces" "--quiet")))

(defn can-exec?
  "Whether the current user may exec into a single resource type."
  [resource]
  (zero? (shell/exit-code-of "kubectl" "auth" "can-i" "create" resource
                             "--subresource=exec" "--all-namespaces" "--quiet")))

(defn resource-exists?
  "Whether a named cluster-scoped resource (e.g. a CRD) is present. Bounded with a
   short request timeout so capability detection stays snappy against a wedged or
   unreachable API server."
  [type name]
  (zero? (shell/exit-code-of "kubectl" "get" type name "--request-timeout=5s")))

(defn any-matching?
  "Whether any resource of `type` matches a label `selector`, across all namespaces
   - the existence check for a tool installed WITHOUT its own CRD, where a Helm
   chart instead labels its workloads (app.kubernetes.io/name=<tool>). Name-only and
   bounded with a short timeout, so capability detection stays snappy; runs against
   the current kubectl context, like resource-exists?. An unreachable API or a
   no-match both yield false (absent)."
  [type selector]
  (not (str/blank? (shell/stdout-of "kubectl" "get" type "--all-namespaces"
                                    (str "--selector=" selector)
                                    "--output=name" "--request-timeout=5s"))))

(defn serves-crd?
  "Whether the cluster serves a CRD by its full plural.group name (e.g.
   \"certificates.cert-manager.io\") - the existence check for an operator that
   installs its own CRD. A capability detector for such a tool is just
   #(serves-crd? \"…\"), the CRD-shaped sibling of resource-exists? and any-matching?."
  [crd]
  (resource-exists? "customresourcedefinition" crd))

(defn parse-api-resources
  "Parses `kubectl api-resources --no-headers` into a map from resource name to
   Kind, keyed by both the bare plural (\"pods\") and the group-qualified plural
   (\"helmreleases.helm.toolkit.fluxcd.io\"), so a lookup resolves whichever form
   a book wrote. Columns are NAME [SHORTNAMES] APIVERSION NAMESPACED KIND; the
   SHORTNAMES column is optional, so the stable fields are read from the edges -
   NAME first, then KIND, NAMESPACED, APIVERSION counted back from the right."
  [output]
  (into {}
        (for [line (str/split-lines (str output))
              :let [fields (str/split (str/trim line) #"\s+")
                    n (count fields)]
              :when (>= n 4)
              :let [resource (first fields)
                    kind (peek fields)
                    api-version (nth fields (- n 3))
                    group (when (str/includes? api-version "/")
                            (first (str/split api-version #"/")))]
              entry (cond-> [[resource kind]]
                      group (conj [(str resource "." group) kind]))]
          entry)))

(defonce ^:private resource-kinds-cache (atom nil))

(defn resource-kinds
  "A map from resource name to Kind, discovered once from `kubectl api-resources`
   and cached for the run. Empty when the API is unreachable, so a caller falls
   back to the raw resource name instead of guessing a Kind."
  []
  (or @resource-kinds-cache
      (reset! resource-kinds-cache
              (parse-api-resources
               (shell/stdout-of "kubectl" "api-resources" "--no-headers"
                                "--request-timeout=5s")))))

(defn resource-kind
  "The Kind the cluster reports for a resource name - \"pods\" -> \"Pod\",
   \"helmreleases.helm.toolkit.fluxcd.io\" -> \"HelmRelease\" - or nil when the API
   is unreachable or serves no such resource. The cluster's own discovery is the
   single source of truth for a resource's Kind; there is no hardcoded table to
   drift out of date."
  [resource]
  (get (resource-kinds) resource))

;; A bounded request timeout on every read means a wedged API server slows a
;; book down by seconds, never hangs it forever. It applies only to these
;; quick get calls - long-running work (port-forward, drain, exec, watches,
;; interactive sessions) streams through gumshoe.shell with no timeout and keeps
;; running for as long as it needs.
(def ^:private request-timeout "--request-timeout=30s")

(def ^:dynamic *strict-reads*
  "When true, a plain read that did not answer throws an ex-info carrying
   ::unread instead of returning nil. select/resolve-target binds it while it
   lists candidates, so an unreachable API is reported as such rather than as
   an empty list - every candidate fn built on these readers inherits it."
  false)

(defn- answered-empty?
  "A failed kubectl get that still answered: the object or the resource type
   does not exist."
  [err]
  (boolean (re-find #"\(NotFound\)|doesn't have a resource type" (str err))))

(defn- read-json
  "The parsed output of a kubectl get, nil when it failed or did not parse.
   Under *strict-reads* the exit status is kept, and a read that did not
   answer throws."
  [& argv]
  (if-not *strict-reads*
    (parse-json (apply shell/stdout-of argv))
    (let [{:keys [exit out err]} (apply shell/execute argv)
          doc (when (zero? exit) (parse-json (str/trim (str out))))]
      (when (and (nil? doc) (not (answered-empty? err)))
        (throw (ex-info (format "kubectl get did not answer: %s"
                                (or (not-empty (str/trim (str err))) (str "exit " exit)))
                        {::unread true})))
      doc)))

(defn get-json
  "Runs `kubectl get` with `args` - a type, names, selectors, a namespace, any
   flag - and returns the parsed JSON document, or nil when the read failed or
   its output did not parse (see parse-json). For a read no other function
   here wraps:

     (get-json context \"namespaces\" \"--selector=team=web\")"
  [context & args]
  (apply read-json "kubectl" (str "--context=" context) request-timeout
         "get" (concat args ["--output=json"])))

(defn get-json-result
  "Like get-json, but returns {:ok? <the read answered> :doc <the document>}.
   :ok? is true only when kubectl exited cleanly and its output parsed, so a
   gate on an absence can require that the read answered before it trusts an
   empty result: a throttled API or a missing CRD is not ok?, where get-json
   would have returned the same nil as a failed parse."
  [context & args]
  (let [{:keys [exit out]} (apply shell/execute "kubectl" (str "--context=" context) request-timeout
                                  "get" (concat args ["--output=json"]))
        doc (parse-json out)]
    {:ok? (and (zero? exit) (some? doc)) :doc doc}))

(defn get-all
  [context type]
  (get-json context type "--all-namespaces"))

(defn label-selector-matches?
  "Whether a metav1.LabelSelector selects labels. An empty selector selects
   everything and a nil one nothing, as the API defines them."
  [selector labels]
  (boolean
   (when (map? selector)
     ;; label keys parse to keywords on both sides, prefix and all, so they
     ;; compare as they are
     (and (every? (fn [[k v]] (= v (get labels k))) (:matchLabels selector))
            (every? (fn [{:keys [key operator values]}]
                      (let [present (get labels (keyword key))]
                        (case operator
                          "In" (contains? (set values) present)
                          "NotIn" (not (contains? (set values) present))
                          "Exists" (some? present)
                          "DoesNotExist" (nil? present)
                          false)))
                    (:matchExpressions selector))))))

(defn status-types
  "The plural.group of every CRD that serves a version with a status
   subresource, minus the covered ones. A CRD without status has no
   conditions to read."
  [crds covered]
  (vec (sort (distinct (for [crd (items-of crds)
                             :let [type (name-of crd)]
                             :when (and type (not (contains? covered type)))
                             :when (some #(and (:served %) (contains? (:subresources %) :status))
                                         (-> crd :spec :versions))]
                         type)))))

(defn custom-resources
  "Every object of the status-carrying CRDs not in covered, or nil when the
   CRDs cannot be listed. Each type is read on its own, so one type the
   caller may not list leaves the others readable."
  [context covered]
  (when-let [crds (get-all context "customresourcedefinitions")]
    (vec (mapcat #(or (:items %) [])
                 (pmap #(get-all context %) (status-types crds covered))))))

(defn secret-names
  "The namespace/name of every Secret, or nil when they cannot be listed. Only
   the names are kept: a detective that checks a reference to a Secret needs
   to know it exists, never what it holds."
  [context]
  (let [{:keys [exit out]} (shell/execute "kubectl" (str "--context=" context) request-timeout
                                          "get" "secrets" "--all-namespaces"
                                          "--output=jsonpath={range .items[*]}{.metadata.namespace}/{.metadata.name}{\"\\n\"}{end}")]
    (when (zero? exit)
      (vec (remove str/blank? (str/split-lines (str out)))))))

(defn parse-helm-listing
  "Reads the tab-separated lines of helm-releases into maps, one per release
   revision."
  [text]
  (vec (for [line (str/split-lines (str text))
             :let [[namespace release status version modified created created-at] (str/split line #"\t" -1)]
             :when (and (seq namespace) (seq release) (seq status))]
         {:namespace namespace :release release :status status
          :version (some-> version not-empty parse-long)
          :modified-at (some-> modified not-empty parse-long)
          :created-at (some-> created not-empty parse-long)
          :creation-timestamp (not-empty created-at)})))

(defn helm-releases
  "Every revision Helm stores as a Secret, read from its labels, or nil when
   they cannot be listed. The jsonpath selects metadata only, so the release
   payload (rendered manifests and values, which may hold credentials) never
   reaches gumshoe."
  [context]
  (let [{:keys [exit out]} (shell/execute "kubectl" (str "--context=" context) request-timeout
                                          "get" "secrets" "--all-namespaces"
                                          "--selector=owner=helm"
                                          "--field-selector=type=helm.sh/release.v1"
                                          (str "--output=jsonpath={range .items[*]}"
                                               "{.metadata.namespace}{\"\\t\"}{.metadata.labels.name}{\"\\t\"}"
                                               "{.metadata.labels.status}{\"\\t\"}{.metadata.labels.version}{\"\\t\"}"
                                               "{.metadata.labels.modifiedAt}{\"\\t\"}{.metadata.labels.createdAt}{\"\\t\"}"
                                               "{.metadata.creationTimestamp}{\"\\n\"}{end}"))]
    (when (zero? exit)
      (parse-helm-listing out))))

(defn parse-tls-listing
  "Reads the tab-separated lines of tls-secrets into maps."
  [text]
  (vec (for [line (str/split-lines (str text))
             :let [[namespace name certificate-name crt] (str/split line #"\t" -1)]
             :when (and (seq namespace) (seq name))]
         {:namespace namespace :name name
          :certificate-name (not-empty certificate-name)
          :crt (not-empty crt)})))

(defn tls-secrets
  "Every Secret of type kubernetes.io/tls with its certificate and the
   cert-manager Certificate that manages it, or nil when they cannot be
   listed. The jsonpath selects tls.crt only, so the private key never
   reaches gumshoe."
  [context]
  (let [{:keys [exit out]} (shell/execute "kubectl" (str "--context=" context) request-timeout
                                          "get" "secrets" "--all-namespaces"
                                          "--field-selector=type=kubernetes.io/tls"
                                          (str "--output=jsonpath={range .items[*]}"
                                               "{.metadata.namespace}{\"\\t\"}{.metadata.name}{\"\\t\"}"
                                               "{.metadata.annotations.cert-manager\\.io/certificate-name}{\"\\t\"}"
                                               "{.data.tls\\.crt}{\"\\n\"}{end}"))]
    (when (zero? exit)
      (parse-tls-listing out))))

(defn get-raw
  "Reads a raw API path, parsed as JSON. Needed for data the resource API does
   not carry - kubelet's per-node stats among it. A failed call parses to nil,
   like every other read here."
  [context path]
  (parse-json (shell/stdout-of "kubectl" (str "--context=" context) request-timeout
                          "get" "--raw" path)))

(defn raw-text
  "Reads a raw API path as text, or nil when the call fails - the apiserver's
   /metrics among them, which is not JSON."
  [context path]
  (let [{:keys [exit out]} (shell/execute "kubectl" (str "--context=" context) request-timeout
                                          "get" "--raw" path)]
    (when (zero? exit) out)))

(defn node-stats-summary
  "One node's kubelet stats summary, which reports per-pod volume consumption.
   A pod whose containers are not running has been observed to be absent from
   it, so a caller must treat a missing pod as unknown rather than as idle."
  [context node]
  (get-raw context (format "/api/v1/nodes/%s/proxy/stats/summary" node)))

(defn container-logs
  "The tail of one container's log as {:read? bool :log string}. A log that
   could not be read is not an empty log, and the caller must be able to tell
   them apart, so the exit code is reported rather than swallowed."
  [context namespace pod container lines]
  (let [result (shell/execute "kubectl" (str "--context=" context)
                              (str "--namespace=" namespace) request-timeout
                              "logs" pod (str "--container=" container)
                              (str "--tail=" lines))]
    {:read? (zero? (:exit result))
     :log (str (:out result))}))

(defn get-selected
  [context type selector]
  (get-json context type "--all-namespaces" (str "--selector=" selector)))

(defn get-cluster-resource
  [context type name]
  (get-json context type name))

(defn get-namespaced-resource
  [context namespace type name]
  (read-json "kubectl" (str "--context=" context) (str "--namespace=" namespace) request-timeout
             "get" type name "--output=json"))

(defn pods-on-node
  [context node]
  (get-json context "pods" "--all-namespaces" (str "--field-selector=spec.nodeName=" node)))

(defn get-namespaced
  "Every resource of a type within one namespace."
  [context namespace type]
  (read-json "kubectl" (str "--context=" context) (str "--namespace=" namespace) request-timeout
             "get" type "--output=json"))

(defn get-namespaced-result
  "Like get-namespaced, but returns {:ok? <did the read succeed> :items <seq>}.
   The plain reads collapse any failure - a throttled API, a network blip, an
   absent CRD - into the same empty result as 'genuinely nothing there', which is
   exactly what you must NOT do when gating on an ABSENCE (a transient failure
   then reads as 'absent' and the gate opens). This keeps the exit status, so a
   caller can require that the read actually answered before trusting an empty."
  [context namespace type]
  (let [{:keys [ok? doc]} (get-json-result context type (str "--namespace=" namespace))]
    {:ok? ok? :items (items-of doc)}))

(defn confirmed-absent?
  "True ONLY when a namespaced read succeeds AND no item matches `pred` - the
   fail-safe way to gate on an absence. A failed or unreachable read returns false
   (not confirmed), so a gate built on it stays closed on uncertainty instead of
   treating 'could not read' as 'absent' and acting. Use this instead of
   (not (some pred (get-namespaced ...))) whenever proceeding on an absence would
   be destructive."
  [context namespace type pred]
  (let [{:keys [ok? items]} (get-namespaced-result context namespace type)]
    (and ok? (not-any? pred items))))

(defn container-statuses
  "Every container status of a pod: the init containers first, among them
   restartable ones (native sidecars), then the app containers. A failing
   init container keeps a pod from starting, and a failing sidecar runs
   beside the app, so a check on containers reads both."
  [pod]
  (concat (-> pod :status :initContainerStatuses) (-> pod :status :containerStatuses)))

(defn container-names
  [pod]
  (mapv :name (-> pod :spec :containers)))

(defn exec-stdout
  "Runs a command in a pod's container and returns its stdout (empty when the
   command is absent or the exec fails - gumshoe.shell never throws)."
  [context namespace pod container command]
  (apply shell/stdout-of "kubectl" (str "--context=" context) (str "--namespace=" namespace) request-timeout
         "exec" pod "-c" container "--" command))

(defn patch!
  "Merge-patches a resource. Returns the full process map."
  [context namespace type name patch]
  (shell/execute "kubectl" (str "--context=" context) (str "--namespace=" namespace)
                 "patch" type name "--type=merge" (str "--patch=" (cheshire/generate-string patch))))

(defn- port-open?
  [port]
  (try
    (with-open [_socket (java.net.Socket. "127.0.0.1" (int port))]
      true)
    (catch Exception _ false)))

(defn with-port-forward
  "Runs f while the service's remote-port is forwarded to local-port, then
   tears the forward down no matter what f does. Refuses to run f unless the
   forward is genuinely ours: a local-port already in use would silently point
   f at the wrong service, so that is an error, not a surprise."
  [{:keys [context namespace service local-port remote-port]} f]
  (cond
    (nil? remote-port)
    (do (stdout/error (format "service/%s has no resolvable port to forward" service)) false)

    (port-open? local-port)
    (do (stdout/error (format "local port %d is already in use - pick another with --port" local-port))
        false)

    :else
    (let [forward (try
                    ;; inherit stdout/stderr: kubectl port-forward logs a line per
                    ;; connection, and with the default captured pipes (that
                    ;; nothing reads) a busy tunnel fills the buffer and blocks,
                    ;; silently stalling the forward.
                    (process/process {:out :inherit :err :inherit}
                                     ["kubectl" (str "--context=" context) (str "--namespace=" namespace)
                                      "port-forward" (str "service/" service)
                                      (str local-port ":" remote-port)])
                    (catch Exception e
                      (stdout/error (format "could not start kubectl port-forward: %s" (ex-message e)))
                      nil))]
      (if (nil? forward)
        false
        (try
          (loop [attempt 0]
            (cond
              (port-open? local-port) (f)
              (>= attempt 30) (do (stdout/error (format "port-forward to service/%s did not come up within 30 seconds" service))
                                  false)
              :else (do (Thread/sleep 1000)
                        (recur (inc attempt)))))
          (finally
            (process/destroy-tree forward)))))))

(defn unused-pvc-candidates
  "Namespace/name of every PersistentVolumeClaim no pod refers to, for a book
   that offers them for deletion. Throws when the pods could not be read:
   without them every claim would look unused."
  [context]
  (let [pods (get-json-result context "pods" "--all-namespaces")
        pvcs (get-json-result context "persistentvolumeclaims" "--all-namespaces")]
    (when-not (and (:ok? pods) (:ok? pvcs))
      (throw (ex-info (str "could not read the pods and PersistentVolumeClaims of the cluster;"
                           " refusing to call any claim unused")
                      {:pods-read? (:ok? pods) :pvcs-read? (:ok? pvcs)})))
    (unused-pvcs (:doc pvcs) (:doc pods))))

(defn pvc-unused?
  "True only when the pods of `namespace` were read and none refers to the
   claim `name` - the check to repeat right before deleting it."
  [context namespace name]
  (confirmed-absent? context namespace "pods"
                     #(contains? (claimed-pvcs {:items [%]}) (format "%s/%s" namespace name))))
