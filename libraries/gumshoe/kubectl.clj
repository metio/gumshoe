;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.kubectl
  "kubectl wrappers plus pure helpers over parsed resource lists.

   Functions that talk to a cluster take an explicit context; everything else
   is a pure transformation over already-fetched data, so it stays testable."
  (:require [babashka.process :as process]
            [cheshire.core :as cheshire]
            [clojure.string :as str]
            [gumshoe.concurrent :as concurrent]
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

(defn namespace-name-map
  "The map {:namespace .. :name ..} for a \"namespace/name\" string, split at
   the first slash. It is a map, so destructure it by key:
   (let [{:keys [namespace name]} (namespace-name-map target)] ..)."
  [namespace-name]
  (let [[namespace name] (str/split namespace-name #"/" 2)]
    {:namespace namespace :name name}))

(defn split-namespace-name
  "Deprecated alias of namespace-name-map: returns the map
   {:namespace .. :name ..}, never a pair, so destructure it by key."
  {:deprecated "2026.11 - use namespace-name-map"}
  [namespace-name]
  (namespace-name-map namespace-name))


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
  "Set of namespace/name for every PersistentVolumeClaim referenced by a pod:
   named in a persistentVolumeClaim volume, or created for a generic
   ephemeral volume, whose claim Kubernetes names <pod>-<volume>."
  [pods-list]
  (into #{}
        (for [pod (items-of pods-list)
              volume (-> pod :spec :volumes)
              :let [claim (or (-> volume :persistentVolumeClaim :claimName)
                              (when (:ephemeral volume)
                                (str (-> pod :metadata :name) "-" (:name volume))))]
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

(defn pod-containers
  "Every container a pod spec declares, each with ::role: :container, :init,
   :sidecar for a native sidecar (an init container with restartPolicy
   Always, which runs for the pod's whole life and adds its requests to the
   pod's) or :ephemeral for a debug container added to a running pod."
  [pod]
  (let [spec (:spec pod)]
    (concat (map #(assoc % ::role :container) (:containers spec))
            (map #(assoc % ::role (if (= "Always" (:restartPolicy %)) :sidecar :init)) (:initContainers spec))
            (map #(assoc % ::role :ephemeral) (:ephemeralContainers spec)))))

(defn image-usage
  "Every image the given pods run, in any container, as rows
   {:image .. :count <containers running it> :roles #{<pod-containers roles>}},
   most used first."
  [pods-list]
  (->> (for [pod (items-of pods-list)
             container (pod-containers pod)]
         container)
       (group-by :image)
       (map (fn [[image containers]]
              {:image image :count (count containers) :roles (into (sorted-set) (map ::role) containers)}))
       (sort-by (juxt (comp - :count) :image))))

(defn image-counts
  "Frequencies of every image the given pods run, in any of their containers."
  [pods-list]
  (into {} (map (juxt :image :count)) (image-usage pods-list)))

(defn container-resource-rows
  "One row per container of the given pods that can declare resources -
   regular, init and native sidecar containers, never ephemeral ones - as
   {:namespace :pod :container :role :memory-request :memory-limit
    :cpu-request :cpu-limit}, a value nil when it is not set. A pod that
   sets pod-level resources (spec.resources, which the scheduler reserves
   for the whole pod) gets a row of its own first, with role :pod and
   container \"-\", so a pod budgeted only at that level does not read as
   one without requests."
  [pods-list]
  (for [pod (items-of pods-list)
        container (concat (when (seq (-> pod :spec :resources))
                            [{:name "-" ::role :pod :resources (-> pod :spec :resources)}])
                          (pod-containers pod))
        :when (not= :ephemeral (::role container))
        :let [{:keys [requests limits]} (:resources container)]]
    {:namespace (namespace-of pod)
     :pod (name-of pod)
     :container (:name container)
     :role (::role container)
     :memory-request (:memory requests)
     :memory-limit (:memory limits)
     :cpu-request (:cpu requests)
     :cpu-limit (:cpu limits)}))

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


(def current-config-args
  "Reads the kubeconfig's current context only, with its credentials redacted:
   without --raw kubectl prints REDACTED or DATA+OMITTED in place of every key,
   token and password, and --minify drops the other contexts, clusters and
   users. We only need two names, so no credential enters this process."
  ["kubectl" "config" "view" "--minify" "--output" "json"])

(defn- current-config
  []
  (parse-json (apply shell/stdout-of current-config-args)))

(defn current-context
  []
  (:current-context (current-config)))

(defn cluster-of
  "Pure: the cluster name the current context of a kubeconfig (as `kubectl
   config view` prints it) targets, or nil."
  [config]
  (let [context-name (:current-context config)]
    (-> (first (filter #(= (:name %) context-name) (:contexts config)))
        :context
        :cluster)))

(defn current-cluster
  "The name of the cluster the current kubectl context targets, verbatim from the
   kubeconfig. This is the operator's real cluster name; env.edn matches
   environments and capabilities against it."
  []
  (cluster-of (current-config)))

(defn resource-and-subresource
  "A resource as a permission prerequisite names it, split into the resource
   and its subresource: \"pods/eviction\" -> [\"pods\" \"eviction\"],
   \"deployments\" -> [\"deployments\" nil]. `kubectl auth can-i` reads
   \"pods/eviction\" as the pod named eviction, so the subresource must go to
   it as --subresource."
  [resource]
  (let [[base sub] (str/split (str resource) #"/" 2)]
    [base (when-not (str/blank? sub) sub)]))

(defn can-i-args
  "The `kubectl auth can-i` arguments that ask whether the current user may
   perform verb on resource, which may name a subresource as
   \"deployments/scale\": across every namespace, or in namespace when one is
   given."
  ([verb resource] (can-i-args verb resource nil))
  ([verb resource namespace]
   (let [[base sub] (resource-and-subresource resource)]
     (cond-> ["auth" "can-i" verb base]
       sub (conj (str "--subresource=" sub))
       namespace (conj (str "--namespace=" namespace))
       (nil? namespace) (conj "--all-namespaces")
       true (conj "--quiet")))))

(defn can-i?
  "Whether the current user may perform verb on a single resource type,
   cluster wide. resource may name a subresource, as \"pods/eviction\"."
  [verb resource]
  (zero? (apply shell/exit-code-of "kubectl" (can-i-args verb resource))))

(declare ^:private request-timeout)

(defn can-i-in?
  "Whether the user of context may perform verb on resource in namespace.
   resource may name a subresource, as \"namespaces/finalize\". A check that
   did not answer is false, so a refusal built on it fails closed."
  [context verb resource namespace]
  (zero? (apply shell/exit-code-of "kubectl" (str "--context=" context) request-timeout
                (can-i-args verb resource namespace))))

(defn missing-rights
  "The [verb resource] pairs of checks the user of context may not perform in
   namespace, in order, asked concurrently. A check that did not answer counts
   as missing."
  [context namespace checks]
  (let [granted (concurrent/map-all (fn [[verb resource]] (can-i-in? context verb resource namespace)) checks)]
    (vec (keep (fn [[check ok?]] (when-not (true? ok?) check)) (map vector checks granted)))))

(defn can-exec?
  "Whether the current user may exec into a single resource type."
  [resource]
  (can-i? "create" (str resource "/exec")))

(defn- answered-empty?
  "A failed kubectl get that still answered: the object or the resource type
   does not exist."
  [err]
  (boolean (re-find #"\(NotFound\)|doesn't have a resource type" (str err))))

(defn existence-of
  "Pure: the answer of a name-only existence read (`kubectl get ..
   --output=name`): true when it named something, false when it answered that
   nothing matches - a clean exit with no names, NotFound, or a resource type
   the API server does not have - and nil when it did not answer, so an
   unreachable or wedged API server is never read as an absence."
  [{:keys [exit out err]}]
  (cond
    (and (= 0 exit) (not (str/blank? out))) true
    (= 0 exit) false
    (answered-empty? err) false
    :else nil))

(defn resource-exists?
  "Whether a named cluster-scoped resource (e.g. a CRD) is present: true,
   false when the API server answered that it is absent, or nil when it did
   not answer. Bounded with a short request timeout so capability detection
   stays snappy against a wedged or unreachable API server."
  [type name]
  (existence-of (shell/execute "kubectl" "get" type name "--output=name" "--request-timeout=5s")))

(defn any-matching?
  "Whether any resource of `type` matches a label `selector`, across all namespaces
   - the existence check for a tool installed WITHOUT its own CRD, where a Helm
   chart instead labels its workloads (app.kubernetes.io/name=<tool>). Name-only and
   bounded with a short timeout, so capability detection stays snappy; runs against
   the current kubectl context, like resource-exists?. True, false for an answered
   no-match, or nil when the API server did not answer."
  [type selector]
  (existence-of (shell/execute "kubectl" "get" type "--all-namespaces"
                               (str "--selector=" selector)
                               "--output=name" "--request-timeout=5s")))

(defn serves-crd?
  "Whether the cluster serves a CRD by its full plural.group name (e.g.
   \"certificates.cert-manager.io\") - the existence check for an operator that
   installs its own CRD. True, false, or nil when the API server did not
   answer, as resource-exists? returns. A capability detector for such a tool is just
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

(def ^:private incomplete-discovery-ttl-ms
  "How long a discovery that did not fully answer stands for the run before
   the next call asks again. Long enough that building one prerequisite list
   asks once, short enough that a recovered API is asked again later."
  60000)

(defn- now-ms [] (System/currentTimeMillis))

(defn- kinds-discovery
  "One `kubectl api-resources` call, as {:kinds .. :complete? .. :at ..}.
   kubectl exits non-zero while still printing every group it did read when
   one aggregated API is unavailable, so the output is parsed whatever the
   exit; only an exit 0 with kinds counts as a complete answer."
  []
  (let [{:keys [exit out]} (shell/execute "kubectl" "api-resources" "--no-headers"
                                          "--request-timeout=5s")
        kinds (parse-api-resources out)]
    {:kinds kinds
     :complete? (and (= 0 exit) (boolean (seq kinds)))
     :at (now-ms)}))

(defn- fresh-discovery?
  [{:keys [complete? at]}]
  (or complete? (< (- (now-ms) at) incomplete-discovery-ttl-ms)))

(defn resource-kinds
  "A map from resource name to Kind, discovered from `kubectl api-resources`.
   For labels only: a caller falls back to the raw resource name for a
   resource the map lacks instead of guessing a Kind. A complete discovery is
   cached for the run. A discovery that failed or answered only in part keeps
   what it read for incomplete-discovery-ttl-ms, so the labels of one
   prerequisite list trigger at most one call against an unreachable or
   partly unavailable API, and a later call asks again once that time has
   passed."
  []
  (let [cached @resource-kinds-cache]
    (:kinds (if (and cached (fresh-discovery? cached))
              cached
              (reset! resource-kinds-cache (kinds-discovery))))))

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
  "When true, a read that did not answer throws an ex-info carrying ::unread
   instead of returning nil or {:ok? false}. A read that answered that the
   object or the resource type does not exist still returns as it would
   unbound. select/listed binds it while it lists candidates, so an
   unreachable API is reported as such rather than as an empty list;
   gumshoe.mutation binds it around :derive and :precondition, and
   verify/eventually around every post-check.

   The strict readers are get-json, get-json-result, get-all, get-namespaced,
   get-namespaced-result, get-namespaced-resource, confirmed-absent? and
   helm-releases, and every function built on them inherits it. The other
   readers here (secret-names, tls-secrets, get-raw, raw-text, any-matching?,
   resource-kinds) ignore it."
  false)

(def ^:dynamic *read-cache*
  "nil, or an atom from read-cache that serves each read once while it is
   bound. A read is keyed by its whole kubectl argv - context, namespace,
   type, name, selectors and output format - so it is served only to a read
   of exactly the same thing. Only a read that answered is kept: one that
   exited cleanly with output that parses, or one that answered that its
   object or resource type does not exist. A read that did not answer is
   issued again by the next caller. Concurrent reads of the same argv wait
   for the one in flight instead of issuing their own.

   The entry is the raw result of the command, and each read interprets it
   as it would a fresh one, so a strict and a lax read of the same object
   share an entry and each keeps its own contract.

   The cached readers are those built on get-json, get-json-result and the
   namespaced reads: get-json, get-json-result, get-all, get-selected,
   get-namespaced-selected, get-cluster-resource, get-namespaced, get-namespaced-result,
   get-namespaced-resource, confirmed-absent? and pods-on-node. The other
   readers always read.

   Bind it for one unit of work whose reads must agree with one another and
   need not see a change made during it, and never across a change or a
   wait for one: gumshoe.mutation binds a fresh cache for each phase of a
   change and none around its post-checks, which poll the cluster."
  nil)

(defn read-cache
  "A fresh cache for *read-cache*."
  []
  (atom {}))

(defn- did-not-answer
  "The ::unread exception a strict read throws for a kubectl get that did not
   answer."
  [exit err]
  (ex-info (format "kubectl get did not answer: %s"
                   (or (not-empty (str/trim (str err))) (str "exit " exit)))
           {::unread true}))

(defn answered?
  "Whether the result of a kubectl get answered: it exited cleanly and its
   output parses, or it answered that its object or resource type does not
   exist."
  [{:keys [exit out err]}]
  (boolean (or (and (= 0 exit) (some? (parse-json out)))
               (answered-empty? err))))

(defn- execute-read
  "The result of running the kubectl get argv, served from *read-cache* when
   it is bound and holds an answer for argv. Within one cache the first
   caller of an argv runs it and every concurrent caller waits for its
   result; a result that did not answer is dropped from the cache once
   delivered, so the next caller reads again."
  [argv]
  (if-let [cache *read-cache*]
    (let [pending (delay (apply shell/execute argv))
          entry (get (swap! cache #(if (contains? % argv) % (assoc % argv pending))) argv)
          result @entry]
      (when-not (answered? result)
        (swap! cache #(if (identical? entry (get % argv)) (dissoc % argv) %)))
      result)
    (apply shell/execute argv)))

(defn- read-result
  "{:ok? <the read answered> :doc <the parsed document>} of a kubectl get.
   :ok? is true only when kubectl exited cleanly and its output parsed. Under
   *strict-reads* a read that is not ok? and did not answer that its object
   or resource type is absent throws."
  [& argv]
  (let [{:keys [exit out err]} (execute-read argv)
        doc (parse-json out)
        ok? (and (zero? exit) (some? doc))]
    (when (and *strict-reads* (not ok?) (not (answered-empty? err)))
      (throw (did-not-answer exit err)))
    {:ok? ok? :doc doc}))

(defn- read-json
  "The parsed output of a kubectl get, nil when it failed or did not parse.
   Under *strict-reads* a read that did not answer throws."
  [& argv]
  (cond
    *strict-reads* (let [{:keys [ok? doc]} (apply read-result argv)]
                     (when ok? doc))
    *read-cache* (parse-json (:out (execute-read argv)))
    :else (parse-json (apply shell/stdout-of argv))))

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
   would have returned the same nil as a failed parse. Under *strict-reads*
   a read that did not answer throws instead of returning {:ok? false}."
  [context & args]
  (apply read-result "kubectl" (str "--context=" context) request-timeout
         "get" (concat args ["--output=json"])))

(defn get-all
  [context type]
  (get-json context type "--all-namespaces"))

(defn- not-found?
  [err]
  (boolean (re-find #"\(NotFound\)" (str err))))

(defn- discovery-documents
  "The parsed discovery documents of the results, or nil unless every one of
   them exited cleanly and parsed: a version that did not answer may be the
   one serving the type."
  [results]
  (let [docs (map #(when (zero? (:exit %)) (parse-json (:out %))) results)]
    (when (and (seq results) (every? map? docs))
      docs)))

(defn unserved-in-discovery?
  "Pure: whether discovery answered in full that a resource type is not
   served. group-result is the `kubectl get --raw /apis/<group>` result, or
   nil for the core group; version-results are the results of reading each
   version's resource list (`/apis/<group>/<version>`, or `/api/v1` for the
   core group). A group the API server answers NotFound for is not served.
   A group it lists is judged by its versions' resource lists, and only when
   every one of them answered: an aggregated API whose server is down still
   has its group listed by the kube-apiserver, while its versions answer 503.
   Anything else - a failed read, a type a version lists after all - is not
   a confirmed absence."
  [plural group-result version-results]
  (cond
    (and group-result (not (zero? (:exit group-result)))) (not-found? (:err group-result))
    :else (boolean
           (when-let [docs (discovery-documents version-results)]
             (not-any? #(= plural (:name %)) (mapcat :resources docs))))))

(defn- raw-result
  [context path]
  (execute-read ["kubectl" (str "--context=" context) request-timeout "get" "--raw" path]))

(defn type-unserved?
  "Whether the API server's discovery answers in full that type (plural or
   plural.group) is not served. A kubectl get reports a type it finds no
   mapping for as \"doesn't have a resource type\" both when the type is
   not served and when discovery of its group failed and kubectl carried on
   with what it did read, so that message alone never proves an absence."
  [context type]
  (let [[plural group] (str/split type #"\." 2)]
    (if (str/blank? group)
      (unserved-in-discovery? plural nil [(raw-result context "/api/v1")])
      (let [group-result (raw-result context (str "/apis/" group))
            versions (when (zero? (:exit group-result))
                       (keep :version (:versions (parse-json (:out group-result)))))]
        (unserved-in-discovery? plural group-result
                                (map #(raw-result context (str "/apis/" group "/" %)) versions))))))

(defn get-all-optional
  "Every object of a resource type the cluster may not serve, in every
   namespace: the parsed list, an empty list when the server answered that it
   has no such type and its discovery confirms the type is not served, or nil
   when the read did not answer. A check over a type only some clusters serve
   (a CRD, an aggregated API) can then join the read, so a failed read - a
   failed discovery of the type's group included - is unread while a cluster
   without the type answers that nothing exists."
  [context type]
  (try
    (let [{:keys [ok? doc]} (binding [*strict-reads* true]
                              (get-json-result context type "--all-namespaces"))]
      (cond
        ok? doc
        (type-unserved? context type) {:items []}
        :else nil))
    (catch clojure.lang.ExceptionInfo e
      (when-not (::unread (ex-data e))
        (throw e)))))

(defn read-all
  "{:doc <the parsed list>} of every object of type in every namespace, or
   {:unread <why>} when the read did not answer or the server has no such
   resource type. A book that reports on what the list holds reads through
   here, so a failed read is never taken for an empty cluster."
  [context type]
  (try
    (let [{:keys [ok? doc]} (binding [*strict-reads* true]
                              (get-json-result context type "--all-namespaces"))]
      (if ok?
        {:doc doc}
        {:unread (format "the server has no %s resource" type)}))
    (catch clojure.lang.ExceptionInfo e
      (if (::unread (ex-data e))
        {:unread (ex-message e)}
        (throw e)))))

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
  "Every object of the status-carrying CRDs not in covered as a list document
   {:items [..] :unread [..]}, or nil when the CRDs cannot be listed. Each
   type is read on its own, so one type the caller may not list leaves the
   others readable; the types whose read did not answer are named under
   :unread, so a reader can tell them from types without objects."
  [context covered]
  (when-let [crds (get-all context "customresourcedefinitions")]
    (let [types (status-types crds covered)
          reads (map vector types (concurrent/map-all #(get-all context %) types
                                                      {:parallelism concurrent/default-parallelism}))]
      {:items (vec (mapcat (comp :items second) reads))
       :unread (vec (for [[type result] reads :when (nil? result)] type))})))

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

(def ^:private helm-listing-jsonpath
  (str "--output=jsonpath={range .items[*]}"
       "{.metadata.namespace}{\"\\t\"}{.metadata.labels.name}{\"\\t\"}"
       "{.metadata.labels.status}{\"\\t\"}{.metadata.labels.version}{\"\\t\"}"
       "{.metadata.labels.modifiedAt}{\"\\t\"}{.metadata.labels.createdAt}{\"\\t\"}"
       "{.metadata.creationTimestamp}{\"\\n\"}{end}"))

(defn helm-releases
  "Every revision Helm stores as a Secret, read from its labels, or nil when
   they cannot be listed; under *strict-reads* a listing that did not answer
   throws instead. The jsonpath selects metadata only, so the release
   payload (rendered manifests and values, which may hold credentials) never
   reaches gumshoe.

   With a namespace, only the revisions stored in that namespace are listed;
   with a release too, only that release's, selected by Helm's name label on
   the server. A book that knows where a release is stored reads that way: the
   API server sends back a handful of Secrets instead of every revision on the
   cluster, and the read needs list rights on Secrets in that one namespace
   only."
  ([context]
   (helm-releases context nil nil))
  ([context namespace]
   (helm-releases context namespace nil))
  ([context namespace release]
   (let [{:keys [exit out err]} (shell/execute "kubectl" (str "--context=" context) request-timeout
                                               "get" "secrets"
                                               (if namespace (str "--namespace=" namespace) "--all-namespaces")
                                               (str "--selector=owner=helm" (when release (str ",name=" release)))
                                               "--field-selector=type=helm.sh/release.v1"
                                               helm-listing-jsonpath)]
     (when (and *strict-reads* (not (zero? exit)))
       (throw (did-not-answer exit err)))
     (when (zero? exit)
       (parse-helm-listing out)))))

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
  "Every resource of a type matching a label selector, in every namespace."
  [context type selector]
  (get-json context type "--all-namespaces" (str "--selector=" selector)))

(defn get-namespaced-selected
  "Every resource of a type matching a label selector within one namespace. A
   read that only concerns one namespace asks for that namespace, so a user
   whose RBAC is a RoleBinding there can answer it where a cluster-scope list
   is refused."
  [context namespace type selector]
  (get-json context type (str "--namespace=" namespace) (str "--selector=" selector)))

(defn get-cluster-resource
  [context type name]
  (get-json context type name))

(defn get-namespaced-resource
  [context namespace type name]
  (read-json "kubectl" (str "--context=" context) (str "--namespace=" namespace) request-timeout
             "get" type name "--output=json"))

(defn presence
  "Whether the object a ref {:type :namespace :name} names exists, read
   strictly: :present, :absent when the read answered and found no such
   object, or :unread when it did not answer, which the plain getters report
   as the same nil as NotFound. namespace is nil for a cluster-scoped type."
  [context {:keys [type namespace name]}]
  (let [{:keys [exit out]} (apply shell/execute "kubectl" (str "--context=" context) request-timeout
                                  (concat (when namespace [(str "--namespace=" namespace)])
                                          ["get" type name "--ignore-not-found" "--output=name"]))]
    (cond
      (not (zero? exit)) :unread
      (str/blank? out) :absent
      :else :present)))

(defn confirmed-gone?
  "True ONLY when a read answered and found no such object - the fail-safe way
   to verify a deletion. A failed or unreachable read returns false, where the
   plain getters return the same nil for a failure as for NotFound. namespace
   is nil for a cluster-scoped type."
  [context namespace type name]
  (= :absent (presence context {:type type :namespace namespace :name name})))

(defn namespace-objects
  "Every object left in `namespace`, read across each namespaced type the
   cluster lists: {:ok? true :objects [\"<kind>/<name>\" ..]} when discovery
   and the listing both answered in full, else {:ok? false :reason <why>}.
   kubectl api-resources exits non-zero while still printing the groups it
   did read when one aggregated API is unavailable; that counts as unread,
   because the unavailable API may serve exactly the objects in question.
   Ignores *strict-reads*: the caller judges :ok? itself."
  [context namespace]
  (let [discovery (shell/execute "kubectl" (str "--context=" context) request-timeout
                                 "api-resources" "--namespaced=true" "--verbs=list" "--output=name")
        types (remove str/blank? (map str/trim (str/split-lines (str (:out discovery)))))
        failure (fn [what {:keys [exit err]}]
                  (format "%s did not answer: %s" what
                          (or (not-empty (str/trim (str err))) (str "exit " exit))))]
    (cond
      (not (zero? (:exit discovery)))
      {:ok? false :reason (failure "the discovery of namespaced types" discovery)}

      (empty? types)
      {:ok? false :reason "the discovery of namespaced types listed none"}

      :else
      (let [listing (shell/execute "kubectl" (str "--context=" context) (str "--namespace=" namespace)
                                   request-timeout "get" (str/join "," types) "--output=name")]
        (if (zero? (:exit listing))
          {:ok? true :objects (vec (remove str/blank? (map str/trim (str/split-lines (str (:out listing))))))}
          {:ok? false :reason (failure (str "the listing of namespace " namespace) listing)})))))

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

(defn exec-answer
  "The trimmed stdout of a command run in a pod's container, or nil unless it
   exited 0. A command the container lacks, a failed exec and a command that
   failed after printing part of its output are all unanswered, where
   exec-stdout returns whatever reached stdout."
  [context namespace pod container command]
  (let [{:keys [exit out]} (apply shell/execute "kubectl" (str "--context=" context)
                                  (str "--namespace=" namespace) request-timeout
                                  "exec" pod "-c" container "--" command)]
    (when (= 0 exit)
      (str/trim (str out)))))

(defn merge-patch-args
  "The kubectl arguments after --context that merge-patch a namespaced
   resource with patch, a map encoded as JSON. patch! runs them, and an effect
   plan hands them to gumshoe.effect/kubectl, so a planned patch and a direct
   one run the same command."
  [namespace type name patch]
  [(str "--namespace=" namespace) "patch" type name "--type=merge"
   (str "--patch=" (cheshire/generate-string patch))])

(defn patch!
  "Merge-patches a resource. Returns the full process map."
  [context namespace type name patch]
  (apply shell/execute "kubectl" (str "--context=" context) (merge-patch-args namespace type name patch)))

(defn- port-open?
  [port]
  (try
    (with-open [_socket (java.net.Socket. "127.0.0.1" (int port))]
      true)
    (catch Exception _ false)))

(defn bind-failure
  "Why a bind to port failed, from the BindException's message: :in-use when
   something holds the port, :permission-denied when this user may not bind
   it, or :other. The JDK raises BindException for EADDRINUSE, EACCES and
   EADDRNOTAVAIL alike and tells them apart only by the message, whose
   wording differs between runtimes: the JVM says \"Address already in use\",
   babashka's native image \"Address in use\". A message
   naming neither cause on a port below 1024 counts as denied, since that is
   what an unprivileged user meets there."
  [port message]
  (let [message (str message)]
    (cond
      (re-find #"(?i)\bin use\b" message) :in-use
      (re-find #"(?i)permission denied|not permitted" message) :permission-denied
      (and (int? port) (< port 1024)) :permission-denied
      :else :other)))

(defn- try-bind!
  "Binds port on 127.0.0.1 and releases it at once; throws when it cannot."
  [port]
  (with-open [_socket (java.net.ServerSocket. (int port) 1 (java.net.InetAddress/getByName "127.0.0.1"))]
    true))

(defn- port-bindable?
  "Whether this process could bind port on 127.0.0.1 right now. A program
   holding the port without accepting connections is caught here, where a
   connection attempt would miss it. Only a bind that failed because the
   address is in use means the port is held. A bind refused for permission
   throws ex-info with :reason :permission-denied, and any other failure is
   thrown as itself, so neither is reported as an occupied port."
  [port]
  (try
    (try-bind! port)
    (catch java.net.BindException e
      (case (bind-failure port (ex-message e))
        :in-use false
        :permission-denied (throw (ex-info (str "permission denied for port " port)
                                           {:reason :permission-denied :port port}
                                           e))
        (throw e)))))

(defn valid-local-port?
  "Whether port can be a local port to forward to: 1 to 65535. Port 0 asks
   the kernel for any port, which kubectl would choose and report on its
   own, so it is refused as well; leaving --port out picks a free port."
  [port]
  (and (int? port) (<= 1 port 65535)))

(defn local-port-message
  "The refusal for a local port outside 1-65535."
  [port]
  (format "--port must be between 1 and 65535, got %s - leave it out to use a free port" (pr-str port)))

(def local-port-spec
  "The spec of the --port option of a book that forwards a service to a
   local port, refusing a port outside 1-65535 as the arguments are parsed."
  {:desc "The local port used for the port-forward (1-65535) - a free port when omitted"
   :alias :p
   :coerce :long
   :validate {:pred valid-local-port?
              :ex-msg (fn [{:keys [value]}] (local-port-message value))}})

(defn local-port-taken?
  "Whether anything holds port on 127.0.0.1: a listener or a bind."
  [port]
  (or (port-open? port) (not (port-bindable? port))))

(defn local-port-problem
  "Why with-port-forward may not use port, or nil: a port outside 1-65535,
   one something already holds, one this user may not bind, or a check that
   failed for another reason, named as itself."
  [port]
  (if-not (valid-local-port? port)
    (local-port-message port)
    (try
      (when (local-port-taken? port)
        (format "local port %d is already in use - pick another with --port" port))
      (catch Exception e
        (if (= :permission-denied (:reason (ex-data e)))
          (format "permission denied for port %d - this user may not bind it (ports below 1024 usually need root); pick another with --port" port)
          (format "local port %d could not be checked: %s" port (or (ex-message e) (str e))))))))

(defn free-local-port
  "A port on 127.0.0.1 nothing holds right now, chosen by the kernel from the
   ephemeral range. Another program can take it before kubectl binds it;
   with-port-forward then refuses instead of running against that program,
   because kubectl never reports it as forwarded."
  []
  (with-open [socket (java.net.ServerSocket. 0 1 (java.net.InetAddress/getByName "127.0.0.1"))]
    (.getLocalPort socket)))

(defn local-port-or-free
  "The local port an operator asked for, or a free one when they asked for
   none, so concurrent books never share a default port."
  [port]
  (or port (free-local-port)))

(defn forwarded-port
  "The local port a line of kubectl port-forward's output reports as bound on
   127.0.0.1, or nil. kubectl prints the line only once its listener is bound,
   and binds 127.0.0.1 and ::1 separately: when 127.0.0.1 is taken by another
   program it still forwards ::1 and prints only the ::1 line."
  [line]
  (some-> (re-find #"^Forwarding from 127\.0\.0\.1:(\d+) -> " (str line)) second parse-long))

(def ^:dynamic *port-forward*
  "The kubectl port-forward process with-port-forward runs f under, so a step
   inside f can check that the forward it talks through is still ours."
  nil)

(defn port-forward-alive?
  "Whether the port-forward f runs under is still running. Once it exits, the
   local port is free for any program to take, so a later request to it may
   reach somebody else's service. False outside with-port-forward."
  []
  (boolean (some-> *port-forward* process/alive?)))

(defn- watch-forward-output
  "Copies each line kubectl port-forward prints to stderr and delivers :bound
   to bound once kubectl reports local-port forwarded on 127.0.0.1, or :exited
   when its output ends first. Our stdout carries the book's result (--output
   json), so the per-connection chatter goes to stderr; it is read
   continuously, since an unread pipe fills up and blocks the forward."
  [forward local-port bound]
  (doto (Thread.
         ^Runnable
         (fn []
           (try
             (with-open [reader (java.io.BufferedReader. (java.io.InputStreamReader. ^java.io.InputStream (:out forward)))]
               (loop []
                 (when-let [line (.readLine reader)]
                   (binding [*out* *err*] (println line) (flush))
                   (when (= local-port (forwarded-port line)) (deliver bound :bound))
                   (recur))))
             (catch Exception _ nil)
             (finally (deliver bound :exited)))))
    (.setDaemon true)
    (.start)))

(def ^:dynamic *forward-start-ms*
  "How long kubectl port-forward may take to report its listener bound."
  30000)

(def ^:dynamic *forward-exit-ms*
  "How long a stopped kubectl port-forward may take to exit and release its
   local port before the port counts as still held by that forward."
  10000)

(defonce ^:private lingering-forwards
  ;; local port -> our kubectl port-forward that was stopped but had not
  ;; exited within *forward-exit-ms*, so a later forward on the same port
  ;; can tell our own leftover apart from another program.
  (atom {}))

(defn- exited-within?
  "Whether forward's process exited, waiting at most ms for it."
  [forward ms]
  (.waitFor ^Process (:proc forward) (long ms) java.util.concurrent.TimeUnit/MILLISECONDS))

(defn- stop-forward!
  "Takes forward down and waits for it to exit, so the next forward on
   local-port can bind it at once: kubectl releases the port only when it
   exits, and destroy-tree only signals it. A forward still running after
   *forward-exit-ms* is remembered as holding local-port and stays tracked,
   so the JVM's shutdown still takes it down."
  [forward resource local-port]
  (process/destroy-tree forward)
  (if (exited-within? forward *forward-exit-ms*)
    (do (swap! lingering-forwards (fn [m] (if (identical? forward (get m local-port)) (dissoc m local-port) m)))
        (shell/untrack-child! forward))
    (do (swap! lingering-forwards assoc local-port forward)
        (stdout/error (format (str "kubectl port-forward to %s did not exit within %d seconds of being stopped;"
                                   " local port %d is still held by our own forward")
                              resource (quot *forward-exit-ms* 1000) local-port)))))

(defn- own-forward-problem
  "Why local-port cannot be used yet because our own earlier forward still
   holds it, or nil. That forward gets another *forward-exit-ms* to exit."
  [local-port]
  (when-let [forward (get @lingering-forwards local-port)]
    (if (exited-within? forward *forward-exit-ms*)
      (do (swap! lingering-forwards dissoc local-port)
          (shell/untrack-child! forward)
          nil)
      (format (str "local port %d is still held by our own earlier kubectl port-forward, which has not exited"
                   " since it was stopped - leave --port out to use a free port")
              local-port))))

(defn- forward-and-run
  "with-port-forward once local-port passed its checks."
  [context namespace resource local-port remote-port f]
  (let [forward (try
                  ;; shell/start takes the options first and the
                  ;; command as separate strings; a vector after the
                  ;; options would be run as one program named "[kubectl".
                  ;; Tracked, so a SIGTERM that halts the JVM before the
                  ;; finally below runs still takes the forward down
                  ;; instead of leaving it holding local-port.
                  (shell/track-child!
                   (apply shell/start {:out :stream :err :inherit}
                          ["kubectl" (str "--context=" context) (str "--namespace=" namespace)
                           "port-forward" resource
                           (str local-port ":" remote-port)]))
                  (catch Exception e
                    (stdout/error (format "could not start kubectl port-forward: %s" (ex-message e)))
                    nil))]
    (if (nil? forward)
      false
      (try
        (let [bound (promise)
              deadline (+ (System/currentTimeMillis) *forward-start-ms*)]
          (watch-forward-output forward local-port bound)
          (loop []
            (cond
              (not (process/alive? forward))
              (do (stdout/error (format "kubectl port-forward to %s exited before the port opened" resource))
                  false)

              (and (= :bound (deref bound 0 nil)) (port-open? local-port) (process/alive? forward))
              (let [result (binding [*port-forward* forward] (f))]
                (if (process/alive? forward)
                  result
                  (do (stdout/error (format (str "kubectl port-forward to %s exited while it was in use;"
                                                 " what went through local port %d afterwards may have reached another program")
                                            resource local-port))
                      false)))

              (> (System/currentTimeMillis) deadline)
              (do (stdout/error (format "port-forward to %s did not report 127.0.0.1:%d bound within %d seconds"
                                        resource local-port (quot *forward-start-ms* 1000)))
                  false)

              :else (do (Thread/sleep 100)
                        (recur)))))
        (finally
          (stop-forward! forward resource local-port))))))

(defn with-port-forward
  "Runs f while the service's remote-port is forwarded to local-port, then
   tears the forward down no matter what f does. f runs only against our own
   forward: a local-port outside 1-65535 or already taken is refused before
   kubectl starts, f
   starts only once kubectl itself reported 127.0.0.1:local-port bound and is
   still running, and a forward that exited while f ran turns f's result into
   a failure, since its port may by then belong to another program. A step
   inside f that talks to the port repeatedly checks port-forward-alive?.
   The forward has exited and released local-port when with-port-forward
   returns, so the next call may reuse the port; one that does not exit
   within *forward-exit-ms* is reported, and a later call on its port is
   refused as held by our own forward.

   The forward goes to :service, through whichever pod kubectl picks behind
   it, or to the one :pod when that is given."
  [{:keys [context namespace service pod local-port remote-port]} f]
  (let [resource (if pod (str "pod/" pod) (str "service/" service))]
    (cond
      (nil? remote-port)
      (do (stdout/error (format "%s has no resolvable port to forward" resource)) false)

      (nil? local-port)
      (do (stdout/error "no local port to forward to - pass one with --port") false)

      :else
      (if-let [problem (or (when (valid-local-port? local-port) (own-forward-problem local-port))
                           (local-port-problem local-port))]
        (do (stdout/error problem) false)
        (forward-and-run context namespace resource local-port remote-port f)))))

(defn unused-pvc-candidates
  "Namespace/name of every PersistentVolumeClaim no pod refers to. Throws
   when the pods could not be read: without them every claim would look
   unused. Pods are not the only holders of a claim (a StatefulSet scaled to
   0, a CronJob between runs, an owner), so a book that offers claims for
   deletion uses gumshoe.claims/unused-candidates instead."
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
   claim `name`. Before deleting a claim, judge it with
   gumshoe.cleanup/claim-refusal, which also asks the workloads and owners
   that keep it."
  [context namespace name]
  (confirmed-absent? context namespace "pods"
                     #(contains? (claimed-pvcs {:items [%]}) (format "%s/%s" namespace name))))
