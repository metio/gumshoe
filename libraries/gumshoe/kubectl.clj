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

(defn- parse
  "Parses kubectl JSON output. Empty output (a missing CRD, a failed command)
   is nil, and truncated or malformed output is nil rather than an exception -
   so one unreadable resource type degrades to no-evidence instead of
   crashing a whole investigation."
  [json-string]
  (try
    (cheshire/parse-string json-string true)
    (catch Exception _ nil)))

(defn- current-config
  []
  (parse (shell/stdout-of "kubectl" "config" "view" "--raw" "--output" "json")))

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

;; A bounded request timeout on every read means a wedged API server slows a
;; book down by seconds, never hangs it forever. It applies only to these
;; quick get calls - long-running work (port-forward, drain, exec, watches,
;; interactive sessions) streams through gumshoe.shell with no timeout and keeps
;; running for as long as it needs.
(def ^:private request-timeout "--request-timeout=30s")

(defn get-all
  [context type]
  (parse (shell/stdout-of "kubectl" (str "--context=" context) request-timeout
                          "get" type "--all-namespaces" "--output=json")))

(defn get-selected
  [context type selector]
  (parse (shell/stdout-of "kubectl" (str "--context=" context) request-timeout
                          "get" type "--all-namespaces" (str "--selector=" selector) "--output=json")))

(defn get-cluster-resource
  [context type name]
  (parse (shell/stdout-of "kubectl" (str "--context=" context) request-timeout
                          "get" type name "--output=json")))

(defn get-namespaced-resource
  [context namespace type name]
  (parse (shell/stdout-of "kubectl" (str "--context=" context) (str "--namespace=" namespace) request-timeout
                          "get" type name "--output=json")))

(defn pods-on-node
  [context node]
  (parse (shell/stdout-of "kubectl" (str "--context=" context) request-timeout
                          "get" "pods" "--all-namespaces"
                          (str "--field-selector=spec.nodeName=" node)
                          "--output=json")))

(defn get-namespaced
  "Every resource of a type within one namespace."
  [context namespace type]
  (parse (shell/stdout-of "kubectl" (str "--context=" context) (str "--namespace=" namespace) request-timeout
                          "get" type "--output=json")))

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
                    (process/process ["kubectl" (str "--context=" context) (str "--namespace=" namespace)
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
