;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.prometheus.targets
  "The scrape targets of every Prometheus the prometheus-operator runs, read
   from each shard's own /api/v1/targets.

   A sharded Prometheus splits its targets across shards, and each shard
   answers /api/v1/targets for its own targets only. Every shard the
   Prometheus object asks for (spec.shards, at least one) is therefore read,
   and a shard that did not answer is listed as :unread with the reason. A
   shard left out would shrink the result silently, and an incomplete sweep
   would read as a clean one.

   The replicas of one shard scrape the same targets, so one ready replica
   per shard is read; when it does not answer, the next ready replica is
   tried. A replica that is not ready is never read: until it is ready its
   target list may still be filling.

   Each read goes through the API server's pod proxy as a GET
   (`kubectl get --raw /api/v1/namespaces/<ns>/pods/<scheme>:<pod>:<port>/proxy<route>/api/v1/targets?state=active`).
   It needs get on pods/proxy and no local port, and kubectl's request
   timeout bounds it.

   Collection (collect, with every read injected) is apart from the pure
   halves (expected-shards, classify-answer, shard-path), so each branch is
   tested without a cluster."
  (:require [clojure.string :as str]
            [gumshoe.conditions :as conditions]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.reads :as reads]))

(def evidence-key "prometheus-scrape-targets")

(def prometheus-type "prometheuses.monitoring.coreos.com")

(def name-label
  "The label prometheus-operator puts on every pod of a Prometheus, valued
   with the Prometheus' name."
  :operator.prometheus.io/name)

(def shard-label
  "The label prometheus-operator puts on every pod of a Prometheus, valued
   with the pod's shard number."
  :operator.prometheus.io/shard)

(def pods-selector
  "Every pod prometheus-operator runs for a Prometheus or a PrometheusAgent;
   pods-of-shard keeps the Prometheus ones."
  "operator.prometheus.io/name")

(def default-port-name "web")

(defn shard-count
  "The shards the operator runs for prometheus: spec.shards, and one when it
   is unset or below one."
  [prometheus]
  (let [n (-> prometheus :spec :shards)]
    (if (and (integer? n) (> n 1)) n 1)))

(defn replica-count
  "The replicas per shard: spec.replicas, one when unset, zero when scaled
   down."
  [prometheus]
  (let [n (-> prometheus :spec :replicas)]
    (cond (nil? n) 1
          (and (integer? n) (>= n 0)) n
          :else 1)))

(defn- prometheus-ref [prometheus] (kubectl/namespace-name-of prometheus))

(defn statefulset-name
  "The StatefulSet the operator runs for one shard: prometheus-<name> for
   shard 0, prometheus-<name>-shard-<n> for the others."
  [prometheus shard]
  (let [base (str "prometheus-" (kubectl/name-of prometheus))]
    (if (zero? shard) base (format "%s-shard-%d" base shard))))

(defn expected-shards
  "One row per shard prometheus asks for: its namespace, name, shard number
   and StatefulSet."
  [prometheus]
  (vec (for [shard (range (shard-count prometheus))]
         {:prometheus (prometheus-ref prometheus)
          :namespace (kubectl/namespace-of prometheus)
          :name (kubectl/name-of prometheus)
          :shard shard
          :statefulset (statefulset-name prometheus shard)})))

(defn pods-of-shard
  "The pods of one shard among pods: in the Prometheus' namespace, labelled
   with its name and the shard number, and owned by the shard's StatefulSet
   by name (prometheus-<name>[-shard-<n>]-<ordinal>), which leaves a
   PrometheusAgent's prom-agent-* pods of the same name out. Sorted by name."
  [{:keys [namespace name shard statefulset]} pods]
  (let [pattern (re-pattern (str "\\Q" statefulset "\\E-\\d+"))]
    (->> pods
         (filter #(and (= namespace (kubectl/namespace-of %))
                       (= name (get-in % [:metadata :labels name-label]))
                       (= (str shard) (get-in % [:metadata :labels shard-label]))
                       (re-matches pattern (str (kubectl/name-of %)))))
         (sort-by kubectl/name-of)
         vec)))

(defn ready?
  [pod]
  (and (= "Running" (-> pod :status :phase)) (conditions/ready? pod)))

(defn api-port
  "The port the pod's prometheus container serves its API on: the container
   port named port-name. nil when there is none."
  [pod port-name]
  (->> (-> pod :spec :containers)
       (filter #(= "prometheus" (:name %)))
       (mapcat :ports)
       (filter #(= port-name (:name %)))
       first
       :containerPort))

(def ^:private dns-label #"[a-z0-9]([-a-z0-9]*[a-z0-9])?")

(def ^:private route-segment #"[A-Za-z0-9._~-]+")

(defn route-prefix
  "spec.routePrefix as a path prefix without a trailing slash (\"\" for the
   default \"/\"), or nil when it holds anything but plain path segments. The
   prefix goes into an API server path, where a `..` segment or a `?` would
   lead the read away from the pod's API, so it is refused rather than
   escaped."
  [prometheus]
  (let [prefix (or (-> prometheus :spec :routePrefix) "/")
        segments (remove str/blank? (str/split (str prefix) #"/"))]
    (when (and (string? prefix)
               (str/starts-with? prefix "/")
               (every? #(and (re-matches route-segment %) (not (#{"." ".."} %))) segments))
      (apply str (map #(str "/" %) segments)))))

(defn shard-path
  "The API server path that GETs pod's active targets through the pod proxy,
   as {:path p} or {:reason why} when it cannot be built safely: a pod or
   namespace name that is not a DNS label, no API port, or a route prefix
   that is not plain path segments."
  [prometheus pod]
  (let [namespace (kubectl/namespace-of pod)
        pod-name (kubectl/name-of pod)
        port-name (or (not-empty (-> prometheus :spec :portName)) default-port-name)
        port (api-port pod port-name)
        prefix (route-prefix prometheus)
        scheme (if (some? (-> prometheus :spec :web :tlsConfig)) "https" "http")]
    (cond
      (not (and (string? namespace) (re-matches dns-label namespace)
                (string? pod-name) (re-matches dns-label pod-name)))
      {:reason "the pod's name or namespace is not a DNS label"}

      (not (and (integer? port) (<= 1 port 65535)))
      {:reason (format "its prometheus container has no port named %s" port-name)}

      (nil? prefix)
      {:reason "spec.routePrefix is not a plain path, so no request is built from it"}

      :else
      {:path (format "/api/v1/namespaces/%s/pods/%s:%s:%d/proxy%s/api/v1/targets?state=active"
                     namespace scheme pod-name port prefix)})))

(defn- first-line
  [s]
  (let [line (first (remove str/blank? (str/split-lines (str s))))]
    (if (str/blank? line) "kubectl gave no reason" (str/trim line))))

(defn- target-row
  [target]
  {:scrape-pool (:scrapePool target)
   :scrape-url (:scrapeUrl target)
   :health (:health target)
   :last-error (:lastError target)
   :labels (:labels target)})

(defn classify-answer
  "Pure: what the raw result {:exit :out :err} of one /api/v1/targets read
   proves: {:state :read :targets [..]} when kubectl exited 0 and the body is
   a successful targets answer, {:state :unread :reason why} otherwise. A
   read that answered with no active targets is :read with none; anything
   that is not that answer is never taken for it.

   Each target keeps its pool, URL, health, lastError and labels.
   discoveredLabels is dropped: it carries every pod annotation the
   discovery saw, which is more than a finding needs."
  [{:keys [exit out err]}]
  (if (not= 0 exit)
    {:state :unread :reason (first-line err)}
    (let [doc (kubectl/parse-json out)
          active (get-in doc [:data :activeTargets])]
      (cond
        (not (map? doc)) {:state :unread :reason "the answer does not parse as JSON"}
        (not= "success" (:status doc)) {:state :unread :reason (format "the answer's status is %s" (pr-str (:status doc)))}
        (not (sequential? active)) {:state :unread :reason "the answer holds no activeTargets list"}
        :else {:state :read :targets (mapv target-row (filter map? active))}))))

(defn- read-shard
  "One shard's row once its pods are known: the first ready pod that
   answers is read; every pod tried is listed under :attempts with why it
   did not answer."
  [prometheus row pods read-path]
  (let [ready-pods (filter ready? pods)]
    (cond
      (zero? (replica-count prometheus))
      (assoc row :state :scaled-down)

      (empty? pods)
      (assoc row :state :unread :reason (format "no pod of StatefulSet %s exists" (:statefulset row)))

      (empty? ready-pods)
      (assoc row :state :unread
             :reason (format "none of its pods is ready (%s)" (str/join ", " (map kubectl/name-of pods))))

      :else
      (loop [[pod & more] ready-pods attempts []]
        (if (nil? pod)
          (assoc row :state :unread :attempts attempts
                 :reason (format "no ready pod answered (%s)"
                                 (str/join "; " (map #(str (:pod %) ": " (:reason %)) attempts))))
          (let [{:keys [path reason]} (shard-path prometheus pod)
                answer (if path (classify-answer (read-path path)) {:state :unread :reason reason})]
            (if (= :read (:state answer))
              (assoc row :state :read :pod (kubectl/name-of pod) :attempts attempts
                     :targets (mapv #(assoc % :prometheus (:prometheus row) :shard (:shard row)) (:targets answer)))
              (recur more (conj attempts {:pod (kubectl/name-of pod) :reason (:reason answer)})))))))))

(defn dedupe-targets
  "targets with one row per Prometheus, pool, URL and label set, the first
   kept: replicas of a shard report the same targets."
  [targets]
  (first (reduce (fn [[kept seen] t]
                   (let [k [(:prometheus t) (:scrape-pool t) (:scrape-url t) (:labels t)]]
                     (if (contains? seen k) [kept seen] [(conj kept t) (conj seen k)])))
                 [[] #{}]
                 targets)))

(defn collect
  "The evidence: {:shards [..] :targets [..]}, or nil when the Prometheus
   objects could not be listed. reads holds the injected reads:

     :prometheuses (fn []) -> a gumshoe.reads list result of the Prometheus objects
     :pods         (fn []) -> a gumshoe.reads list result of their pods
     :targets      (fn [path]) -> the raw {:exit :out :err} of a GET of path

   Every shard the Prometheus objects ask for gets a row: :read with the pod
   that answered, :unread with the reason, or :scaled-down when the
   Prometheus runs no replica. When the pods cannot be listed every shard is
   :unread, since none can be read without them. Targets come only from
   :read shards, with their Prometheus and shard."
  [{:keys [prometheuses pods targets]}]
  (let [listed (prometheuses)]
    (when (reads/answered? listed)
      (let [objects (sort-by kubectl/namespace-name-of (reads/items! listed))
            pod-list (when (seq objects) (pods))
            rows (vec (for [prometheus objects
                            row (expected-shards prometheus)]
                        (if (reads/answered? pod-list)
                          (read-shard prometheus row (pods-of-shard row (reads/items! pod-list)) targets)
                          (assoc row :state :unread
                                 :reason (str "the Prometheus pods could not be listed: "
                                              (or (:reason pod-list) "the read did not answer"))))))]
        {:shards (mapv #(dissoc % :targets) rows)
         :targets (dedupe-targets (mapcat :targets rows))}))))

(defn collector
  "The evidence collector, for registration through the plugin seam."
  [context _key]
  (collect {:prometheuses #(reads/listing context {:type prometheus-type :namespace :all})
            :pods #(reads/listing context {:type "pods" :namespace :all :selector pods-selector})
            :targets #(kubectl/get-answer context "--raw" %)}))
