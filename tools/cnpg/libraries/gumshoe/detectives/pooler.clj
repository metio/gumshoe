;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.pooler
  "Detectives for CloudNativePG Poolers: a pooler whose Service resolves to no
   ready address, a pooler left behind by an instance rebuild, a rollout that
   never finished, and pooler pods with no resources.

   A Pooler's PgBouncer pods cache each role's SCRAM verifier from the moment a
   client logged in, and nothing refreshes that cache. Any verifier re-derived
   on the server afterwards leaves the pooler authenticating with the old one,
   so the next backend login fails with `password authentication failed` while
   the same credential works against the cluster directly. An instance
   replacement is one way to reach that state; a password re-applied by whatever
   manages the role is another, and needs no new instances, so a pooler older
   than its instances is a symptom of one cause rather than the condition
   itself. The log detective covers the cases this structural check cannot see.

   Pod age alone does not prove it, so the finding reports the structural fact
   and names the two signatures that tell the causes apart: `password
   authentication failed` is this, while `connect failed` is the backend being
   unreachable and has nothing to do with the pooler. An unhealthy cluster is
   reported by the cnpg detectives instead, because a stale pooler cannot be
   told from an unreachable one until the cluster itself is well."
  (:require [clojure.string :as str]
            [gumshoe.cnpg.instances :as inst]
            [gumshoe.conditions :as conditions]
            [gumshoe.endpointslices :as endpointslices]
            [gumshoe.kubectl :as kubectl]))

(def pooler-type "poolers.postgresql.cnpg.io")
(def cluster-type "clusters.postgresql.cnpg.io")
(def service-type "services")
(def endpointslice-type "endpointslices")

(def ^:private healthy-phase "Cluster in healthy state")

(def ^:private template-hash-label :pod-template-hash)

(def ^:private labels-of inst/labels-of)

(defn- created-at
  [pod]
  (some-> pod :metadata :creationTimestamp java.time.Instant/parse))

(defn- pooler-pods
  [pods namespace pooler]
  (filter #(and (= namespace (kubectl/namespace-of %))
                (= pooler (get (labels-of %) inst/pooler-name-label)))
          pods))

(defn- ready?
  [pod]
  (conditions/ready? pod))

(defn- earliest
  [instants]
  (when (seq instants) (reduce #(if (.isBefore %2 %1) %2 %1) instants)))

(defn- cluster-of [pooler] (-> pooler :spec :cluster :name))

(defn- roll-command
  [pooler]
  (format "kubectl -n %s rollout restart deployment/%s"
          (kubectl/namespace-of pooler) (kubectl/name-of pooler)))

(defn- without-resources?
  [pod]
  (some (fn [container]
          (let [resources (:resources container)]
            (or (empty? (:requests resources)) (empty? (:limits resources)))))
        (-> pod :spec :containers)))

(defn- healthy?
  "A cluster in its healthy phase with every instance ready. While a cluster is
   unwell its poolers cannot connect for reasons of their own, so pooler age
   says nothing actionable and the cnpg detectives carry the report."
  [cluster]
  (and cluster
       (= healthy-phase (-> cluster :status :phase))
       (let [instances (-> cluster :spec :instances)
             ready (-> cluster :status :readyInstances)]
         ;; absent counts suppress rather than report: a detective that cannot
         ;; see the cluster's health must not claim its poolers are at fault
         (and instances ready (= ready instances)))))

(defn- cluster-named
  [clusters namespace name]
  (first (filter #(and (= namespace (kubectl/namespace-of %)) (= name (kubectl/name-of %)))
                 clusters)))

(defn detect-stale-poolers
  "Pooler pods created before every instance pod of their cluster, which is what
   a rebuild leaves behind. Quiet while the cluster itself is unhealthy."
  [evidence]
  (let [pods (kubectl/items-of (get evidence "pods"))
        clusters (kubectl/items-of (get evidence cluster-type))]
    (for [pooler (kubectl/items-of (get evidence pooler-type))
          :let [namespace (kubectl/namespace-of pooler)
                cluster (cluster-of pooler)
                instances (keep created-at (inst/instance-pods pods namespace cluster))
                ;; the OLDEST instance pod: every instance must postdate the
                ;; pooler, which is a rebuild. Against the newest, any single
                ;; instance replacement - a failover, a reschedule - would fire.
                oldest (earliest instances)
                stale (when oldest
                        (filter #(when-let [created (created-at %)] (.isBefore created oldest))
                                (pooler-pods pods namespace (kubectl/name-of pooler))))]
          :when (and (seq stale) (healthy? (cluster-named clusters namespace cluster)))]
      {:severity :warning
       :component (kubectl/namespace-name-of pooler)
       :summary (format "%d of its pods predate every instance of cluster %s"
                        (count stale) cluster)
       :hint (str "read the pooler log before acting: `password authentication failed` "
                  "is a cached verifier the pooler never refreshed, cleared by `"
                  (roll-command pooler)
                  "` until the next re-derivation; `connect failed` is the backend "
                  "being unreachable")})))

(defn detect-unfinished-rollouts
  "Ready pooler pods from more than one generation: a rollout whose old pods
   have not drained, so the Service fans out across both. Session-mode poolers
   do not drain while clients hold their sessions, so this can persist."
  [evidence]
  (let [pods (kubectl/items-of (get evidence "pods"))]
    (for [pooler (kubectl/items-of (get evidence pooler-type))
          :let [ready (filter ready? (pooler-pods pods (kubectl/namespace-of pooler)
                                                  (kubectl/name-of pooler)))
                generations (set (keep #(get (labels-of %) template-hash-label) ready))]
          :when (> (count generations) 1)]
      {:severity :warning
       :component (kubectl/namespace-name-of pooler)
       :summary (format "%d ready pods across %d generations"
                        (count ready) (count generations))
       :hint (str "the Service load-balances over both, so a fault in one shows "
                  "up intermittently - check the EndpointSlice, not pod readiness")})))

(defn detect-poolers-without-resources
  "Pooler pods with a container missing requests or limits. The operator's
   generated pod template carries none unless the Pooler spec sets them, which
   leaves PgBouncer in BestEffort QoS, first to be evicted under node pressure."
  [evidence]
  (let [pods (kubectl/items-of (get evidence "pods"))]
    (for [pooler (kubectl/items-of (get evidence pooler-type))
          :let [unbounded (filter without-resources?
                                  (pooler-pods pods (kubectl/namespace-of pooler)
                                               (kubectl/name-of pooler)))]
          :when (seq unbounded)]
      {:severity :info
       :component (kubectl/namespace-name-of pooler)
       :summary "pods run without resource requests or limits"
       :hint "BestEffort QoS - set them on the Pooler's template"})))

;; --- a Service that resolves to nothing ---------------------------------------

(defn- declared-instances
  "spec.instances, or 1 where it is not a number: the CRD defaults it to 1, so
   a Pooler read without it still runs one PgBouncer pod."
  [pooler]
  (let [instances (-> pooler :spec :instances)]
    (if (number? instances) instances 1)))

(defn- backs?
  "Whether the Pooler backs the Service: CNPG names a Pooler's Service after
   the Pooler and selects its pods by cnpg.io/poolerName, and a Service of our
   own that selects those pods resolves to the same addresses. An ExternalName
   Service resolves through DNS and has no endpoints to judge."
  [pooler service]
  (let [name (kubectl/name-of pooler)]
    (and (= (kubectl/namespace-of pooler) (kubectl/namespace-of service))
         (not= "ExternalName" (-> service :spec :type))
         (or (= name (kubectl/name-of service))
             (= name (get-in service [:spec :selector inst/pooler-name-label]))))))

(defn- service-readings
  "One row per Service the Pooler backs, sorted by name, with its ready
   addresses, plus a row for CNPG's own Service when it does not exist."
  [pooler services slices]
  (let [namespace (kubectl/namespace-of pooler)
        backed (sort-by kubectl/name-of (filter #(backs? pooler %) services))
        own? (some #(= (kubectl/name-of pooler) (kubectl/name-of %)) backed)]
    (sort-by :service
             (cond-> (for [service backed]
                       {:service (kubectl/name-of service)
                        :ready (count (endpointslices/ready-addresses slices namespace (kubectl/name-of service)))})
               (not own?) (conj {:service (kubectl/name-of pooler) :missing? true})))))

(defn- reading-phrase
  [namespace {:keys [service missing?]}]
  (format "Service %s/%s %s" namespace service
          (if missing? "does not exist" "has no ready address")))

(defn detect-poolers-serving-nothing
  "Poolers that declare one or more instances while a Service they back
   resolves to no ready address, or CNPG's own Service does not exist: every
   client connecting through it is refused or times out, whatever the pods
   say. A Pooler declaring 0 instances serves nothing on purpose. The check
   joins three reads, and judges nothing when any of them did not answer, so
   a missing read never reads as zero endpoints."
  [evidence]
  (let [poolers (get evidence pooler-type)
        services (get evidence service-type)
        slices (get evidence endpointslice-type)]
    (when (and poolers services slices)
      (for [pooler (kubectl/items-of poolers)
            :let [instances (declared-instances pooler)]
            :when (pos? instances)
            :let [namespace (kubectl/namespace-of pooler)
                  name (kubectl/name-of pooler)
                  readings (service-readings pooler (kubectl/items-of services) slices)
                  empty (filter #(or (:missing? %) (zero? (:ready %))) readings)]
            :when (seq empty)]
        {:severity :critical
         :component (kubectl/namespace-name-of pooler)
         :summary (format "serves nothing%s: %s while the Pooler declares %d instance%s"
                          (if (= (count empty) (count readings)) "" " through part of its Services")
                          (str/join ", " (map #(reading-phrase namespace %) empty))
                          instances (if (= 1 instances) "" "s"))
         :hint (str "every client connecting through "
                    (str/join ", " (map #(str namespace "/" (:service %)) empty))
                    " is refused or times out"
                    (when (some (complement :missing?) empty)
                      (str "; read why the pooler's pods are not ready: `kubectl -n " namespace
                           " get pods -l " (subs (str inst/pooler-name-label) 1) "=" name
                           "` and the events of Deployment " namespace "/" name))
                    (when (some :missing? empty)
                      (str "; CNPG creates the Service named after the Pooler on every reconcile, "
                           "so a missing one points at the operator - read its log")))}))))

(def detectives
  [{:name "cnpg-pooler-stale"
    :description "Poolers left behind by an instance rebuild"
    :requires [pooler-type cluster-type "pods"]
    :detect detect-stale-poolers}
   {:name "cnpg-pooler-rollout"
    :description "Poolers serving traffic from more than one pod generation"
    :requires [pooler-type "pods"]
    :detect detect-unfinished-rollouts}
   {:name "cnpg-pooler-resources"
    :description "Pooler pods without resource requests or limits"
    :requires [pooler-type "pods"]
    :detect detect-poolers-without-resources}
   {:name "cnpg-pooler-endpoints"
    :description "Poolers declaring instances whose Service resolves to no ready address, a total outage for their clients"
    :requires [pooler-type service-type endpointslice-type]
    :joins [pooler-type service-type endpointslice-type]
    :judges [pooler-type]
    :detect detect-poolers-serving-nothing}])
