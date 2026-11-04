;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.pooler
  "Detectives for CloudNativePG Poolers: a pooler left behind by an instance
   rebuild, a rollout that never finished, and pooler pods with no resources.

   A Pooler's PgBouncer pods hold credential state that the operator does not
   refresh when it replaces the cluster's instances. A pooler older than the
   instances it fronts has been seen to fail every backend login with `password
   authentication failed` while the same credential works against the cluster
   directly, which a restart of the pooler clears.

   Pod age alone does not prove that, so the finding reports the structural fact
   and names the two signatures that tell the causes apart: `password
   authentication failed` is this, while `connect failed` is the backend being
   unreachable and has nothing to do with the pooler. An unhealthy cluster is
   reported by the cnpg detectives instead, because a stale pooler cannot be
   told from an unreachable one until the cluster itself is well."
  (:require [gumshoe.kubectl :as kubectl]))

(def pooler-type "poolers.postgresql.cnpg.io")
(def cluster-type "clusters.postgresql.cnpg.io")

(def ^:private healthy-phase "Cluster in healthy state")

(def ^:private pooler-name-label :cnpg.io/poolerName)
(def ^:private cluster-label :cnpg.io/cluster)
(def ^:private template-hash-label :pod-template-hash)

(defn- labels-of [pod] (-> pod :metadata :labels))

(defn- created-at
  [pod]
  (some-> pod :metadata :creationTimestamp java.time.Instant/parse))

(defn- owned-by-cluster?
  "Instance pods are created by the operator with an ownerReference to the
   Cluster; pooler pods are owned by a ReplicaSet. Checked alongside the label
   so neither alone has to be load-bearing."
  [cluster pod]
  (some #(and (= "Cluster" (:kind %)) (= cluster (:name %)))
        (-> pod :metadata :ownerReferences)))

(defn- instance-pods
  [pods namespace cluster]
  (filter #(and (= namespace (kubectl/namespace-of %))
                (or (= cluster (get (labels-of %) cluster-label))
                    (owned-by-cluster? cluster %)))
          pods))

(defn- pooler-pods
  [pods namespace pooler]
  (filter #(and (= namespace (kubectl/namespace-of %))
                (= pooler (get (labels-of %) pooler-name-label)))
          pods))

(defn- ready?
  [pod]
  (some #(and (= "Ready" (:type %)) (= "True" (:status %)))
        (-> pod :status :conditions)))

(defn- earliest
  [instants]
  (when (seq instants) (reduce #(if (.isBefore %2 %1) %2 %1) instants)))

(defn- cluster-of [pooler] (-> pooler :spec :cluster :name))

(defn- roll-hint
  [pooler]
  (format "roll it: kubectl -n %s rollout restart deployment/%s"
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
         (or (nil? instances) (nil? ready) (= ready instances)))))

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
                instances (keep created-at (instance-pods pods namespace cluster))
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
                  "is a pooler the rebuild left behind, cleared by " (roll-hint pooler)
                  "; `connect failed` is the backend being unreachable, which a "
                  "restart will not fix")})))

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
    :detect detect-poolers-without-resources}])
