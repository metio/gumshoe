;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.loki
  "Detectives for a microservice-mode Loki: the components that make up its
   read, write, and backend paths, whether each is healthy, and - when a ring
   endpoint is reachable - the hash ring state. Simple-scalable mode
   (read/write/backend) is retired; a deployment still running it is flagged to
   migrate. A ring member that is not ACTIVE means a degraded component; an
   UNHEALTHY member is losing writes."
  (:require [clojure.string :as str]))

(def essential-components
  "Without every one of these the read or write path is broken - a missing one
   is an outage, not a warning."
  #{"distributor" "ingester" "querier" "query-frontend"})

(def ^:private simple-scalable-components
  "The components of the retired simple-scalable mode."
  #{"read" "write" "backend"})

(defn- components-of
  [workloads]
  (into #{} (keep :component workloads)))

(defn deployment-mode
  "Pure classification of a Loki deployment from its component workloads:
   :none when nothing matched, :simple-scalable when only read/write/backend are
   present, else :microservice."
  [workloads]
  (let [components (components-of workloads)]
    (cond
      (empty? workloads) :none
      (and (seq components) (every? simple-scalable-components components)) :simple-scalable
      :else :microservice)))

(defn detect-deployment-mode
  [evidence]
  (let [workloads (get evidence "workloads")
        namespace (get evidence "namespace")]
    (case (deployment-mode workloads)
      :none
      [{:severity :critical
        :component namespace
        :summary "no Loki components found in this namespace"
        :hint "wrong namespace or selector? nothing matched the Loki instance label"}]

      :simple-scalable
      [{:severity :warning
        :component namespace
        :summary "Loki is running in retired simple-scalable mode"
        :hint "read/write/backend is no longer supported - migrate to microservice components"}]

      [])))

(defn detect-missing-components
  [evidence]
  (let [workloads (get evidence "workloads")]
    (when (= :microservice (deployment-mode workloads))
      (let [present (components-of workloads)]
        (for [component (sort essential-components)
              :when (not (contains? present component))]
          {:severity :critical
           :component component
           :summary "essential Loki component is missing"
           :hint "the read and write path must be complete or ingest and query fail"})))))

(defn detect-component-health
  [evidence]
  (let [namespace (get evidence "namespace")]
    (for [{:keys [kind name component desired ready]} (get evidence "workloads")
          :let [ready (or ready 0)
                desired (or desired 0)
                label (or component name)
                ;; the finding is labelled by the component role, so it also
                ;; carries the actual workload so a drill-down can open it.
                subject {:kind kind :namespace namespace :name name}]
          :when (< ready desired)]
      (if (zero? ready)
        {:severity :critical
         :component label
         :subject subject
         :summary (format "component has no ready replicas (0/%d)" desired)
         :hint (format "%s/%s is down - the Loki path it serves is broken" kind name)}
        {:severity :warning
         :component label
         :subject subject
         :summary (format "component is degraded (%d/%d ready)" ready desired)
         :hint (format "%s/%s has replicas not ready - a rollout in progress, or failing pods" kind name)}))))

(defn detect-rings
  [evidence]
  (apply concat
         (for [[ring fetched] (sort-by first (get evidence "rings"))
               :when (:reachable fetched)
               :let [shards (-> fetched :json :shards)]
               shard shards
               :when (not= "ACTIVE" (:state shard))]
           [{:severity (if (= "UNHEALTHY" (:state shard)) :critical :warning)
             :component (format "%s/%s" ring (:id shard))
             :summary (format "ring member is %s" (:state shard))
             :hint (if (= "UNHEALTHY" (:state shard))
                     "the member missed its heartbeat - writes routed to it are lost"
                     "the member is mid-transition - transient during rollouts, investigate if it persists")}])))

(def detectives
  [{:name "loki-mode"
    :description "Loki runs in microservice mode with components present"
    :requires ["workloads" "namespace"]
    :detect detect-deployment-mode}
   {:name "loki-components"
    :description "Every essential Loki component (distributor, ingester, querier, query-frontend) exists"
    :requires ["workloads"]
    :detect detect-missing-components}
   {:name "loki-health"
    :description "Every Loki component has all its replicas ready"
    :requires ["workloads"]
    :detect detect-component-health}
   {:name "loki-rings"
    :description "Every reachable component hash ring is fully ACTIVE"
    :requires ["rings"]
    :detect detect-rings}])
