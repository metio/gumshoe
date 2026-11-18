;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.posture
  "Detectives for risks that are not failures yet: a workload that runs fine
   today and breaks on the next node drain, image push or upgrade. They form
   their own scope, left out of whole-cluster health scans, because every
   finding here is a choice someone may have made on purpose."
  (:require [clojure.string :as str]
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
   {:name "deprecated-api-requests"
    :description "Deprecated APIs that clients still request"
    :requires ["deprecated-api-requests"]
    :detect detect-deprecated-api-requests}
   {:name "deprecated-stored-versions"
    :description "CustomResourceDefinitions that still store objects at a deprecated, unserved or removed version"
    :requires ["customresourcedefinitions"]
    :detect detect-deprecated-stored-versions}])
