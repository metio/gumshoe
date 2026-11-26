;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.deletion
  "The decisions behind the delete runbook, pure over what it fetched: which
   objects can be picked, what depends on the one picked, and what the
   confirmation says. A pick names one object as \"<type> <namespace>/<name>\".
   The reads that fetch it (candidates, read-dependents, dry-run) sit apart
   at the end, so the book and its gumshoe.dsl form read the same way."
  (:require [clojure.string :as str]
            [gumshoe.cleanup :as cleanup]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.pick :as pick]
            [gumshoe.shell :as shell]))

(def types
  "The namespaced types the runbook deletes."
  ["deployments" "statefulsets" "daemonsets" "jobs" "cronjobs" "pods" "services" "ingresses"
   "configmaps" "persistentvolumeclaims"])

(def parse-pick
  "\"<type> <namespace>/<name>\" of one of types as {:type :namespace :name},
   or nil."
  (pick/typed types))

(defn picks
  [lists-by-type]
  (vec (for [type types
             object (sort-by kubectl/namespace-name-of (kubectl/items-of (get lists-by-type type)))
             :when (not (-> object :metadata :deletionTimestamp))]
         (str type " " (kubectl/namespace-name-of object)))))

;; --- dependents ---------------------------------------------------------------

(def ^:private workload-kinds
  {"deployments" "Deployment" "statefulsets" "StatefulSet" "daemonsets" "DaemonSet" "jobs" "Job"})

(defn- in-namespace
  [namespace list]
  (filter #(= namespace (kubectl/namespace-of %)) (kubectl/items-of list)))

(defn- pod-templates
  "Each workload's pod spec in the namespace, by \"Kind name\"."
  [evidence namespace]
  (concat
   (for [[type kind] workload-kinds
         workload (in-namespace namespace (get evidence type))]
     [(str kind " " (kubectl/name-of workload)) (-> workload :spec :template :spec)])
   (for [cronjob (in-namespace namespace (get evidence "cronjobs"))]
     [(str "CronJob " (kubectl/name-of cronjob)) (-> cronjob :spec :jobTemplate :spec :template :spec)])))

(defn- config-map-names
  "Every ConfigMap a pod spec reads."
  [spec]
  (set (concat
        (keep #(-> % :configMap :name) (:volumes spec))
        (for [volume (:volumes spec) source (-> volume :projected :sources) :let [n (-> source :configMap :name)] :when n] n)
        (for [container (concat (:initContainers spec) (:containers spec))
              ref (concat (keep #(-> % :configMapRef :name) (:envFrom container))
                          (keep #(-> % :valueFrom :configMapKeyRef :name) (:env container)))]
          ref))))

(defn dependents
  "What depends on the object, as lines for the confirmation."
  [{:keys [type namespace name]} evidence]
  (let [pods (in-namespace namespace (get evidence "pods"))]
    (case type
      ("deployments" "statefulsets" "daemonsets" "jobs")
      (let [owned (filter #(= (str (workload-kinds type) "/" name) (kubectl/workload-of %)) pods)]
        (when (seq owned)
          [(format "its %d pod(s) are deleted with it: %s" (count owned) (str/join ", " (sort (map kubectl/name-of owned))))]))

      "cronjobs"
      (let [jobs (filter (fn [job] (some #(and (= "CronJob" (:kind %)) (= name (:name %))) (-> job :metadata :ownerReferences)))
                         (in-namespace namespace (get evidence "jobs")))]
        (when (seq jobs)
          [(format "its %d Job(s) are deleted with it: %s" (count jobs) (str/join ", " (sort (map kubectl/name-of jobs))))]))

      "persistentvolumeclaims"
      (let [users (filter (fn [pod] (some #(= name (-> % :persistentVolumeClaim :claimName)) (-> pod :spec :volumes))) pods)]
        (when (seq users)
          [(format "pods mount it, so it stays Terminating until they are gone: %s" (str/join ", " (sort (map kubectl/name-of users))))]))

      "services"
      (let [routes (filter (fn [ingress]
                             (some #(= name (-> % :backend :service :name))
                                   (concat [{:backend (-> ingress :spec :defaultBackend)}]
                                           (mapcat #(-> % :http :paths) (-> ingress :spec :rules)))))
                           (in-namespace namespace (get evidence "ingresses")))]
        (when (seq routes)
          [(format "Ingresses route to it and will answer 503: %s" (str/join ", " (sort (map kubectl/name-of routes))))]))

      "configmaps"
      (let [users (for [[label spec] (pod-templates evidence namespace) :when (contains? (config-map-names spec) name)] label)]
        (when (seq users)
          [(format "workloads read it, and their new pods will not start: %s" (str/join ", " (sort users)))]))

      nil)))

;; --- the dry run ----------------------------------------------------------------

(defn dry-run-args
  [{:keys [type namespace name]}]
  [(str "--namespace=" namespace) "delete" type name "--dry-run=server"])

(defn delete-args
  [{:keys [type namespace name]}]
  [(str "--namespace=" namespace) "delete" type name "--wait=false"])

(defn dry-run-result
  "The outcome of a server-side dry run from its process map."
  [{:keys [exit out err]}]
  {:ok? (= 0 exit)
   :message (str/trim (str (if (= 0 exit) out err)))})

(defn deletion-items
  [target dry-run dependents]
  (into [(format "delete %s" target)
         (if (:ok? dry-run)
           (str "the API server accepted a dry run: " (:message dry-run))
           (str "the API server refused a dry run: " (:message dry-run)))]
        dependents))

(defn deleted?
  "Whether the object is gone, or on its way out."
  [object]
  (or (nil? object) (some? (-> object :metadata :deletionTimestamp))))

;; --- the reads ------------------------------------------------------------------

(defn candidates
  "The picks over every listed type in every namespace of context."
  [context]
  (picks (into {} (for [type types] [type (kubectl/get-all context type)]))))

(def ^:private evidence-types ["pods" "jobs" "ingresses" "deployments" "statefulsets" "daemonsets" "cronjobs"])

(defn read-dependents
  "dependents of the object a ref names, over what its namespace holds now.
   A claim's lines also say what deleting it does to its volume, since under
   the Delete reclaim policy the volume and its data go with it."
  [context {:keys [namespace type name] :as ref}]
  (cond-> (vec (dependents ref (into {} (for [t evidence-types]
                                          [t (kubectl/get-namespaced context namespace t)]))))
    (= "persistentvolumeclaims" type) (conj (cleanup/read-claim-volume-line context namespace name))))

(defn dry-run
  "A server-side dry run of deleting the object a ref names: admission
   webhooks and policies judge it and nothing changes, so its refusal is the
   real delete's refusal."
  [context ref]
  (dry-run-result (apply shell/execute "kubectl" (str "--context=" context) (dry-run-args ref))))
