;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.custom-resources
  "Detectives for the API's extensions: aggregated APIServices, the CRDs
   themselves, and a fallback for custom resources no tool package knows. Most
   operators follow the Kubernetes condition conventions, so a False Ready (or
   one of its common siblings) is worth reading even without knowing what the
   object does."
  (:require [gumshoe.kubectl :as kubectl]))

(def ^:private settle-minutes 10)

(def condition-types
  "The condition types read, in order; the first that is False is reported."
  ["Ready" "Available" "Reconciled" "Healthy" "Synced"])

(def ^:private progress-reasons
  #{"Progressing" "Reconciling" "Pending" "Creating" "Updating" "DependencyNotReady"})

(defn- conditions-by-type
  [resource]
  (into {} (map (juxt :type identity)) (-> resource :status :conditions)))

(defn- minutes-since
  [now timestamp]
  (when (and now timestamp)
    (try
      (.toMinutes (java.time.Duration/between (java.time.Instant/parse timestamp) now))
      (catch Exception _ nil))))

(defn detect-api-services
  [evidence]
  (for [service (kubectl/items-of (get evidence "apiservices"))
        :when (-> service :spec :service)
        :let [available (get (conditions-by-type service) "Available")
              {:keys [namespace name]} (-> service :spec :service)]
        :when (= "False" (:status available))]
    {:severity :critical
     :component (kubectl/name-of service)
     :summary (format "APIService is unavailable (%s)" (or (:reason available) "unknown"))
     :hint (format "%s - every request for %s/%s fails, and namespace deletion and API discovery stall on it; check the Service %s/%s and the pods behind it"
                   (or (:message available) "no message")
                   (-> service :spec :group) (-> service :spec :version) namespace name)}))

(defn detect-crd-health
  [evidence]
  (for [crd (kubectl/items-of (get evidence "customresourcedefinitions"))
        :let [conditions (conditions-by-type crd)]
        [type summary] [["NamesAccepted" "CustomResourceDefinition names conflict with another resource"]
                        ["Established" "CustomResourceDefinition is not established"]]
        :let [condition (get conditions type)]
        :when (= "False" (:status condition))]
    {:severity :critical
     :component (kubectl/name-of crd)
     :summary summary
     :hint (format "%s (%s) - the API server does not serve its objects"
                   (or (:message condition) "no message") (or (:reason condition) "no reason"))}))

(defn- in-progress?
  [resource condition]
  (or (contains? progress-reasons (:reason condition))
      (let [generation (-> resource :metadata :generation)
            observed (or (:observedGeneration condition) (-> resource :status :observedGeneration))]
        (and (number? generation) (number? observed) (< observed generation)))))

(defn condition-finding
  "The finding for one custom resource, or nil. Its first False condition among
   `condition-types` is a warning: without knowing the kind, gumshoe cannot
   tell how bad it is. A condition that turned False under ten minutes ago is
   not reported yet, and one that describes work in progress gets thirty."
  [resource now]
  (let [conditions (conditions-by-type resource)
        failing (first (keep #(let [c (get conditions %)] (when (= "False" (:status c)) c)) condition-types))
        age (minutes-since now (:lastTransitionTime failing))]
    (when (and failing
               (not (-> resource :metadata :deletionTimestamp))
               (not (and age (< age settle-minutes)))
               (not (and (in-progress? resource failing) age (< age (* 3 settle-minutes)))))
      {:severity :warning
       :component (kubectl/namespace-name-of resource)
       :summary (format "%s is not %s (%s)" (or (:kind resource) "custom resource") (:type failing)
                        (or (:reason failing) "unknown"))
       :hint (format "%s - %s" (or (:message failing) "no message")
                     (or (:apiVersion resource) "unknown API version"))})))

(defn detect-custom-resource-conditions
  [evidence]
  (let [now (:now evidence)]
    (keep #(condition-finding % now) (kubectl/items-of (get evidence "custom-resources")))))

(def detectives
  [{:name "api-services"
    :description "Aggregated APIServices that are unavailable"
    :requires ["apiservices"]
    :detect detect-api-services}
   {:name "crd-health"
    :description "CustomResourceDefinitions that are not established or whose names conflict"
    :requires ["customresourcedefinitions"]
    :detect detect-crd-health}
   {:name "custom-resource-conditions"
    :description "Custom resources no tool package covers whose Ready, Available, Reconciled, Healthy or Synced condition is False"
    :requires ["custom-resources"]
    :detect detect-custom-resource-conditions}])
