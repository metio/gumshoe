;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.custom-resources
  "Detectives for the API's extensions: aggregated APIServices, the CRDs
   themselves, and a fallback for custom resources no tool package knows. Most
   operators follow the Kubernetes condition conventions, so a False Ready (or
   one of its common siblings) is worth reading even without knowing what the
   object does."
  (:require [gumshoe.conditions :as conditions]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def ^:private settle-minutes 10)

(def condition-types
  "The condition types read, in order; the first that is False is reported."
  ["Ready" "Available" "Reconciled" "Healthy" "Synced"])

(def ^:private progress-reasons
  #{"Progressing" "Reconciling" "Pending" "Creating" "Updating" "DependencyNotReady"})

(defn detect-api-services
  [evidence]
  (for [service (kubectl/items-of (get evidence "apiservices"))
        :when (-> service :spec :service)
        :let [available (conditions/condition service "Available")
              {:keys [namespace name]} (-> service :spec :service)]
        :when (= "False" (:status available))]
    (onset/since
     {:severity :critical
      :component (kubectl/name-of service)
     :summary (format "APIService is unavailable (%s)" (or (:reason available) "unknown"))
     :hint (format "%s - every request for %s/%s fails, and namespace deletion and API discovery stall on it; check the Service %s/%s and the pods behind it"
                   (or (:message available) "no message")
                   (-> service :spec :group) (-> service :spec :version) namespace name)}
     (:lastTransitionTime available))))

(defn detect-crd-health
  [evidence]
  (for [crd (kubectl/items-of (get evidence "customresourcedefinitions"))
        [type summary] [["NamesAccepted" "CustomResourceDefinition names conflict with another resource"]
                        ["Established" "CustomResourceDefinition is not established"]]
        :let [condition (conditions/condition crd type)]
        :when (= "False" (:status condition))]
    {:severity :critical
     :component (kubectl/name-of crd)
     :summary summary
     :hint (format "%s (%s) - the API server does not serve its objects"
                   (or (:message condition) "no message") (or (:reason condition) "no reason"))}))

(defn- in-progress?
  [resource condition]
  (or (contains? progress-reasons (:reason condition))
      (conditions/stale? resource condition)))

(defn condition-finding
  "The finding for one custom resource, or nil. Its first False condition among
   `condition-types` is a warning: without knowing the kind, gumshoe cannot
   tell how bad it is. A condition that turned False under ten minutes ago is
   not reported yet, and one that describes work in progress gets thirty."
  [resource now]
  (let [failing (conditions/first-false resource condition-types)
        age (conditions/minutes-since (:lastTransitionTime failing) now)]
    (when (and failing
               (not (-> resource :metadata :deletionTimestamp))
               (not (and age (< age settle-minutes)))
               (not (and (in-progress? resource failing) age (< age (* 3 settle-minutes)))))
      (onset/since
       {:severity :warning
        :component (kubectl/namespace-name-of resource)
        :summary (format "%s is not %s (%s)" (or (:kind resource) "custom resource") (:type failing)
                         (or (:reason failing) "unknown"))
        :hint (format "%s - %s" (or (:message failing) "no message")
                      (or (:apiVersion resource) "unknown API version"))}
       (:lastTransitionTime failing)))))

(defn detect-custom-resource-conditions
  "The condition findings of every custom resource read, and one warning per
   custom resource type whose read did not answer: its objects were never
   judged, and a conversion webhook that is down breaks both the read and the
   objects."
  [evidence]
  (let [now (:now evidence)
        resources (get evidence "custom-resources")]
    (concat
     (keep #(condition-finding % now) (kubectl/items-of resources))
     (for [type (when (map? resources) (sort (:unread resources)))]
       {:severity :warning
        :component type
        :summary "custom resources of this type could not be read"
        :hint (str "their conditions were not judged - check that you may list " type
                   " and, when the CRD names a conversion webhook, that its Service answers")}))))

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
