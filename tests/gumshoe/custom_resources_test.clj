;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.custom-resources-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.custom-resources :as custom-resources]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.kubectl :as kubectl]))

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

;; --- APIServices --------------------------------------------------------------

(defn- api-service
  [name & {:keys [service status reason message]}]
  {:metadata {:name name}
   :spec (cond-> {:group "metrics.k8s.io" :version "v1beta1"} service (assoc :service service))
   :status {:conditions [{:type "Available" :status status :reason reason :message message}]}})

(deftest an-unavailable-aggregated-api-is-critical
  (let [[finding :as findings] (custom-resources/detect-api-services
                                {"apiservices" {:items [(api-service "v1beta1.metrics.k8s.io"
                                                                     :service {:namespace "kube-system" :name "metrics-server"}
                                                                     :status "False" :reason "FailedDiscoveryCheck"
                                                                     :message "failing or missing response")]}})]
    (is (= 1 (count findings)))
    (is (= :critical (:severity finding)))
    (is (= "v1beta1.metrics.k8s.io" (:component finding)))
    (is (= "APIService is unavailable (FailedDiscoveryCheck)" (:summary finding)))
    (is (str/includes? (:hint finding) "metrics.k8s.io/v1beta1"))
    (is (str/includes? (:hint finding) "Service kube-system/metrics-server"))))

(deftest local-and-available-api-services-are-quiet
  (is (empty? (custom-resources/detect-api-services
               {"apiservices" {:items [(api-service "v1.apps" :status "False")
                                       (api-service "v1beta1.metrics.k8s.io" :service {:namespace "a" :name "b"} :status "True")
                                       {:metadata {:name "v1.example.com"} :spec {:service {:namespace "a" :name "b"}}}]}}))))

;; --- CRDs ---------------------------------------------------------------------

(defn- crd
  [name & {:keys [conditions versions]}]
  {:metadata {:name name}
   :spec {:versions (or versions [{:name "v1" :served true :subresources {:status {}}}])}
   :status {:conditions (or conditions [{:type "Established" :status "True"} {:type "NamesAccepted" :status "True"}])}})

(deftest broken-crds-are-critical
  (is (= [[:critical "CustomResourceDefinition names conflict with another resource"]
          [:critical "CustomResourceDefinition is not established"]]
         (map (juxt :severity :summary)
              (custom-resources/detect-crd-health
               {"customresourcedefinitions"
                {:items [(crd "widgets.example.com" :conditions [{:type "Established" :status "False" :reason "NotAccepted"}
                                                                 {:type "NamesAccepted" :status "False" :reason "ListKindConflict"
                                                                  :message "\"WidgetList\" is already in use"}])
                         (crd "gadgets.example.com")]}}))))
  (is (= ["\"WidgetList\" is already in use (ListKindConflict) - the API server does not serve its objects"]
         (map :hint (custom-resources/detect-crd-health
                     {"customresourcedefinitions"
                      {:items [(crd "widgets.example.com" :conditions [{:type "NamesAccepted" :status "False" :reason "ListKindConflict"
                                                                        :message "\"WidgetList\" is already in use"}])]}})))))

;; --- which types are read -----------------------------------------------------

(deftest only-uncovered-crds-with-status-are-read
  (is (= ["a.example.com" "d.example.com"]
         (kubectl/status-types {:items [(crd "d.example.com")
                                        (crd "a.example.com")
                                        (crd "a.example.com")
                                        (crd "b.example.com" :versions [{:name "v1" :served true}])
                                        (crd "c.example.com" :versions [{:name "v1" :served false :subresources {:status {}}}])
                                        (crd "flux.example.com")]}
                               #{"flux.example.com"})))
  (is (= [] (kubectl/status-types nil #{}))))

(deftest covered-types-are-the-group-qualified-requirements
  (let [covered (registry/covered-types)]
    (is (every? #(str/includes? % ".") covered))
    (is (not (contains? covered "pods")))
    (is (not (contains? covered "custom-resources")))
    (is (contains? covered "gateways.gateway.networking.k8s.io") "the tls-secrets detective reads Gateways")))

;; --- the condition fallback ---------------------------------------------------

(defn- widget
  [conditions & {:keys [generation observed deleted]}]
  {:apiVersion "example.com/v1" :kind "Widget"
   :metadata (cond-> {:namespace "shop" :name "w"}
               generation (assoc :generation generation)
               deleted (assoc :deletionTimestamp "2026-11-18T11:00:00Z"))
   :status (cond-> {:conditions conditions} observed (assoc :observedGeneration observed))})

(defn- false-condition
  [type & {:keys [reason since] :or {reason "Broken" since "2026-11-18T11:00:00Z"}}]
  {:type type :status "False" :reason reason :message "it broke" :lastTransitionTime since})

(defn- judge [resource] (custom-resources/condition-finding resource now))

(deftest the-first-false-condition-is-a-warning
  (doseq [type custom-resources/condition-types]
    (let [finding (judge (widget [(false-condition type)]))]
      (is (= :warning (:severity finding)) type)
      (is (= "shop/w" (:component finding)) type)
      (is (= (format "Widget is not %s (Broken)" type) (:summary finding)) type)
      (is (= "it broke - example.com/v1" (:hint finding)) type))))

(deftest the-conditions-are-read-in-order
  (is (= "Widget is not Ready (Broken)"
         (:summary (judge (widget [(false-condition "Synced") (false-condition "Ready")])))))
  (is (= "Widget is not Available (Broken)"
         (:summary (judge (widget [{:type "Ready" :status "True"} (false-condition "Healthy") (false-condition "Available")]))))))

(deftest other-conditions-and-other-states-are-quiet
  (is (nil? (judge (widget [(false-condition "Degraded")]))) "a False Degraded is good news")
  (is (nil? (judge (widget [{:type "Ready" :status "Unknown"}]))))
  (is (nil? (judge (widget [{:type "Ready" :status "True"}]))))
  (is (nil? (judge (widget []))))
  (is (nil? (judge {}))))

(deftest a-fresh-failure-gets-ten-minutes
  (is (nil? (judge (widget [(false-condition "Ready" :since "2026-11-18T11:55:00Z")]))))
  (is (some? (judge (widget [(false-condition "Ready" :since "2026-11-18T11:50:00Z")]))))
  (testing "without a clock or a transition time it is reported"
    (is (some? (custom-resources/condition-finding (widget [(false-condition "Ready")]) nil)))
    (is (some? (judge (widget [(false-condition "Ready" :since nil)]))))
    (is (some? (judge (widget [(false-condition "Ready" :since "yesterday")]))) "an unreadable time")))

(deftest work-in-progress-gets-thirty-minutes
  (doseq [[label resource] [["a progress reason" (widget [(false-condition "Ready" :reason "Reconciling" :since "2026-11-18T11:40:00Z")])]
                            ["an unobserved generation" (widget [(false-condition "Ready" :since "2026-11-18T11:40:00Z")]
                                                                :generation 4 :observed 3)]
                            ["a condition's own observed generation"
                             (widget [(assoc (false-condition "Ready" :since "2026-11-18T11:40:00Z") :observedGeneration 3)]
                                     :generation 4 :observed 4)]]]
    (is (nil? (judge resource)) label))
  (is (some? (judge (widget [(false-condition "Ready" :reason "Reconciling" :since "2026-11-18T11:20:00Z")])))
      "past thirty minutes"))

(deftest a-terminating-resource-is-left-to-the-terminating-detective
  (is (nil? (judge (widget [(false-condition "Ready")] :deleted true)))))

(deftest a-resource-without-kind-or-version-still-reads
  (is (= ["custom resource is not Ready (Broken)" "it broke - unknown API version"]
         ((juxt :summary :hint) (judge (dissoc (widget [(false-condition "Ready")]) :kind :apiVersion))))))

(deftest the-detective-reads-the-collected-list
  (is (= ["shop/w"]
         (map :component (custom-resources/detect-custom-resource-conditions
                          {:now now "custom-resources" [(widget [(false-condition "Ready")])
                                                        (widget [{:type "Ready" :status "True"}])]}))))
  (is (empty? (custom-resources/detect-custom-resource-conditions {:now now}))))
