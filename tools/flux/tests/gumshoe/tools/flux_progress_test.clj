;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.flux-progress-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.tools.flux :as flux]))

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

(defn- kustomization
  [& {:keys [reason message since generation observed reconciling stalled]
      :or {since "2026-11-18T11:00:00Z"}}]
  {:metadata (cond-> {:namespace "flux-system" :name "apps"} generation (assoc :generation generation))
   :status (cond-> {:conditions (cond-> [{:type "Ready" :status "False" :reason reason :message message
                                          :lastTransitionTime since}]
                                  reconciling (conj {:type "Reconciling" :status "True" :reason reconciling})
                                  stalled (conj {:type "Stalled" :status "True" :reason stalled :message "gave up"}))}
             observed (assoc :observedGeneration observed))})

(defn- judge
  [resource & {:keys [at] :or {at now}}]
  (map (juxt :severity :summary)
       (flux/detect-kustomization-problems {:now at flux/kustomization-type {:items [resource]}})))

(deftest a-failure-stays-critical
  (is (= [[:critical "Kustomization is not Ready (BuildFailed)"]] (judge (kustomization :reason "BuildFailed"))))
  (testing "however recent"
    (is (= [[:critical "Kustomization is not Ready (BuildFailed)"]]
           (judge (kustomization :reason "BuildFailed" :since "2026-11-18T11:59:00Z")))))
  (testing "a failure being retried is still a failure"
    (is (= [[:critical "Kustomization is not Ready (HealthCheckFailed)"]]
           (judge (kustomization :reason "HealthCheckFailed" :since "2026-11-18T11:59:00Z"
                                 :reconciling "ProgressingWithRetry"))))))

(deftest progress-is-given-ten-minutes
  (doseq [[label resource] [["reason Progressing" (kustomization :reason "Progressing" :since "2026-11-18T11:55:00Z")]
                            ["Reconciling True" (kustomization :reason "ArtifactFailed" :since "2026-11-18T11:55:00Z"
                                                               :reconciling "Progressing")]
                            ["an unobserved generation" (kustomization :reason "BuildFailed" :since "2026-11-18T11:55:00Z"
                                                                       :generation 7 :observed 6)]]]
    (is (empty? (judge resource)) label)
    (is (= [[:warning "Kustomization has been reconciling for 25 minutes"]]
           (judge resource :at (java.time.Instant/parse "2026-11-18T12:20:00Z")))
        label)))

(deftest progress-of-unknown-age-is-a-warning
  (is (= [[:warning "Kustomization is reconciling"]] (judge (kustomization :reason "Progressing" :since nil))))
  (is (= [[:warning "Kustomization is reconciling"]]
         (map (juxt :severity :summary)
              (flux/detect-kustomization-problems {flux/kustomization-type {:items [(kustomization :reason "Progressing")]}})))
      "a scan without a clock"))

(deftest an-observed-generation-is-not-progress
  (is (= [:critical] (map first (judge (kustomization :reason "BuildFailed" :since "2026-11-18T11:59:00Z"
                                                      :generation 7 :observed 7))))))

(deftest a-dependency-wait-is-info
  (let [[finding] (flux/detect-kustomization-problems
                   {:now now flux/kustomization-type
                    {:items [(kustomization :reason "DependencyNotReady" :message "dependency 'flux-system/infra' is not ready")]}})]
    (is (= :info (:severity finding)))
    (is (= "Kustomization waits for a dependency" (:summary finding)))
    (is (= "dependency 'flux-system/infra' is not ready - the dependency is reported on its own" (:hint finding)))))

(deftest stalled-is-critical-whatever-the-reason
  (is (= [[:critical "Kustomization is stalled (InvalidPath)"]]
         (judge (kustomization :reason "Progressing" :since "2026-11-18T11:59:00Z" :stalled "InvalidPath")))))

(deftest every-flux-kind-shares-the-rules
  (let [resource (kustomization :reason "Progressing" :since "2026-11-18T11:59:00Z")]
    (doseq [[type detect] [[flux/helmrelease-type flux/detect-helmrelease-problems]
                           [flux/gitrepository-type flux/detect-gitrepository-problems]
                           [flux/ocirepository-type flux/detect-ocirepository-problems]
                           [flux/helmchart-type flux/detect-helmchart-problems]]]
      (is (empty? (detect {:now now type {:items [resource]}})) type))))

(deftest ready-unknown-and-true-are-quiet
  (is (empty? (flux/detect-kustomization-problems
               {:now now flux/kustomization-type
                {:items [{:metadata {:namespace "a" :name "b"}
                          :status {:conditions [{:type "Ready" :status "Unknown" :reason "Progressing"}]}}
                         {:metadata {:namespace "a" :name "c"}
                          :status {:conditions [{:type "Ready" :status "True" :reason "ReconciliationSucceeded"}]}}]}}))))

(deftest in-progress-reads-the-conditions
  (is (flux/in-progress? (kustomization :reason "Progressing")))
  (is (not (flux/in-progress? (kustomization :reason "BuildFailed"))))
  (is (not (flux/in-progress? (kustomization :reason "BuildFailed" :reconciling "ProgressingWithRetry"))))
  (is (not (flux/in-progress? (kustomization :reason "BuildFailed" :generation 3))) "no observed generation to compare")
  (is (not (flux/in-progress? {}))))
