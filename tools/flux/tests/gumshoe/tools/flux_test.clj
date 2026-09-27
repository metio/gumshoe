;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.flux-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.command :as command]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.subject :as subject]
            [gumshoe.tools.flux :as flux]))

(defn- summaries [findings] (set (map :summary findings)))

(deftest flux-detective-test
  (let [evidence {flux/helmrelease-type
                  {:items [{:metadata {:namespace "moodle" :name "moodle"}
                            :status {:conditions [{:type "Ready" :status "False" :reason "UpgradeFailed"}]}}
                           {:metadata {:namespace "keycloak" :name "keycloak"}
                            :spec {:suspend true}
                            :status {:conditions [{:type "Ready" :status "True"}]}}]}}
        findings (flux/detect-helmrelease-problems evidence)]
    (is (= #{"HelmRelease is not Ready (UpgradeFailed)"
             "HelmRelease is suspended"}
           (summaries findings)))))

(deftest flux-source-detective-test
  (is (= #{"OCIRepository is not Ready (PullFailed)"}
         (summaries (flux/detect-ocirepository-problems
                     {flux/ocirepository-type
                      {:items [{:metadata {:namespace "flux-system" :name "charts"}
                                :status {:conditions [{:type "Ready" :status "False"
                                                       :reason "PullFailed"}]}}]}})))))

(deftest package-registers-into-every-flux-seam-test
  (testing "requiring the package ran provide!, so all of flux is registered"
    (is (seq (registry/for-scope :gitops)) "the gitops scan is filled by the flux package")
    (is (contains? (set (capabilities/registered)) :flux) "the :flux capability detector")
    (is (= "2.0" (command/tool-min-version "flux")) "the flux CLI tool profile")))

(deftest externalartifact-back-pointer-edge-test
  (testing "spec.sourceRef walks from an artifact to the object that produced it, in the artifact's namespace"
    (is (= [{:relation "produced by"
             :subject (subject/subject "JsonnetSnippet" "apps" "dashboards")}]
           (flux/externalartifact-edges
            {:metadata {:namespace "apps" :name "dashboards"}
             :spec {:sourceRef {:apiVersion "jaas.metio.wtf/v1"
                                :kind "JsonnetSnippet"
                                :name "dashboards"}}})))))

(deftest externalartifact-without-back-pointer-test
  (testing "an artifact whose producer left no back-pointer simply has no edge"
    (is (empty? (flux/externalartifact-edges
                 {:metadata {:namespace "apps" :name "orphan"} :spec {}})))))

(deftest externalartifact-is-a-drill-down-subject-test
  (is (= flux/externalartifact-type (subject/kind->type "ExternalArtifact"))))

(deftest parked-release-is-reported-with-its-remedy-test
  (testing "a release whose retries are spent is named as parked, not merely unready"
    (let [findings (flux/detect-helmrelease-problems
                    {flux/helmrelease-type
                     {:items [{:metadata {:namespace "web" :name "site"}
                               :status {:upgradeFailures 1
                                        :lastAttemptedRevision "2026.9.19"
                                        :conditions [{:type "Ready" :status "False"
                                                      :reason "UpgradeFailed"
                                                      :message "host already defined in ingress old/site"}]}}]}})]
      (is (= #{"HelmRelease is parked after UpgradeFailed and will not retry"}
             (summaries findings)))
      (is (re-find #"resetAt" (:hint (first findings)))
          "the remedy that clears the counters is in the hint")
      (is (re-find #"2026\.9\.19" (:hint (first findings)))
          "the revision of the attempt, so the age of the claim is visible"))))

(deftest retry-on-failure-is-never-parked-test
  (testing "RetryOnFailure keeps trying, so a failure is reported as a plain unready release"
    (is (= #{"HelmRelease is not Ready (UpgradeFailed)"}
           (summaries (flux/detect-helmrelease-problems
                       {flux/helmrelease-type
                        {:items [{:metadata {:namespace "web" :name "site"}
                                  :spec {:upgrade {:strategy {:name "RetryOnFailure"
                                                              :retryInterval "5m"}}}
                                  :status {:upgradeFailures 3
                                           :conditions [{:type "Ready" :status "False"
                                                         :reason "UpgradeFailed"}]}}]}}))))))

(deftest retries-budget-is-respected-test
  (testing "failures within the configured budget are not parked yet"
    (is (= #{"HelmRelease is not Ready (UpgradeFailed)"}
           (summaries (flux/detect-helmrelease-problems
                       {flux/helmrelease-type
                        {:items [{:metadata {:namespace "web" :name "site"}
                                  :spec {:upgrade {:remediation {:retries 3}}}
                                  :status {:upgradeFailures 2
                                           :conditions [{:type "Ready" :status "False"
                                                         :reason "UpgradeFailed"}]}}]}}))))))

(deftest green-release-over-crash-looping-pods-test
  (testing "a succeeded Helm operation says nothing about whether the software runs"
    (let [findings (flux/detect-succeeded-over-broken
                    {flux/helmrelease-type
                     {:items [{:metadata {:namespace "keycloak" :name "keycloak"}
                               :status {:conditions [{:type "Ready" :status "True"}]}}]}
                     "pods"
                     {:items [{:metadata {:namespace "keycloak" :name "keycloak-0"}
                               :status {:containerStatuses
                                        [{:name "keycloak" :restartCount 349
                                          :state {:waiting {:reason "CrashLoopBackOff"}}}]}}
                              {:metadata {:namespace "keycloak" :name "sql-exporter-1"}
                               :status {:containerStatuses
                                        [{:name "sql-exporter" :state {:running {}}}]}}]}})]
      (is (= #{"HelmRelease reports success while 1 pod(s) in its namespace cannot start"}
             (summaries findings)))
      (is (re-find #"349 restarts" (:hint (first findings)))
          "the restart count that makes it obvious"))))

(deftest green-release-over-healthy-pods-is-quiet-test
  (is (empty? (flux/detect-succeeded-over-broken
               {flux/helmrelease-type
                {:items [{:metadata {:namespace "web" :name "site"}
                          :status {:conditions [{:type "Ready" :status "True"}]}}]}
                "pods"
                {:items [{:metadata {:namespace "web" :name "site-0"}
                          :status {:containerStatuses [{:name "web" :state {:running {}}}]}}]}}))))

(deftest inert-delivery-test
  (testing "a reconcile-disabled gate reads as managed from every angle but is not"
    (is (= #{"delivery is annotated to skip reconciliation"}
           (summaries (flux/detect-inert-deliveries
                       {flux/helmrelease-type
                        {:items [{:metadata {:namespace "web" :name "site"
                                             :annotations {:kustomize.toolkit.fluxcd.io/reconcile "disabled"}}}]}
                        flux/kustomization-type
                        {:items [{:metadata {:namespace "flux-system" :name "platform"}}]}}))))))
