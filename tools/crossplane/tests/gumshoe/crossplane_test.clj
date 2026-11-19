;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.crossplane-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.crossplane :as crossplane]
            [gumshoe.detectives.registry :as registry]
            [gumshoe.tools.crossplane]))

(def ^:private now (java.time.Instant/parse "2026-11-19T12:00:00Z"))

(defn- condition [type status & {:keys [reason message at] :or {at "2026-11-19T11:00:00Z"}}]
  (cond-> {:type type :status status :lastTransitionTime at}
    reason (assoc :reason reason)
    message (assoc :message message)))

(defn- package [& conditions]
  {:metadata {:name "provider-aws-s3"}
   :spec {:package "xpkg.crossplane.io/crossplane-contrib/provider-aws-s3:v2.0.0"}
   :status {:conditions (vec conditions)}})

(defn- provider [& conditions]
  (crossplane/package-findings "Provider" "every managed resource it reconciles stops syncing" (apply package conditions) now))

(deftest a-healthy-package-says-nothing
  (is (empty? (provider (condition "Installed" "True" :reason "ActivePackageRevision")
                        (condition "Healthy" "True" :reason "HealthyPackageRevision")))))

(deftest an-unhealthy-package
  (let [[finding & more] (provider (condition "Installed" "True")
                                   (condition "Healthy" "False" :reason "UnhealthyPackageRevision"
                                              :message "post establish runtime hook failed for package: provider package deployment is unavailable\nmore"))]
    (is (empty? more))
    (is (= :critical (:severity finding)))
    (is (= "provider-aws-s3" (:component finding)))
    (is (= "Provider is not healthy (Healthy)" (:summary finding)))
    (is (= "post establish runtime hook failed for package: provider package deployment is unavailable - every managed resource it reconciles stops syncing"
           (:hint finding)))
    (is (= "2026-11-19T11:00:00Z" (:since finding))))
  (testing "revision and runtime health are read too, in order"
    (is (= "Provider is not healthy (RevisionHealthy)"
           (:summary (first (provider (condition "Healthy" "True") (condition "RevisionHealthy" "False") (condition "RuntimeHealthy" "False"))))))
    (is (str/includes? (:hint (first (provider (condition "RuntimeHealthy" "False")))) "runtime Deployment")))
  (testing "Unknown health is not judged"
    (is (empty? (provider (condition "Healthy" "Unknown" :reason "UnknownPackageRevisionHealth"))))))

(deftest installation
  (testing "unpacking gets ten minutes"
    (is (empty? (provider (condition "Installed" "False" :reason "UnpackingPackage" :at "2026-11-19T11:55:00Z"))))
    (let [[finding] (provider (condition "Installed" "False" :reason "UnpackingPackage" :message "cannot pull image" :at "2026-11-19T11:30:00Z"))]
      (is (= :critical (:severity finding)))
      (is (= "2026-11-19T11:30:00Z" (:since finding)) "the onset carries the duration")
      (is (= "Provider is still unpacking" (:summary finding)))
      (is (= "cannot pull image - pulling xpkg.crossplane.io/crossplane-contrib/provider-aws-s3:v2.0.0 or resolving its dependencies fails - every managed resource it reconciles stops syncing"
             (:hint finding)))))
  (testing "an inactive revision"
    (let [[finding] (provider (condition "Installed" "False" :reason "InactivePackageRevision"))]
      (is (= [:warning "Provider's current revision is inactive"] ((juxt :severity :summary) finding)))
      (is (str/includes? (:hint finding) "revisionActivationPolicy Manual"))))
  (testing "any other reason"
    (is (= "Provider is not installed (SomethingElse)"
           (:summary (first (provider (condition "Installed" "False" :reason "SomethingElse"))))))))

(deftest installation-and-health-are-reported-separately
  (is (= ["Provider's current revision is inactive" "Provider is not healthy (Healthy)"]
         (map :summary (provider (condition "Installed" "False" :reason "InactivePackageRevision")
                                 (condition "Healthy" "False"))))))

(deftest every-package-kind
  (let [broken {:metadata {:name "x"} :status {:conditions [(condition "Healthy" "False")]}}
        findings (crossplane/detect-packages {:now now
                                              crossplane/provider-type {:items [broken]}
                                              crossplane/function-type {:items [broken]}
                                              crossplane/configuration-type {:items [broken]}})]
    (is (= ["Provider is not healthy (Healthy)" "Function is not healthy (Healthy)" "Configuration is not healthy (Healthy)"]
           (map :summary findings)))
    (is (str/ends-with? (:hint (second findings)) "every composition pipeline that calls it fails"))))

(deftest the-package-joins-the-platform-scope
  (is (every? (set (map :name (registry/for-scope :platform))) (map :name crossplane/detectives)))
  (is (empty? (detective/run-detectives crossplane/detectives {:now now})) "a cluster without Crossplane"))
