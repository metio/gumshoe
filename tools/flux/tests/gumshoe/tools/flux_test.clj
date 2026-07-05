;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.flux-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.command :as command]
            [gumshoe.detectives.registry :as registry]
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
