;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.flux-suspend-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.tools.flux :as flux]))

(defn- resource [name & {:keys [suspend]}]
  {:metadata {:namespace "flux-system" :name name} :spec (cond-> {} (some? suspend) (assoc :suspend suspend))})

(deftest delivery-picks-parse
  (is (= {:kind "Kustomization" :type flux/kustomization-type :namespace "flux-system" :name "apps"}
         (flux/parse-delivery-pick "Kustomization flux-system/apps")))
  (is (= flux/helmrelease-type (:type (flux/parse-delivery-pick "HelmRelease db/pg"))))
  (doseq [bad [nil "" "HelmChart flux-system/x" "Kustomization apps" "flux-system/apps"]]
    (is (nil? (flux/parse-delivery-pick bad)) (pr-str bad))))

(deftest picks-offer-the-other-state
  (let [lists {"Kustomization" {:items [(resource "b") (resource "a" :suspend true) (resource "c" :suspend false)]}
               "HelmRelease" {:items [(resource "pg" :suspend true)]}
               "GitRepository" nil}]
    (is (= ["Kustomization flux-system/b" "Kustomization flux-system/c"] (flux/delivery-picks lists true)))
    (is (= ["HelmRelease flux-system/pg" "Kustomization flux-system/a"] (flux/delivery-picks lists false)))))

(deftest suspend-refusals
  (is (nil? (flux/suspend-refusal (resource "a") true)))
  (is (nil? (flux/suspend-refusal (resource "a" :suspend true) false)))
  (is (= "it is suspended already" (flux/suspend-refusal (resource "a" :suspend true) true)))
  (is (= "it is not suspended" (flux/suspend-refusal (resource "a") false)))
  (is (= "the object no longer exists" (flux/suspend-refusal nil true))))

(deftest the-confirmation-says-what-stops-and-what-catches-up
  (is (= ["suspend Kustomization flux-system/apps - Flux stops reconciling it, and what it applied stays as it is"
          "changes pushed to its source are not applied until it is resumed"]
         (flux/suspend-items "Kustomization" (resource "apps") true)))
  (is (= ["resume Kustomization flux-system/apps and request a reconcile - changes pushed to its source meanwhile are applied now"]
         (flux/suspend-items "Kustomization" (resource "apps" :suspend true) false))))

(deftest suspending-patches-and-resuming-also-requests-a-reconcile
  (let [pick {:type flux/kustomization-type :namespace "flux-system" :name "apps"}]
    (is (= [[:kubectl "staging" "-n" "flux-system" "patch" flux/kustomization-type "apps" "--type=merge"
             "--patch={\"spec\":{\"suspend\":true}}"]]
           (flux/suspend-effect "staging" pick true "2026-11-19T10:00:00Z")))
    (is (= [[:kubectl "staging" "-n" "flux-system" "patch" flux/kustomization-type "apps" "--type=merge"
             "--patch={\"spec\":{\"suspend\":false}}"]
            [:kubectl "staging" "-n" "flux-system" "annotate" flux/kustomization-type "apps" "--overwrite"
             "reconcile.fluxcd.io/requestedAt=2026-11-19T10:00:00Z"]]
           (flux/suspend-effect "staging" pick false "2026-11-19T10:00:00Z")))))
