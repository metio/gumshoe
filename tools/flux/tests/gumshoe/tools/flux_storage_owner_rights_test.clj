;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.flux-storage-owner-rights-test
  "clear_helm_storage checks patch rights on the owner it found - a
   Kustomization or a ResourceSet - before the confirmation, so an operator
   without them is refused before anything is suspended or deleted, and one
   who holds them for that kind is not refused for the other."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.playbook-books-test :as harness]
            [gumshoe.shell :as shell]
            [gumshoe.tools.flux-storage :as storage]))

(def ^:private resourceset-owner
  {:kind "ResourceSet" :type "resourcesets.fluxcd.controlplane.io" :namespace "flux-system" :name "apps"})

(deftest the-owner-found-is-the-one-whose-rights-are-checked
  (is (nil? (storage/owner-patch-refusal (constantly true) resourceset-owner)))
  (is (nil? (storage/owner-patch-refusal (constantly false) nil)) "no owner needs no rights")
  (let [asked (atom nil)
        refusal (storage/owner-patch-refusal (fn [owner] (reset! asked owner) false) resourceset-owner)]
    (is (= resourceset-owner @asked))
    (is (str/includes? refusal "you may not patch ResourceSet flux-system/apps"))))

(deftest the-rights-are-asked-for-that-one-object
  (let [argv (atom nil)]
    (with-redefs [shell/execute (fn [& args] (reset! argv (vec args)) {:exit 0 :out "" :err ""})]
      (is (true? (storage/can-patch-owner? "ctx" resourceset-owner))))
    (is (= ["kubectl" "--context=ctx" "auth" "can-i" "patch" "resourcesets.fluxcd.controlplane.io/apps"
            "--namespace=flux-system" "--quiet"]
           @argv)))
  (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err ""})]
    (is (false? (storage/can-patch-owner? "ctx" resourceset-owner)))))

(defn- run
  [owner-entry denied]
  (let [spec (assoc (#'harness/load-book "tools/flux/runbooks/flux/clear_helm_storage.clj") :steps (storage/steps))]
    (harness/transcript spec {:objects (#'harness/release-cluster owner-entry)
                              :argv ["--namespace" "apps" "--name" "podinfo"]
                              :react @#'harness/helm-controller
                              :denied denied})))

(def ^:private resourceset-entry
  ["resourcesets.fluxcd.controlplane.io"
   {:metadata {:namespace "flux-system" :name "apps" :uid "uid-apps"
               :annotations {:fluxcd.controlplane.io/reconcileEvery "30m"}}
    :spec {}
    :status {:conditions [{:type "Ready" :status "True" :reason "ReconciliationSucceeded"}]
             :inventory {:entries [{:id "apps_podinfo_helm.toolkit.fluxcd.io_HelmRelease" :v "v2"}]}}}])

(defn- asks-about [kind] (fn [argv] (some #(str/starts-with? (str %) kind) argv)))

(deftest a-resourceset-owner-without-patch-rights-is-refused-before-anything-changes
  (let [{:keys [result writes confirmations out]} (run resourceset-entry (asks-about "resourcesets."))]
    (is (not (true? result)))
    (is (= [] writes) "nothing was suspended or deleted")
    (is (= [] confirmations) "the operator was not asked to approve a run that would stop")
    (is (some #(str/includes? % "you may not patch ResourceSet flux-system/apps") out) (pr-str out))))

(deftest rights-on-kustomizations-do-not-stand-in-for-a-resourceset-owner
  (testing "an operator denied Kustomizations whose owner is a ResourceSet runs"
    (let [{:keys [result writes]} (run resourceset-entry (asks-about "kustomizations."))]
      (is (true? result))
      (is (some #(str/includes? % "resourcesets.fluxcd.controlplane.io") writes)))))
