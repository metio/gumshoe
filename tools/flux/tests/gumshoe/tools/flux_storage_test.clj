;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.flux-storage-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.tools.flux :as flux]
            [gumshoe.tools.flux-storage :as storage]))

;; --- the pending predicate and durations --------------------------------------

(deftest a-pending-revision-is-read-from-its-status-label
  (doseq [status ["pending-install" "pending-upgrade" "pending-rollback"]]
    (is (true? (flux/pending-release? {:status status})) status))
  (doseq [status ["deployed" "superseded" "failed" "uninstalling" "" nil]]
    (is (false? (flux/pending-release? {:status status})) (pr-str status)))
  (is (false? (flux/pending-release? nil))))

(deftest go-durations-become-seconds
  (is (= 300 (flux/duration-seconds "5m0s")))
  (is (= 3600 (flux/duration-seconds "1h")))
  (is (= 5430 (flux/duration-seconds "1h30m30s")))
  (is (= 1 (flux/duration-seconds "500ms")) "a fraction of a second rounds up, so a wait is never zero")
  (is (= 90 (flux/duration-seconds "1.5m")))
  (doseq [bad [nil "" "5" "five minutes" "-5m" "5x"]]
    (is (nil? (flux/duration-seconds bad)) (pr-str bad))))

(def ^:private release
  {:metadata {:namespace "apps" :name "podinfo"}
   :spec {:interval "10m" :timeout "3m"}})

(deftest the-release-waits-come-from-its-spec
  (is (= 180 (flux/helm-timeout-seconds release)))
  (is (= 300 (flux/helm-timeout-seconds {:spec {}})) "helm-controller's default timeout is 5m")
  (is (= 600 (flux/interval-seconds release))))

(deftest an-action-is-in-flight-while-a-pending-revision-is-younger-than-the-timeout
  (let [now 10000
        row (fn [status modified] {:namespace "apps" :release "podinfo" :status status :modified-at modified})]
    (is (true? (flux/action-in-flight? [(row "pending-upgrade" (- now 60))] release now)))
    (is (false? (flux/action-in-flight? [(row "pending-upgrade" (- now 181))] release now))
        "a pending revision older than spec.timeout is parked, which is what the book clears")
    (is (false? (flux/action-in-flight? [(row "deployed" now)] release now)))
    (testing "another release's pending revision does not count"
      (is (false? (flux/action-in-flight? [(assoc (row "pending-install" now) :release "other")] release now))))
    (testing "a pending revision without a modified time falls back to its creation time"
      (is (true? (flux/action-in-flight? [{:namespace "apps" :release "podinfo" :status "pending-install"
                                           :created-at (- now 10)}] release now))))))

;; --- the owner ----------------------------------------------------------------

(defn- inventory [& ids] {:status {:inventory {:entries (mapv (fn [id] {:id id :v "v2"}) ids)}}})

(deftest the-owner-is-whoever-lists-the-helmrelease-in-its-inventory
  (let [id "apps_podinfo_helm.toolkit.fluxcd.io_HelmRelease"
        ks (merge {:metadata {:namespace "flux-system" :name "apps"} :spec {:interval "5m"}}
                  (inventory "apps_other_helm.toolkit.fluxcd.io_HelmRelease" id))
        rset (merge {:metadata {:namespace "apps" :name "podinfo-set"
                                :annotations {:fluxcd.controlplane.io/reconcileEvery "30m"}}}
                    (inventory id))
        unrelated (merge {:metadata {:namespace "flux-system" :name "infra"}} (inventory "x_y_apps_Deployment"))]
    (is (= id (storage/inventory-id release)))
    (is (= [{:kind "Kustomization" :type flux/kustomization-type :namespace "flux-system" :name "apps"}]
           (map #(dissoc % :object) (storage/owners release {"Kustomization" [ks unrelated] "ResourceSet" []}))))
    (is (= [{:kind "ResourceSet" :type storage/resourceset-type :namespace "apps" :name "podinfo-set"}]
           (map #(dissoc % :object) (storage/owners release {"Kustomization" [unrelated] "ResourceSet" [rset]}))))
    (testing "the owner's reconcile interval"
      (is (= 300 (storage/owner-interval-seconds {:kind "Kustomization" :object ks})))
      (is (= 1800 (storage/owner-interval-seconds {:kind "ResourceSet" :object rset})))
      (is (= 3600 (storage/owner-interval-seconds {:kind "ResourceSet" :object {:metadata {}}}))
          "a ResourceSet without reconcileEvery reconciles hourly"))))

(deftest suspending-an-owner-uses-its-kinds-switch
  (let [ks {:kind "Kustomization" :type flux/kustomization-type :namespace "flux-system" :name "apps"}
        rs {:kind "ResourceSet" :type storage/resourceset-type :namespace "apps" :name "set"}
        flat (fn [plan] (str/join " " (flatten plan)))]
    (is (str/includes? (flat (storage/owner-suspend-effect "ctx" ks true "T")) "{\"spec\":{\"suspend\":true}}"))
    (is (str/includes? (flat (storage/owner-suspend-effect "ctx" rs true "T")) "fluxcd.controlplane.io/reconcile=disabled"))
    (is (str/includes? (flat (storage/owner-suspend-effect "ctx" rs false "T")) "fluxcd.controlplane.io/reconcile=enabled"))
    (is (true? (storage/owner-suspended? "ResourceSet" {:metadata {:annotations {:fluxcd.controlplane.io/reconcile "disabled"}}})))
    (is (false? (storage/owner-suspended? "ResourceSet" {:metadata {}})))
    (is (true? (storage/owner-suspended? "Kustomization" {:spec {:suspend true}})))))

;; --- what the release owns --------------------------------------------------------

(defn- object [kind name & {:keys [release namespace spec] :or {namespace "apps"}}]
  {:kind kind
   :metadata (cond-> {:namespace namespace :name name}
               release (assoc :annotations {:meta.helm.sh/release-name release
                                            :meta.helm.sh/release-namespace namespace}))
   :spec spec})

(deftest workloads-and-volumes-are-the-ones-helm-recorded-for-this-release
  (let [sts (object "StatefulSet" "db" :release "podinfo"
                    :spec {:volumeClaimTemplates [{:metadata {:name "data"}}]})
        lists {"deployments" [(object "Deployment" "web" :release "podinfo")
                              (object "Deployment" "other" :release "other")
                              (object "Deployment" "by-hand")]
               "statefulsets" [sts]}
        workloads (storage/release-workloads "podinfo" "apps" lists)
        pvcs [(object "PersistentVolumeClaim" "cache" :release "podinfo")
              (object "PersistentVolumeClaim" "data-db-0")
              (object "PersistentVolumeClaim" "data-db-1")
              (object "PersistentVolumeClaim" "data-dbx-0")
              (object "PersistentVolumeClaim" "unrelated")]]
    (is (= #{["deployments" "web"] ["statefulsets" "db"]} (set (map (juxt :type :name) workloads))))
    (is (= ["cache" "data-db-0" "data-db-1"]
           (sort (map :name (storage/release-volumes "podinfo" "apps" pvcs [sts])))))))

;; --- the plan order ---------------------------------------------------------------

(def ^:private observed
  {:release (assoc release :status {})
   :release-name "podinfo" :release-namespace "apps" :storage-namespace "apps"
   :owner {:kind "Kustomization" :type flux/kustomization-type :namespace "flux-system" :name "apps"
           :object {:spec {:interval "5m"}}}
   :workloads [{:type "deployments" :namespace "apps" :name "web"}]
   :volumes [{:type "persistentvolumeclaims" :namespace "apps" :name "cache"}]})

(defn- plan-of [step]
  (vec (flatten ((:effect step) {:context "ctx" :target "apps/podinfo" :observed observed :timestamp "T"}))))

(deftest the-owner-is-suspended-before-anything-is-deleted-and-resumed-last
  (let [steps (storage/steps)
        changes (filterv :effect steps)
        plans (mapv plan-of changes)
        index-of (fn [pred] (first (keep-indexed #(when (pred %2) %1) plans)))
        deletes? (fn [plan] (some #{"delete"} plan))
        mentions (fn [s] (fn [plan] (some #(and (string? %) (str/includes? % s)) plan)))
        first-delete (index-of deletes?)]
    (is (some? first-delete))
    (is (< (index-of (every-pred (mentions "flux-system") (mentions "\"suspend\":true"))) first-delete)
        "the owner's suspension comes before the first delete")
    (is (< (index-of (every-pred (mentions "podinfo") (mentions "\"suspend\":true"))) first-delete)
        "the HelmRelease is suspended before the first delete")
    (is (< (index-of (mentions "deployments")) (index-of (mentions "persistentvolumeclaims")))
        "workloads go before volumes")
    (is (< (index-of (mentions "persistentvolumeclaims")) (index-of (mentions "owner=helm")))
        "the storage goes last among the deletes")
    (is (= (dec (count plans)) (index-of (every-pred (mentions "flux-system") (mentions "\"suspend\":false"))))
        "resuming the owner is the final change")
    (testing "a step that deletes refuses while an action is in flight"
      (doseq [step changes
              :when (deletes? (plan-of step))]
        (is (fn? (:precondition step)) (:title step))))))

(deftest every-wait-comes-from-live-values
  (let [w (storage/waits observed)]
    (is (= 180 (:in-flight w)) "the HelmRelease's spec.timeout")
    (is (= 180 (:deletion w)))
    (is (= (+ 180 600) (:release-ready w)) "spec.timeout plus spec.interval")
    (is (= (+ 300 180) (:owner-ready w)) "the owner's interval plus spec.timeout")))

(deftest help-names-the-owner-suspend-and-resume
  (let [titles (map :title (storage/steps))]
    (is (some #(re-find #"(?i)suspend the owner" %) titles))
    (is (some #(re-find #"(?i)resume the owner" %) titles))))
