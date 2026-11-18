;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.finalizers-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.terminating :as terminating]
            [gumshoe.finalizers :as finalizers]))

(deftest kubernetes-own-finalizers-are-protected
  (doseq [finalizer ["kubernetes.io/pvc-protection" "kubernetes.io/pv-protection" "foregroundDeletion" "orphan"
                     "kubernetes" "batch.kubernetes.io/job-tracking" "service.kubernetes.io/load-balancer-cleanup"
                     "snapshot.storage.k8s.io/volumesnapshot-as-source-protection"
                     "external-attacher/rbd-csi-ceph-com" "external-provisioner.volume.kubernetes.io/finalizer"]]
    (is (finalizers/protected? finalizer) finalizer))
  (doseq [finalizer ["cert-manager.io/cleanup" "example.com/backup" "cluster.cluster.x-k8s.io" "kubernetes.io.example.com/x"]]
    (is (not (finalizers/protected? finalizer)) finalizer)))

(deftest a-pick-names-type-object-and-finalizer
  (is (= {:type "configmaps" :namespace "shop" :name "web" :finalizer "example.com/cleanup"}
         (finalizers/parse-pick "configmaps shop/web example.com/cleanup")))
  (is (= {:type "persistentvolumes" :namespace nil :name "pv-1" :finalizer "example.com/x"}
         (finalizers/parse-pick "persistentvolumes pv-1 example.com/x")))
  (is (nil? (finalizers/parse-pick "configmaps shop/web"))))

(def ^:private deleting
  {:metadata {:namespace "shop" :name "web" :uid "u-1" :deletionTimestamp "2026-11-18T11:00:00Z"
              :finalizers ["kubernetes.io/pvc-protection" "example.com/cleanup"]}})

(deftest only-controller-finalizers-of-deleted-objects-are-offered
  (is (= ["persistentvolumeclaims shop/web example.com/cleanup"]
         (finalizers/picks [["persistentvolumeclaims"
                             {:items [deleting
                                      (update deleting :metadata dissoc :deletionTimestamp)]}]]))))

(deftest removal-is-refused-unless-sound
  (let [pick (finalizers/parse-pick "persistentvolumeclaims shop/web example.com/cleanup")]
    (is (nil? (finalizers/refusal deleting pick)))
    (is (= "the object no longer exists" (finalizers/refusal nil pick)))
    (is (str/starts-with? (finalizers/refusal deleting (assoc pick :finalizer "kubernetes.io/pvc-protection"))
                          "kubernetes.io/pvc-protection is Kubernetes' own finalizer"))
    (is (str/starts-with? (finalizers/refusal (update deleting :metadata dissoc :deletionTimestamp) pick)
                          "the object is not being deleted"))
    (is (= "the object no longer carries example.com/cleanup"
           (finalizers/refusal (assoc-in deleting [:metadata :finalizers] ["kubernetes.io/pvc-protection"]) pick)))))

(deftest the-patch-is-pinned-to-uid-and-index
  (let [args (finalizers/patch-args deleting (finalizers/parse-pick "persistentvolumeclaims shop/web example.com/cleanup"))
        patch (json/decode (subs (last args) (count "--patch=")) true)]
    (is (= ["--namespace=shop" "patch" "persistentvolumeclaims" "web" "--type=json"] (butlast args)))
    (is (= [{:op "test" :path "/metadata/uid" :value "u-1"}
            {:op "test" :path "/metadata/finalizers/1" :value "example.com/cleanup"}
            {:op "remove" :path "/metadata/finalizers/1"}]
           patch))))

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

(deftest objects-stuck-terminating-are-reported
  (let [claim (assoc-in deleting [:metadata :deletionTimestamp] "2026-11-18T11:45:00Z")
        user {:metadata {:namespace "shop" :name "web-0"}
              :spec {:volumes [{:name "data" :persistentVolumeClaim {:claimName "web"}}]}}
        [finding :as findings] (terminating/detect-stuck-terminating
                                {:now now "persistentvolumeclaims" {:items [claim]} "pods" {:items [user]}})]
    (is (= 1 (count findings)))
    (is (= :warning (:severity finding)))
    (is (= "shop/web" (:component finding)))
    (is (= "PersistentVolumeClaim is stuck terminating for 15 minutes, waiting on kubernetes.io/pvc-protection, example.com/cleanup"
           (:summary finding)))
    (is (str/includes? (:hint finding) "kubernetes.io/pvc-protection waits until no pod uses the claim - mounted by web-0"))
    (is (str/includes? (:hint finding) "--pick 'persistentvolumeclaims shop/web example.com/cleanup'"))
    (is (= {:kind "PersistentVolumeClaim" :namespace "shop" :name "web"} (:subject finding))))
  (testing "30 minutes is critical, and a cluster-scoped object is named bare"
    (let [[finding] (terminating/detect-stuck-terminating
                     {:now now "persistentvolumes" {:items [{:metadata {:name "pv-1" :deletionTimestamp "2026-11-18T11:00:00Z"
                                                                         :finalizers ["kubernetes.io/pv-protection"]}}]}})]
      (is (= :critical (:severity finding)))
      (is (= "pv-1" (:component finding)))
      (testing "with only protected finalizers no removal is suggested"
        (is (not (str/includes? (:hint finding) "remove.clj")))))))

(deftest a-fresh-delete-or-one-without-finalizers-is-quiet
  (is (empty? (terminating/detect-stuck-terminating
               {:now now "configmaps" {:items [(assoc-in deleting [:metadata :deletionTimestamp] "2026-11-18T11:55:00Z")
                                               (assoc-in deleting [:metadata :finalizers] [])]}}))))
