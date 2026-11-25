;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cleanup-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.cleanup :as cleanup]))

(def ^:private order-ref {:type "orders.acme.cert-manager.io" :namespace "shop" :name "web-1"})

(defn- order [state] {:metadata {:namespace "shop" :name "web-1"} :status {:state state}})

(deftest an-order-is-recreated-until-cert-manager-reports-it-valid
  (doseq [state ["invalid" "errored" "pending" nil]]
    (testing (pr-str state)
      (is (cleanup/invalid-order? (order state)))
      (is (nil? (cleanup/order-refusal order-ref (order state))))))
  (is (not (cleanup/invalid-order? (order "valid"))))
  (is (= "Order shop/web-1 is valid now; it no longer needs to be recreated"
         (cleanup/order-refusal order-ref (order "valid"))))
  (is (= "Order shop/web-1 no longer exists" (cleanup/order-refusal order-ref nil))))

(defn- namespace-object
  [phase finalizers]
  {:metadata {:name "stuck"} :spec {:finalizers finalizers} :status {:phase phase}})

(deftest only-a-terminating-namespace-held-by-the-kubernetes-finalizer-is-forced
  (is (nil? (cleanup/namespace-refusal "stuck" (namespace-object "Terminating" ["kubernetes"]))))
  (is (= "namespace stuck no longer exists" (cleanup/namespace-refusal "stuck" nil)))
  (is (= "namespace stuck is Active, not Terminating"
         (cleanup/namespace-refusal "stuck" (namespace-object "Active" ["kubernetes"]))))
  (is (= "namespace stuck is in no phase, not Terminating"
         (cleanup/namespace-refusal "stuck" (namespace-object nil ["kubernetes"]))))
  (is (= "namespace stuck carries no kubernetes finalizer"
         (cleanup/namespace-refusal "stuck" (namespace-object "Terminating" ["example.com/other"]))))
  (is (= "namespace stuck carries no kubernetes finalizer"
         (cleanup/namespace-refusal "stuck" (namespace-object "Terminating" nil)))))

(defn- volume
  [phase policy]
  {:metadata {:name "pv-1"} :spec (cond-> {} policy (assoc :persistentVolumeReclaimPolicy policy)) :status {:phase phase}})

(deftest only-a-released-volume-is-deleted
  (let [ref {:type "persistentvolumes" :name "pv-1"}]
    (is (nil? (cleanup/volume-refusal ref (volume "Released" "Delete"))))
    (is (= "PersistentVolume pv-1 is Bound, not Released" (cleanup/volume-refusal ref (volume "Bound" "Delete"))))
    (is (= "PersistentVolume pv-1 is Available, not Released" (cleanup/volume-refusal ref (volume "Available" "Retain"))))
    (is (= "PersistentVolume pv-1 no longer exists" (cleanup/volume-refusal ref nil)))))

(deftest an-unread-reclaim-policy-is-unknown
  (is (= "Delete" (cleanup/reclaim-policy (volume "Released" "Delete"))))
  (is (= "Unknown" (cleanup/reclaim-policy (volume "Released" nil))))
  (is (= "Unknown" (cleanup/reclaim-policy nil))))

(deftest the-first-refusal-names-the-first-object-that-moved
  (let [refs [{:type "persistentvolumes" :name "pv-1"} {:type "persistentvolumes" :name "pv-2"} {:type "persistentvolumes" :name "pv-3"}]]
    (is (= "PersistentVolume pv-2 is Bound, not Released"
           (cleanup/first-refusal cleanup/volume-refusal refs
                                  [(volume "Released" "Delete") (volume "Bound" "Delete") nil])))
    (is (nil? (cleanup/first-refusal cleanup/volume-refusal refs (repeat 3 (volume "Released" "Retain")))))
    (is (nil? (cleanup/first-refusal cleanup/volume-refusal [] [])))))

(def ^:private claim-ref {:type "persistentvolumeclaims" :namespace "shop" :name "data"})

(defn- pod
  [name & volumes]
  {:metadata {:namespace "shop" :name name} :spec {:volumes (vec volumes)}})

(deftest a-claim-a-pod-uses-is-refused-naming-every-such-pod
  (let [pods {:items [(pod "web-1" {:name "data" :persistentVolumeClaim {:claimName "data"}})
                      (pod "cache" {:name "tmp" :emptyDir {}})
                      (pod "web-0" {:name "data" :persistentVolumeClaim {:claimName "data"}})]}]
    (is (= ["web-0" "web-1"] (cleanup/pods-claiming pods "shop" "data")))
    (is (= "PersistentVolumeClaim shop/data is used by pod(s) web-0, web-1"
           (cleanup/claim-refusal claim-ref {:metadata {}} {:ok? true :doc pods})))))

(deftest a-generic-ephemeral-volume-claims-the-claim-kubernetes-names-after-it
  (is (= ["job"] (cleanup/pods-claiming {:items [(pod "job" {:name "data" :ephemeral {}})]} "shop" "job-data"))))

(deftest an-unused-claim-is-deleted-only-when-the-pods-were-read
  (is (nil? (cleanup/claim-refusal claim-ref {:metadata {}} {:ok? true :doc {:items []}})))
  (is (= "the pods of namespace shop could not be read, so whether one uses PersistentVolumeClaim shop/data is unknown"
         (cleanup/claim-refusal claim-ref {:metadata {}} {:ok? false})))
  (is (= "PersistentVolumeClaim shop/data no longer exists"
         (cleanup/claim-refusal claim-ref nil {:ok? true :doc {:items []}}))))
