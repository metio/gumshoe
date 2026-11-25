;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cleanup
  "The decisions behind the books that delete or force out what is stuck:
   orders/recreate, namespaces/remove_finalizers, volumes/delete_released
   and volumes/delete_unused. Each state predicate decides both what the
   book offers and what it refuses once the operator confirmed, so the
   offer and the guard never disagree. Every refusal is pure over objects
   the engine read again after the confirmation, and returns the reason
   the change must not run, or nil."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(defn- describe
  [kind {:keys [namespace name]}]
  (str kind " " (when namespace (str namespace "/")) name))

;; --- ACME orders --------------------------------------------------------------------

(defn invalid-order?
  "Whether cert-manager has not reported the order valid, which is what
   orders/recreate deletes."
  [order]
  (not= "valid" (-> order :status :state)))

(defn order-refusal
  "Why the order of ref must not be deleted, judged on the order as read
   now: it no longer exists, or cert-manager has reported it valid."
  [ref order]
  (cond
    (nil? order) (str (describe "Order" ref) " no longer exists")
    (not (invalid-order? order)) (str (describe "Order" ref) " is valid now; it no longer needs to be recreated")
    :else nil))

;; --- namespaces stuck terminating ---------------------------------------------------

(defn terminating?
  [namespace]
  (= "Terminating" (-> namespace :status :phase)))

(defn namespace-refusal
  "Why the kubernetes finalizer of the namespace named `name` must not be
   removed, judged on the namespace as read now: it no longer exists, it is
   not Terminating, or it carries no kubernetes finalizer to remove."
  [name namespace]
  (cond
    (nil? namespace) (str "namespace " name " no longer exists")
    (not (terminating? namespace))
    (format "namespace %s is %s, not Terminating" name (or (-> namespace :status :phase) "in no phase"))
    (not (some #{"kubernetes"} (-> namespace :spec :finalizers)))
    (format "namespace %s carries no kubernetes finalizer" name)
    :else nil))

;; --- Released PersistentVolumes -----------------------------------------------------

(defn released?
  [volume]
  (= "Released" (-> volume :status :phase)))

(defn reclaim-policy
  "The reclaim policy of a volume, or \"Unknown\" when no read found the
   volume or it names none."
  [volume]
  (or (-> volume :spec :persistentVolumeReclaimPolicy) "Unknown"))

(defn volume-refusal
  "Why the volume of ref must not be deleted, judged on the volume as read
   now: it no longer exists, or it left Released (bound to a claim again,
   or Available)."
  [ref volume]
  (cond
    (nil? volume) (str (describe "PersistentVolume" ref) " no longer exists")
    (not (released? volume))
    (format "%s is %s, not Released" (describe "PersistentVolume" ref) (or (-> volume :status :phase) "in no phase"))
    :else nil))

(defn first-refusal
  "The first reason (refusal ref object) gives over refs and objects, in
   order, or nil."
  [refusal refs objects]
  (some (fn [[ref object]] (refusal ref object)) (map vector refs objects)))

;; --- unused PersistentVolumeClaims --------------------------------------------------

(defn pods-claiming
  "The names of the pods of pods-list that refer to the claim name in
   namespace, sorted."
  [pods-list namespace name]
  (let [claim (format "%s/%s" namespace name)]
    (sort (for [pod (kubectl/items-of pods-list)
                :when (contains? (kubectl/claimed-pvcs {:items [pod]}) claim)]
            (-> pod :metadata :name)))))

(defn claim-refusal
  "Why the claim of ref must not be deleted, judged on the claim and the
   pods of its namespace as read now, `pods` as kubectl/get-json-result
   returns them: the claim no longer exists, the pods could not be read so
   whether one uses it is unknown, or a pod uses it, named."
  [{:keys [namespace name] :as ref} claim {:keys [ok? doc]}]
  (let [claim-name (describe "PersistentVolumeClaim" ref)]
    (cond
      (nil? claim) (str claim-name " no longer exists")
      (not ok?) (format "the pods of namespace %s could not be read, so whether one uses %s is unknown" namespace claim-name)
      :else (when-let [users (seq (pods-claiming doc namespace name))]
              (format "%s is used by pod(s) %s" claim-name (str/join ", " users))))))
