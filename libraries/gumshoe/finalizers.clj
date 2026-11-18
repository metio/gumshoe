;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.finalizers
  "What a finalizer on a deleted object waits for, and whether removing it by
   hand is ever sound. Kubernetes' own finalizers guard an invariant - a claim
   still mounted by a pod, dependents still alive - and removing one breaks that
   invariant, so they are never removable. Any other finalizer belongs to a
   controller that cleans something up before letting go; removing it skips the
   cleanup, which is sound only when that controller is gone for good."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(def ^:private known
  {"kubernetes.io/pvc-protection" "waits until no pod uses the claim"
   "kubernetes.io/pv-protection" "waits until the volume is no longer bound to a claim"
   "foregroundDeletion" "waits until the object's dependents are deleted"
   "orphan" "waits until the object's dependents are orphaned"
   "kubernetes" "waits until every resource in the namespace is deleted"})

(defn protected?
  "Whether a finalizer is Kubernetes' own: the garbage collector's, the
   kubernetes.io and *.k8s.io ones, and the CSI sidecars' external-attacher and
   external-provisioner finalizers."
  [finalizer]
  (boolean
   (or (contains? known finalizer)
       (re-find #"(^|\.)kubernetes\.io(/|$)" finalizer)
       (re-find #"(^|\.)k8s\.io(/|$)" finalizer)
       (re-find #"^external-(attacher|provisioner)" finalizer))))

(defn explain
  "What a finalizer waits for, as a phrase."
  [finalizer]
  (or (get known finalizer)
      (when (protected? finalizer) "is held by Kubernetes until its condition clears")
      "waits for its controller to finish cleaning up"))

(defn removal-patch
  "The JSON patch that removes the finalizer at `index`, refused by the API
   server unless the object still has `uid` and that finalizer still sits at
   that index - so a recreated object, or one whose finalizers moved since they
   were read, is left alone."
  [uid index finalizer]
  [{:op "test" :path "/metadata/uid" :value uid}
   {:op "test" :path (str "/metadata/finalizers/" index) :value finalizer}
   {:op "remove" :path (str "/metadata/finalizers/" index)}])

(defn parse-pick
  "A pick reads \"<type> <namespace>/<name> <finalizer>\", or \"<type> <name>
   <finalizer>\" for a cluster-scoped object. The finalizer may itself hold a
   slash, so it is the rest of the line."
  [pick]
  (let [[type object finalizer] (str/split (str/trim (str pick)) #"\s+" 3)
        [namespace name] (if (str/includes? (str object) "/") (str/split object #"/" 2) [nil object])]
    (when (and type name finalizer)
      {:type type :namespace namespace :name name :finalizer finalizer})))

(defn picks
  "Every removable finalizer on an object being deleted, as picks."
  [lists]
  (for [[type list] lists
        object (kubectl/items-of list)
        :when (-> object :metadata :deletionTimestamp)
        finalizer (-> object :metadata :finalizers)
        :when (not (protected? finalizer))]
    (format "%s %s %s" type
            (if (kubectl/namespace-of object) (kubectl/namespace-name-of object) (kubectl/name-of object))
            finalizer)))

(defn refusal
  "Why the finalizer must not be removed from this object, or nil."
  [object {:keys [finalizer]}]
  (cond
    (nil? object) "the object no longer exists"
    (protected? finalizer) (str finalizer " is Kubernetes' own finalizer - it " (explain finalizer))
    (nil? (-> object :metadata :deletionTimestamp)) "the object is not being deleted - its finalizer is doing its job"
    (not (some #{finalizer} (-> object :metadata :finalizers))) (str "the object no longer carries " finalizer)))

(defn patch-args
  "The kubectl arguments that remove the finalizer from the object as read."
  [object {:keys [type namespace name finalizer]}]
  (let [index (.indexOf ^java.util.List (vec (-> object :metadata :finalizers)) finalizer)]
    (concat (when namespace [(str "--namespace=" namespace)])
            ["patch" type name "--type=json"
             (str "--patch=" (json/encode (removal-patch (-> object :metadata :uid) index finalizer)))])))
