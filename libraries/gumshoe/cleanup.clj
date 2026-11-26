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
            [gumshoe.claims :as claims]
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

(def deletion-condition-types
  "The conditions the namespace controller writes while it empties a
   Terminating namespace."
  ["NamespaceContentRemaining" "NamespaceFinalizersRemaining" "NamespaceDeletionContentFailure"
   "NamespaceDeletionDiscoveryFailure" "NamespaceDeletionGroupVersionParsingFailure"])

(defn deletion-conditions
  "The namespace's deletion conditions, in the order of deletion-condition-types."
  [namespace]
  (let [by-type (into {} (map (juxt :type identity)) (-> namespace :status :conditions))]
    (keep by-type deletion-condition-types)))

(defn content-blockers
  "The deletion conditions that hold. Each of them means content may remain:
   objects left or held by finalizers, a delete that failed, or a group the
   controller could not enumerate at all, whose objects it cannot count."
  [namespace]
  (filter #(= "True" (:status %)) (deletion-conditions namespace)))

(defn deletion-condition-lines
  "The deletion conditions as the confirmation shows them."
  [namespace]
  (if-let [conditions (seq (deletion-conditions namespace))]
    (for [{:keys [type status message]} conditions]
      (str type " " status (when-not (str/blank? message) (str ": " message))))
    ["the namespace reports no deletion conditions"]))

(defn content-lines
  "What a namespace-objects read found, as the confirmation shows it."
  [{:keys [ok? objects reason]}]
  (cond
    (not ok?) [(str "could not list the objects left in it: " reason)]
    (empty? objects) ["no object is left in it"]
    :else (cons (format "%d object(s) left in it:" (count objects))
                (map #(str "  " %) objects))))

(defn namespace-items
  "The confirmation items of a forced finalization: the namespace, its
   deletion conditions and the objects left in it."
  [name namespace contents]
  (concat [name] (deletion-condition-lines namespace) (content-lines contents)))

(defn remaining-content
  "Why objects may remain in the namespace, or nil when its listing answered
   empty and no deletion condition holds. A listing that did not answer is a
   reason too, since nothing then shows the namespace is empty."
  [namespace {:keys [ok? objects reason]}]
  (let [blockers (map :type (content-blockers namespace))]
    (cond
      (not ok?) (str "the objects in it could not be listed (" reason ")")
      (seq objects) (format "%d object(s) are left in it" (count objects))
      (seq blockers) (str (str/join ", " blockers) " hold")
      :else nil)))

(defn orphan-refusal
  "Why the namespace must not be finalized while objects may remain in it,
   unless the operator acknowledged orphaning them. The finalize subresource
   removes the Namespace without deleting what is left in it: those objects
   stay in etcd, their controllers never clean up, and a namespace created
   later under the same name exposes them again."
  [name namespace contents acknowledged?]
  (when-not acknowledged?
    (when-let [why (remaining-content namespace contents)]
      (format (str "namespace %s may still hold objects: %s. Forcing its finalization leaves them "
                   "in etcd without cleanup, and a namespace created later under this name exposes "
                   "them again; pass --orphan-remaining to accept that")
              name why))))

;; --- Released PersistentVolumes -----------------------------------------------------

(defn released?
  [volume]
  (= "Released" (-> volume :status :phase)))

(defn reclaim-policy
  "The reclaim policy of a volume, or \"Unknown\" when no read found the
   volume or it names none."
  [volume]
  (or (-> volume :spec :persistentVolumeReclaimPolicy) "Unknown"))

(defn read-claim-volume
  "The PersistentVolume claim is bound to, read without strict reads:
   {:volume <the volume>}, {:volume nil} when a read answered that it does
   not exist, {:unread true} when no read answered, and nil for a claim bound
   to none. The read is lenient so that an operator without the right to
   read PersistentVolumes is told the reclaim policy is unknown rather than
   refused."
  [context claim]
  (when-let [volume-name (not-empty (-> claim :spec :volumeName))]
    (binding [kubectl/*strict-reads* false]
      (let [{:keys [ok? doc]} (kubectl/get-json-result context "persistentvolumes" volume-name)]
        (cond
          ok? {:volume doc}
          (= :absent (kubectl/presence context {:type "persistentvolumes" :name volume-name})) {:volume nil}
          :else {:unread true})))))

(defn claim-volume-line
  "The confirmation line saying what deleting claim does to the volume it is
   bound to, from volume-read as read-claim-volume returns it. Under the
   Delete reclaim policy the volume and the data on it are destroyed with
   the claim, which every book deleting a claim states the same way."
  [claim {:keys [volume unread] :as volume-read}]
  (let [volume-name (-> claim :spec :volumeName)
        policy (reclaim-policy volume)]
    (cond
      (str/blank? volume-name)
      "the claim is bound to no volume, so no data is deleted with it"

      unread
      (format "the reclaim policy of its volume %s could not be read - under Delete the volume and the data on it are destroyed with the claim"
              volume-name)

      (and (contains? volume-read :volume) (nil? volume))
      (format "its volume %s no longer exists" volume-name)

      (= "Delete" policy)
      (format "its volume %s has reclaim policy Delete - the volume and the data on it are destroyed with the claim" volume-name)

      (= "Retain" policy)
      (format "its volume %s has reclaim policy Retain - the volume and its data stay, as a Released PersistentVolume" volume-name)

      (= "Recycle" policy)
      (format "its volume %s has reclaim policy Recycle - the data on the volume is scrubbed" volume-name)

      :else
      (format "its volume %s has reclaim policy %s - the data on it may be destroyed with the claim" volume-name policy))))

(defn read-claim-volume-line
  "claim-volume-line for the claim namespace/name, read now without strict
   reads, or a line saying the reclaim policy is unknown when the claim
   itself could not be read."
  [context namespace name]
  (binding [kubectl/*strict-reads* false]
    (let [{:keys [ok? doc]} (kubectl/get-json-result context (str "--namespace=" namespace) "persistentvolumeclaims" name)]
      (if ok?
        (claim-volume-line doc (read-claim-volume context doc))
        (format "claim %s/%s could not be read, so the reclaim policy of its volume is unknown - under Delete the volume and the data on it are destroyed with the claim"
                namespace name)))))

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
  "Why the claim of ref must not be deleted, judged on the claim and what
   could hold it in its namespace as read now, `reads` as
   gumshoe.claims/read-holders returns them (type ->
   kubectl/get-json-result): the claim no longer exists, a read did not
   answer so whether something holds it is unknown, or what holds it, named
   (see gumshoe.claims/holders). A pods read alone, as
   kubectl/get-json-result returns it, leaves every other holder unread and
   so refuses."
  [{:keys [namespace name] :as ref} claim reads]
  (let [claim-name (describe "PersistentVolumeClaim" ref)]
    (cond
      (nil? claim) (str claim-name " no longer exists")
      (claims/unread reads)
      (format "the %s of namespace %s could not be read, so whether one uses %s is unknown"
              (str/join ", " (claims/unread reads)) namespace claim-name)
      :else (when-let [holders (seq (claims/holders (claims/index reads)
                                                         (update claim :metadata merge {:namespace namespace :name name})))]
              (format "%s is %s" claim-name (str/join "; " holders))))))
