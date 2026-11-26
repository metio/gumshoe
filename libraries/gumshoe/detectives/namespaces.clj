;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.namespaces
  "Detectives for namespace lifecycle: a namespace that cannot finish
   terminating. The namespace controller records why in the namespace's own
   conditions, which is the whole diagnosis - enumerating every API type in the
   namespace costs one request per type and says less."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def ^:private blocking
  "The conditions that mean a deletion is refused rather than in flight. Graded
   by how little can be done about them from outside: a refusal names the
   refuser, a finalizer may still return."
  {"NamespaceDeletionContentFailure"
   [:critical "something refuses to be deleted"]
   "NamespaceDeletionDiscoveryFailure"
   [:critical "an API cannot be reached, so the controller cannot enumerate content"]
   "NamespaceDeletionGroupVersionParsingFailure"
   [:critical "a CRD version cannot be parsed"]})

(def ^:private remaining
  "The conditions every deletion passes through while content and its
   finalizers go away: the namespace controller sets them in the same pass as
   it deletes. Only one that still holds after terminating-grace is stuck."
  {"NamespaceFinalizersRemaining"
   [:warning "a finalizer has not returned"]
   "NamespaceContentRemaining"
   [:warning "content has not been deleted"]})

(def terminating-grace
  "How long a namespace may stay Terminating with content or finalizers
   remaining before that counts as stuck. A deletion that meets a
   pvc-protection or foregroundDeletion finalizer normally finishes within
   a few minutes."
  (java.time.Duration/ofMinutes 10))

(defn- past-grace?
  "Whether the namespace has been terminating longer than terminating-grace,
   or :unknown when :now or deletionTimestamp is missing."
  [namespace now]
  (let [since (onset/instant (-> namespace :metadata :deletionTimestamp))]
    (if (and since now)
      (.isAfter ^java.time.Instant now (.plus ^java.time.Instant since ^java.time.Duration terminating-grace))
      :unknown)))

(defn- graded
  "The [severity why] for one condition, or nil. A remaining condition counts
   only past the grace; without an age NamespaceFinalizersRemaining is still
   reported, as the safer reading, while NamespaceContentRemaining alone is
   not, because it holds through every deletion."
  [condition-type past]
  (or (get blocking condition-type)
      (when-let [grade (get remaining condition-type)]
        (cond (true? past) grade
              (and (= :unknown past) (= "NamespaceFinalizersRemaining" condition-type)) grade
              :else nil))))

(defn- namespace-findings
  [namespace now]
  (let [past (past-grace? namespace now)
        since (-> namespace :metadata :deletionTimestamp)
        found (for [condition (-> namespace :status :conditions)
                    :let [[severity why] (graded (:type condition) past)]
                    :when (and severity (= "True" (:status condition)))]
                [(:type condition)
                 (onset/since
                  {:severity severity
                   :component (kubectl/name-of namespace)
                   :summary (format "namespace cannot finish terminating: %s" why)
                   :hint (format "%s%s"
                                 (or (:message condition) (:type condition))
                                 (if since
                                   (str " -- deleting since " since)
                                   ""))}
                  since)])
        explained? (some #(not= "NamespaceContentRemaining" (first %)) found)]
    (for [[condition-type finding] found
          :when (not (and explained? (= "NamespaceContentRemaining" condition-type)))]
      finding)))

(defn detect-stuck-namespaces
  "Terminating namespaces whose conditions say the deletion is refused, or
   that content or finalizers still remain past terminating-grace. Content
   remaining is reported only when no other condition explains why."
  [evidence]
  (mapcat #(namespace-findings % (:now evidence))
          (filter #(= "Terminating" (-> % :status :phase))
                  (kubectl/items-of (get evidence "namespaces")))))

(def detectives
  [{:name "terminating-namespaces"
    :description "Namespaces that cannot finish terminating, and what refuses"
    :requires ["namespaces"]
    :detect detect-stuck-namespaces}])
