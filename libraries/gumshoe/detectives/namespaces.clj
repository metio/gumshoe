;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.namespaces
  "Detectives for namespace lifecycle: a namespace that cannot finish
   terminating. The namespace controller records why in the namespace's own
   conditions, which is the whole diagnosis - enumerating every API type in the
   namespace costs one request per type and says less."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]
            [gumshoe.render :as render]))

(def ^:private blocking
  "The conditions that mean the deletion cannot finish however long it waits:
   the controller cannot enumerate the content, cannot parse a group version,
   or was refused a delete. Each is reported as soon as it holds, with what to
   repair."
  {"NamespaceDeletionDiscoveryFailure"
   {:severity :critical
    :why "an API cannot be reached, so the controller cannot enumerate content"
    :fix (str "the message names the group versions discovery could not reach - repair the aggregated "
              "APIService serving them (the api-services check of the platform scan) or the webhook "
              "Service behind them (webhook-backends); the deletion resumes once discovery answers")}
   "NamespaceDeletionGroupVersionParsingFailure"
   {:severity :critical
    :why "a CRD version cannot be parsed"
    :fix (str "the message names the group version that does not parse - correct the "
              "CustomResourceDefinition or APIService declaring it (the crd-health check of the "
              "platform scan); waiting does not resolve it")}
   "NamespaceDeletionContentFailure"
   {:severity :critical
    :why "something refuses to be deleted"
    :fix (str "the message names the resource and its refusal - when an admission webhook or "
              "ValidatingAdmissionPolicy denies the delete, let it admit the namespace controller's "
              "deletes in a Terminating namespace (webhook-backends and webhook-references in the "
              "platform scan); do not remove the namespace's finalizer, which orphans its content in etcd")}})

(def ^:private remaining
  "The conditions every deletion passes through while content and its
   finalizers go away: the namespace controller sets them in the same pass as
   it deletes. Only one that still holds after terminating-grace is stuck."
  {"NamespaceFinalizersRemaining"
   {:severity :warning
    :why "a finalizer has not returned"
    :fix (str "the message names the finalizers - check that the controller owning each one runs and "
              "can reach what it cleans up; remove a finalizer by hand only once its controller is gone for good")}
   "NamespaceContentRemaining"
   {:severity :warning
    :why "content has not been deleted"
    :fix "the message lists the types still holding objects - read why those objects stay"}})

(def message-limit
  "The most columns of a condition's message a finding quotes. The message is
   written by the cluster, so it is bounded as well as stripped of control
   characters before it reaches a terminal."
  600)

(defn condition-message
  "The condition's message as a finding quotes it: escape sequences and control
   characters removed (gumshoe.render/printable) and cut to message-limit
   columns, or nil when it holds no text."
  [condition]
  (let [text (render/printable (:message condition))]
    (when-not (str/blank? text)
      (render/cut message-limit text))))

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
  "The grade {:severity :why :fix} for one condition, or nil. A blocking
   condition counts at once. A remaining condition counts only past the grace;
   without an age NamespaceFinalizersRemaining is still reported, as the safer
   reading, while NamespaceContentRemaining alone is not, because it holds
   through every deletion."
  [condition-type past]
  (or (get blocking condition-type)
      (when-let [grade (get remaining condition-type)]
        (cond (true? past) grade
              (and (= :unknown past) (= "NamespaceFinalizersRemaining" condition-type)) grade
              :else nil))))

(defn- namespace-findings
  [namespace now]
  (let [past (past-grace? namespace now)
        since (some-> namespace :metadata :deletionTimestamp onset/instant str)
        found (for [condition (-> namespace :status :conditions)
                    :when (= "True" (:status condition))
                    :let [{:keys [severity why fix]} (graded (:type condition) past)]
                    :when severity
                    :let [message (condition-message condition)]]
                [(:type condition)
                 (onset/since
                  (cond-> {:severity severity
                           :component (kubectl/name-of namespace)
                           :summary (format "namespace cannot finish terminating: %s" why)
                           :condition (:type condition)
                           :hint (str (:type condition)
                                      (when message (str ": " message))
                                      (when since (str " -- deleting since " since))
                                      " - " fix)}
                    message (assoc :message message))
                  since)])
        explained? (some #(not= "NamespaceContentRemaining" (first %)) found)]
    (for [[condition-type finding] found
          :when (not (and explained? (= "NamespaceContentRemaining" condition-type)))]
      finding)))

(defn detect-stuck-namespaces
  "Terminating namespaces whose conditions say the deletion cannot finish
   (critical at once), or that content or finalizers still remain past
   terminating-grace (warning). Content remaining is reported only when no
   other condition explains why. Each finding names its :condition and quotes
   the condition's :message, sanitised and bounded."
  [evidence]
  (mapcat #(namespace-findings % (:now evidence))
          (filter #(= "Terminating" (-> % :status :phase))
                  (kubectl/items-of (get evidence "namespaces")))))

(def detectives
  [{:name "terminating-namespaces"
    :description "Namespaces that cannot finish terminating, and what refuses"
    :requires ["namespaces"]
    :detect detect-stuck-namespaces}])
