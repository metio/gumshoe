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

(def ^:private deletion-condition-types
  "The five conditions the namespace controller writes on a Terminating
   namespace."
  (set (concat (keys blocking) (keys remaining))))

(def ^:private unreconciled
  "A Terminating namespace past the grace that still carries a spec finalizer
   while no deletion condition holds: the namespace controller of
   kube-controller-manager has not finished it. A controller that runs writes
   its conditions on its first pass and removes the `kubernetes` finalizer once
   the content is gone, so a namespace past the grace with neither step done
   points at the controller itself, which nothing in the namespace's content
   shows. It is a warning: one namespace cannot tell a stopped controller from a
   long backlog, and the control-plane check reports a kube-controller-manager
   that is down as critical on its own."
  {:severity :warning
   :why "the namespace controller has recorded no progress"
   :fix (str "the namespace controller in kube-controller-manager owns this deletion - check that "
             "kube-controller-manager runs and holds its leader lease (the control-plane check of the "
             "platform scan), that its --controllers flag does not disable the namespace controller "
             "(\"-namespace\"), and what its logs say about this namespace; a long deletion backlog "
             "delays it the same way. Do not remove the namespace's finalizer, which orphans its "
             "content in etcd")})

(def ^:private held-by-metadata
  "A Terminating namespace past the grace whose spec finalizers are gone, so the
   namespace controller has finished, but whose metadata.finalizers remain:
   another controller holds it."
  {:severity :warning
   :why "a finalizer on the namespace itself has not returned"
   :fix (str "metadata.finalizers names them - check that the controller owning each one runs; "
             "remove one by hand only once its controller is gone for good")})

(defn- silent-grade
  "The grade for a Terminating namespace on which none of the five deletion
   conditions holds, or nil while it is within terminating-grace. Without an age
   it is reported, as FinalizersRemaining is, because a controller that never
   reconciles leaves no age-independent trace. Conditions present with status
   False (or Unknown) count as absent: they record a pass that is over, and a
   namespace still Terminating past the grace after it is as stuck as one the
   controller never touched."
  [namespace past]
  (when (or (true? past) (= :unknown past))
    (if (and (empty? (-> namespace :spec :finalizers))
             (seq (-> namespace :metadata :finalizers)))
      held-by-metadata
      unreconciled)))

(defn- silent-finding
  "The finding for a namespace no deletion condition explains. Its :condition
   is nil, because no condition holds; the hint says whether the controller
   wrote no condition at all or only conditions that do not read True."
  [namespace past since]
  (when-let [{:keys [severity why fix] :as grade} (silent-grade namespace past)]
    (let [written (filter #(deletion-condition-types (:type %)) (-> namespace :status :conditions))
          meta-finalizers (seq (-> namespace :metadata :finalizers))
          state (cond (identical? held-by-metadata grade)
                      (str "no deletion condition holds and metadata.finalizers remain: "
                           (render/cut message-limit (render/printable (str/join ", " meta-finalizers))))
                      (empty? written) "no deletion condition written"
                      :else "no deletion condition reads True")]
      (onset/since
       {:severity severity
        :component (kubectl/name-of namespace)
        :summary (format "namespace cannot finish terminating: %s" why)
        :condition nil
        :hint (str state
                   (when since (str " -- deleting since " since))
                   " - " fix)}
       since))))

(defn- namespace-findings
  [namespace now]
  (let [past (past-grace? namespace now)
        since (some-> namespace :metadata :deletionTimestamp onset/instant str)
        held (filter #(and (= "True" (:status %)) (deletion-condition-types (:type %)))
                     (-> namespace :status :conditions))
        found (for [condition held
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
    (if (empty? held)
      (some-> (silent-finding namespace past since) list)
      (for [[condition-type finding] found
            :when (not (and explained? (= "NamespaceContentRemaining" condition-type)))]
        finding))))

(defn detect-stuck-namespaces
  "Terminating namespaces whose conditions say the deletion cannot finish
   (critical at once), or that content or finalizers still remain past
   terminating-grace (warning). Content remaining is reported only when no
   other condition explains why. A namespace on which none of the five
   conditions holds past terminating-grace is reported too (warning, :condition
   nil): either the namespace controller has not reconciled it, or a finalizer
   in its metadata holds it. Each condition finding names its :condition and
   quotes the condition's :message, sanitised and bounded."
  [evidence]
  (mapcat #(namespace-findings % (:now evidence))
          (filter #(= "Terminating" (-> % :status :phase))
                  (kubectl/items-of (get evidence "namespaces")))))

(def detectives
  [{:name "terminating-namespaces"
    :description "Namespaces that cannot finish terminating, and what refuses"
    :requires ["namespaces"]
    :detect detect-stuck-namespaces}])
