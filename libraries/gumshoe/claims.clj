;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.claims
  "Who holds a PersistentVolumeClaim. A claim is held, and must be neither
   offered for deletion nor deleted, when any of these refers to it:

   - a pod that exists, through a persistentVolumeClaim volume or a generic
     ephemeral volume;
   - a StatefulSet, by the name its volumeClaimTemplates give a claim,
     <template>-<statefulset>-<ordinal>, at any ordinal: the controller
     reuses a claim by name when it scales up again;
   - an owner reference on the claim;
   - the pod template of a Deployment, StatefulSet, DaemonSet, ReplicaSet,
     Job or CronJob, whether it runs pods now or is scaled to 0 or
     suspended.

   The judgement needs every one of these reads. A read that did not answer
   leaves the claim's holders unknown, and an unknown holder is never read
   as none. A StatefulSet whose persistentVolumeClaimRetentionPolicy deletes
   claims on scale-down still holds the claims it left, by name: deleting
   one of those is left to the operator."
  (:require [clojure.string :as str]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.kubectl :as kubectl]))

(def workload-types
  "Every resource type whose objects can hold a claim, with the path to the
   pod spec its objects carry."
  [{:type "pods" :kind "Pod" :pod-spec [:spec]}
   {:type "statefulsets" :kind "StatefulSet" :pod-spec [:spec :template :spec]}
   {:type "deployments" :kind "Deployment" :pod-spec [:spec :template :spec]}
   {:type "daemonsets" :kind "DaemonSet" :pod-spec [:spec :template :spec]}
   {:type "replicasets" :kind "ReplicaSet" :pod-spec [:spec :template :spec]}
   {:type "jobs" :kind "Job" :pod-spec [:spec :template :spec]}
   {:type "cronjobs" :kind "CronJob" :pod-spec [:spec :jobTemplate :spec :template :spec]}])

(def holder-types
  "The resource types the judgement reads, in the order it reports them."
  (mapv :type workload-types))

;; --- StatefulSet claims --------------------------------------------------------------

(defn template-names
  [statefulset]
  (keep #(-> % :metadata :name) (-> statefulset :spec :volumeClaimTemplates)))

(defn template-ordinal
  "The ordinal n when claim-name is <template>-<statefulset>-<n> as the
   StatefulSet controller writes it, for template-name or, without one, any
   of the StatefulSet's volumeClaimTemplates; nil otherwise."
  ([statefulset claim-name]
   (some #(template-ordinal statefulset % claim-name) (template-names statefulset)))
  ([statefulset template-name claim-name]
   (let [prefix (str template-name "-" (kubectl/name-of statefulset) "-")
         claim-name (str claim-name)]
     (when (and (str/starts-with? claim-name prefix)
                (re-matches #"0|[1-9]\d*" (subs claim-name (count prefix))))
       (parse-long (subs claim-name (count prefix)))))))

(defn selector-labelled?
  "Whether claim carries every label of the StatefulSet's
   selector.matchLabels, which the controller copies onto each claim it
   creates. A StatefulSet without matchLabels labels nothing."
  [statefulset claim]
  (let [match (-> statefulset :spec :selector :matchLabels)
        labels (-> claim :metadata :labels)]
    (boolean (and (seq match) (every? (fn [[k v]] (= v (get labels k))) match)))))

(defn owned-by?
  "Whether one of claim's owner references names owner, of kind, by name and,
   when both carry one, by uid."
  [kind owner claim]
  (let [uid (-> owner :metadata :uid)]
    (boolean (some #(and (= kind (:kind %))
                         (= (kubectl/name-of owner) (:name %))
                         (or (nil? uid) (nil? (:uid %)) (= uid (:uid %))))
                   (-> claim :metadata :ownerReferences)))))

(defn statefulset-claim?
  "Whether claim belongs to template-name of statefulset: in its namespace,
   named for one of its ordinals, and labelled with its selector or owned by
   it."
  [statefulset template-name claim]
  (boolean (and (= (kubectl/namespace-of statefulset) (kubectl/namespace-of claim))
                (template-ordinal statefulset template-name (kubectl/name-of claim))
                (or (selector-labelled? statefulset claim)
                    (owned-by? "StatefulSet" statefulset claim)))))

;; --- holders -------------------------------------------------------------------------

(defn unread
  "The holder types reads (type -> kubectl/get-json-result) has no answered
   read for, in holder-types order; nil when every one answered."
  [reads]
  (seq (for [type holder-types :when (not (:ok? (get reads type)))] type)))

(defn index
  "{namespace {type [object ..]}} over the documents of reads, so judging
   many claims walks each object once."
  [reads]
  (reduce (fn [acc type]
            (reduce (fn [acc object] (update-in acc [(kubectl/namespace-of object) type] (fnil conj []) object))
                    acc (kubectl/items-of (:doc (get reads type)))))
          {} holder-types))

(defn- state-of
  [kind object]
  (cond
    (and (#{"StatefulSet" "Deployment" "ReplicaSet"} kind) (= 0 (-> object :spec :replicas))) " (scaled to 0)"
    (and (#{"CronJob" "Job"} kind) (true? (-> object :spec :suspend))) " (suspended)"
    :else ""))

(defn- template-claims
  [pod-spec]
  (set (keep #(-> % :persistentVolumeClaim :claimName) (:volumes pod-spec))))

(defn holders
  "Why claim is in use, one phrase per holder, judged over indexed (see
   index); empty when nothing holds it. Pods come first, in the words the
   refusal has always used."
  [indexed claim]
  (let [namespace (kubectl/namespace-of claim)
        name (kubectl/name-of claim)
        objects (get indexed namespace)
        target (str namespace "/" name)
        pods (sort (for [pod (get objects "pods")
                         :when (contains? (kubectl/claimed-pvcs {:items [pod]}) target)]
                     (kubectl/name-of pod)))]
    (vec
     (concat
      (when (seq pods) [(str "used by pod(s) " (str/join ", " pods))])
      (for [{:keys [kind name]} (-> claim :metadata :ownerReferences)]
        (format "owned by %s %s" kind name))
      (for [statefulset (get objects "statefulsets")
            :let [ordinal (template-ordinal statefulset name)]
            :when ordinal]
        (format "kept by StatefulSet %s/%s%s as the claim of its ordinal %d"
                namespace (kubectl/name-of statefulset) (state-of "StatefulSet" statefulset) ordinal))
      (for [{:keys [type kind pod-spec]} (rest workload-types)
            object (get objects type)
            :when (contains? (template-claims (get-in object pod-spec)) name)]
        (format "named in the pod template of %s %s/%s%s"
                kind namespace (kubectl/name-of object) (state-of kind object)))))))

(defn unused
  "Namespace/name of every claim of pvcs (a list document) nothing holds,
   judged over reads (type -> kubectl/get-json-result). Throws when a read
   did not answer: without it a held claim would look unused."
  [pvcs reads]
  (when-let [missing (unread reads)]
    (throw (ex-info (format "could not read the %s of the cluster; refusing to call any claim unused"
                            (str/join ", " missing))
                    {:unread (vec missing)})))
  (let [indexed (index reads)]
    (vec (for [claim (kubectl/items-of pvcs)
               :when (empty? (holders indexed claim))]
           (kubectl/namespace-name-of claim)))))

(defn read-holders
  "type -> kubectl/get-json-result for every holder type and the types in
   extra, read at once, in namespace or, when it is nil, in every namespace."
  ([context namespace] (read-holders context namespace []))
  ([context namespace extra]
   (let [scope (if namespace (str "--namespace=" namespace) "--all-namespaces")]
     (concurrent/values (into {} (for [type (concat holder-types extra)]
                                   [type #(kubectl/get-json-result context type scope)]))))))

(defn unused-candidates
  "Namespace/name of every claim nothing holds, for a book that offers them
   for deletion. Throws when a read did not answer."
  [context]
  (let [reads (read-holders context nil ["persistentvolumeclaims"])
        pvcs (get reads "persistentvolumeclaims")]
    (when-not (:ok? pvcs)
      (throw (ex-info "could not read the PersistentVolumeClaims of the cluster; refusing to call any claim unused"
                      {:unread ["persistentvolumeclaims"]})))
    (unused (:doc pvcs) reads)))
