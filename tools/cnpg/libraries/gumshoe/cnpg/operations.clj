;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.operations
  "The decisions behind the switchover and fencing runbooks, pure over Cluster
   objects so every refusal is testable.

   Fencing is the cnpg.io/fencedInstances annotation on the Cluster: a JSON
   list of instance names, sorted, where \"*\" fences every instance. Adding
   \"*\" replaces the list, adding a name while \"*\" stands changes nothing,
   and lifting one name while \"*\" stands is refused, because it cannot be
   expressed. An empty list removes the annotation."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(def fence-annotation "cnpg.io/fencedInstances")
(def everything "*")

(defn fenced
  "The fenced instance names of a cluster, or nil when the annotation is not a
   JSON list of strings."
  [cluster]
  (let [raw (get-in cluster [:metadata :annotations (keyword fence-annotation)])]
    (if (str/blank? raw)
      #{}
      (try
        (let [parsed (json/parse-string raw)]
          (when (and (sequential? parsed) (every? string? parsed)) (set parsed)))
        (catch Exception _ nil)))))

(defn fenced?
  [cluster instance]
  (let [names (fenced cluster)]
    (boolean (or (contains? names everything) (contains? names instance)))))

(defn with-fence
  "The fenced set after fencing instance."
  [names instance]
  (cond
    (= everything instance) #{everything}
    (contains? names everything) names
    :else (conj names instance)))

(defn without-fence
  "The fenced set after lifting the fence on instance, or nil when that cannot
   be expressed: one instance out of a fence on all of them."
  [names instance]
  (cond
    (= everything instance) #{}
    (contains? names everything) nil
    :else (disj names instance)))

(defn annotation-arg
  "The kubectl annotate argument that writes a fenced set. nil is a change
   that cannot be expressed, and reading it as an empty set would lift every
   fence, so it throws."
  [names]
  (when (nil? names)
    (throw (ex-info "no fenced set to write" {})))
  (if (empty? names)
    (str fence-annotation "-")
    (str fence-annotation "=" (json/generate-string (vec (sort names))))))

(defn instances
  "Every instance a cluster lists in status.instancesStatus, sorted."
  [cluster]
  (vec (sort (distinct (mapcat val (-> cluster :status :instancesStatus))))))

(defn healthy-instances
  [cluster]
  (set (get-in cluster [:status :instancesStatus :healthy])))

(defn- cluster-key [cluster] (kubectl/namespace-name-of cluster))

(defn parse-pick
  "\"<namespace>/<cluster> <instance>\" as {:namespace :cluster :instance}, or nil."
  [pick]
  (when-let [[_ namespace cluster instance] (re-matches #"\s*([^/\s]+)/(\S+)\s+(\S+)\s*" (str pick))]
    {:namespace namespace :cluster cluster
     ;; a bare number names the instance as the cnpg plugin does: <cluster>-<n>
     :instance (if (re-matches #"\d+" instance) (str cluster "-" instance) instance)}))

;; --- switchover ------------------------------------------------------------------

(defn switchover-refusal
  "Why instance must not become the primary of cluster, or nil."
  [cluster instance]
  (let [status (:status cluster)]
    (cond
      (nil? cluster) "the cluster no longer exists"
      (= instance (:currentPrimary status)) (format "%s is the primary already" instance)
      (not= (:currentPrimary status) (:targetPrimary status))
      (format "a switchover to %s is under way already" (:targetPrimary status))
      (fenced? cluster instance) (format "%s is fenced and cannot be promoted" instance)
      (not (contains? (healthy-instances cluster) instance))
      (format "%s is not among the healthy instances, so promoting it may lose data or fail" instance)
      :else nil)))

(defn switchover-picks
  "Every healthy, unfenced replica of every cluster, as a pick."
  [clusters]
  (vec (for [cluster (sort-by cluster-key (kubectl/items-of clusters))
             instance (sort (healthy-instances cluster))
             :when (nil? (switchover-refusal cluster instance))]
         (str (cluster-key cluster) " " instance))))

(defn promote-args
  "The cnpg plugin call that promotes an instance. The plugin binary is run
   directly: it registers the kubeconfig flags itself, so the context reaches
   it without depending on how kubectl forwards flags to a plugin."
  [context {:keys [namespace cluster instance]}]
  ["kubectl-cnpg" "promote" cluster instance (str "--context=" context) (str "--namespace=" namespace)])

;; --- fencing ---------------------------------------------------------------------

(defn fence-refusal
  [cluster instance]
  (let [names (fenced cluster)]
    (cond
      (nil? cluster) "the cluster no longer exists"
      (nil? names) (format "the %s annotation is not a JSON list of names - fix it by hand" fence-annotation)
      (fenced? cluster instance) (format "%s is fenced already" instance)
      (and (not= everything instance) (not (some #{instance} (instances cluster))))
      (format "%s is not an instance of this cluster" instance)
      :else nil)))

(defn unfence-refusal
  [cluster instance]
  (let [names (fenced cluster)]
    (cond
      (nil? cluster) "the cluster no longer exists"
      (nil? names) (format "the %s annotation is not a JSON list of names - fix it by hand" fence-annotation)
      (and (contains? names everything) (not= everything instance))
      "every instance is fenced with \"*\" - lift the fence on all of them, then fence the others again"
      (not (contains? names instance)) (format "%s is not fenced" instance)
      :else nil)))

(defn fence-picks
  [clusters]
  (vec (for [cluster (sort-by cluster-key (kubectl/items-of clusters))
             instance (conj (instances cluster) everything)
             :when (nil? (fence-refusal cluster instance))]
         (str (cluster-key cluster) " " instance))))

(defn unfence-picks
  [clusters]
  (vec (for [cluster (sort-by cluster-key (kubectl/items-of clusters))
             instance (sort (or (fenced cluster) #{}))]
         (str (cluster-key cluster) " " instance))))
