;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.workloads
  "The decisions behind the remediation runbooks for workload controllers,
   pure over the objects so every refusal and every completion test is
   testable. A pick names one workload as \"<Kind> <namespace>/<name>\"."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(def types
  "The workload controllers, by resource type and kind."
  [["deployments" "Deployment"] ["statefulsets" "StatefulSet"] ["daemonsets" "DaemonSet"]])

(def ^:private type-of-kind (into {} (map (fn [[type kind]] [kind type])) types))

(defn type-of [kind] (type-of-kind kind))

(defn pick
  [kind workload]
  (str kind " " (kubectl/namespace-name-of workload)))

(defn parse-pick
  "\"<Kind> <namespace>/<name>\" as {:kind :type :namespace :name}, or nil for
   another shape or an unknown kind."
  [value]
  (when-let [[_ kind namespace name] (re-matches #"\s*(\w+)\s+([^/\s]+)/(\S+)\s*" (str value))]
    (when-let [type (type-of kind)]
      {:kind kind :type type :namespace namespace :name name})))

(defn picks
  "Every workload of the listed kinds that pred accepts, as picks."
  [lists-by-kind pred]
  (vec (for [[kind list] lists-by-kind
             workload (sort-by kubectl/namespace-name-of (kubectl/items-of list))
             :when (pred kind workload)]
         (pick kind workload))))

(defn desired-replicas
  "How many pods the workload wants: spec.replicas (1 when unset) for a
   Deployment or StatefulSet, the scheduled count for a DaemonSet."
  [kind workload]
  (if (= "DaemonSet" kind)
    (or (-> workload :status :desiredNumberScheduled) 0)
    (or (-> workload :spec :replicas) 1)))

(defn rolled-out?
  "Whether the controller has observed the newest spec and every pod runs it
   and is available."
  [kind workload]
  (let [{:keys [metadata spec status]} workload
        observed? (>= (or (:observedGeneration status) 0) (or (:generation metadata) 0))
        wanted (desired-replicas kind workload)]
    (boolean
     (and workload observed?
          (case kind
            "Deployment" (and (= wanted (or (:updatedReplicas status) 0))
                              (= wanted (or (:availableReplicas status) 0))
                              (= wanted (or (:replicas status) 0)))
            "StatefulSet" (and (= wanted (or (:updatedReplicas status) 0))
                               (= wanted (or (:readyReplicas status) 0))
                               (or (nil? (:updateRevision status))
                                   (= (:currentRevision status) (:updateRevision status))
                                   (= "OnDelete" (-> spec :updateStrategy :type))))
            "DaemonSet" (and (= wanted (or (:updatedNumberScheduled status) 0))
                             (= wanted (or (:numberAvailable status) 0)))
            false)))))

;; --- rollout restart -----------------------------------------------------------

(defn restart-refusal
  "Why restarting the workload would do nothing useful, or nil."
  [kind workload]
  (cond
    (nil? workload) "the workload no longer exists"
    (and (= "Deployment" kind) (true? (-> workload :spec :paused)))
    "the Deployment is paused, so a restart waits until it is resumed - resume it first"
    (and (= "StatefulSet" kind) (= "OnDelete" (-> workload :spec :updateStrategy :type)))
    "the StatefulSet updates OnDelete, so a restart replaces no pod until each is deleted by hand"
    (zero? (desired-replicas kind workload)) "it runs no pods, so there is nothing to restart"
    :else nil))

(defn restart-args
  [{:keys [type namespace name]}]
  [(str "--namespace=" namespace) "rollout" "restart" (str (str/replace type #"s$" "") "/" name)])
