;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.detectives.workloads
  "Detectives for controllers: deployments, statefulsets, and daemonsets whose
   pods are not all up, and jobs that failed."
  (:require [infra.kubectl :as kubectl]))

(defn- replica-findings
  [resources kind]
  (for [resource resources
        :let [wanted (or (-> resource :spec :replicas) 0)
              ready (or (-> resource :status :readyReplicas) 0)]
        :when (< ready wanted)]
    {:severity (if (zero? ready) :critical :warning)
     :component (kubectl/namespace-name-of resource)
     :summary (format "%s has %d of %d replicas ready" kind ready wanted)}))

(defn detect-deployment-problems
  [evidence]
  (replica-findings (kubectl/items-of (get evidence "deployments")) "Deployment"))

(defn detect-statefulset-problems
  [evidence]
  (replica-findings (kubectl/items-of (get evidence "statefulsets")) "StatefulSet"))

(defn detect-daemonset-problems
  [evidence]
  (for [daemonset (kubectl/items-of (get evidence "daemonsets"))
        :let [unavailable (or (-> daemonset :status :numberUnavailable) 0)]
        :when (pos? unavailable)]
    {:severity :warning
     :component (kubectl/namespace-name-of daemonset)
     :summary (format "DaemonSet has %d unavailable pods" unavailable)}))

(defn detect-failed-jobs
  [evidence]
  (for [job (kubectl/items-of (get evidence "jobs"))
        :let [failed (first (filter #(and (= "Failed" (:type %)) (= "True" (:status %)))
                                    (-> job :status :conditions)))]
        :when failed]
    {:severity :warning
     :component (kubectl/namespace-name-of job)
     :summary (format "Job failed (%s)" (or (:reason failed) "unknown reason"))
     :hint (:message failed)}))

(def detectives
  [{:name "deployments"
    :description "Deployments with missing replicas"
    :requires ["deployments"]
    :detect detect-deployment-problems}
   {:name "statefulsets"
    :description "StatefulSets with missing replicas"
    :requires ["statefulsets"]
    :detect detect-statefulset-problems}
   {:name "daemonsets"
    :description "DaemonSets with unavailable pods"
    :requires ["daemonsets"]
    :detect detect-daemonset-problems}
   {:name "jobs"
    :description "Jobs that failed"
    :requires ["jobs"]
    :detect detect-failed-jobs}])
