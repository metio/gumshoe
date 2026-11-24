;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.kueue
  "Detectives for Kueue (kueue.x-k8s.io): queues that admit nothing because
   they are not Active, Workloads that can never be admitted or have waited
   for quota for an hour, and admitted Workloads whose pods never become
   ready. Kueue's Active condition is outside the generic condition
   fallback, so without this package an inactive queue goes unseen."
  (:require [gumshoe.conditions :as conditions]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def clusterqueue-type "clusterqueues.kueue.x-k8s.io")
(def localqueue-type "localqueues.kueue.x-k8s.io")
(def workload-type "workloads.kueue.x-k8s.io")

(def ^:private waiting-minutes
  "How long a Workload may wait for quota before it is worth a look - batch
   queues are meant to hold work, so this is long."
  60)

(def ^:private pods-ready-minutes 15)

(def never-fits
  "QuotaReserved reasons that do not resolve by waiting: the Workload needs a
   change to its requests or to the queue's flavors and quotas."
  #{"NoMatchingFlavor" "ExceedsMaxQuota" "Misconfigured" "AssignmentError"
    "UnrecordableResourceUsage"})

(defn queue-finding
  [kind queue]
  (let [active (get (conditions/by-type queue) "Active")
        component (if (kubectl/namespace-of queue) (kubectl/namespace-name-of queue) (kubectl/name-of queue))]
    (when (= "False" (:status active))
      (onset/since
       (if (= "Stopped" (:reason active))
         {:severity :info
          :component component
          :summary (format "%s is stopped" kind)
          :hint "its stopPolicy holds new admissions - clear spec.stopPolicy to resume"}
         {:severity (if (= "ClusterQueue" kind) :critical :warning)
          :component component
          :summary (format "%s is not Active (%s)" kind (or (:reason active) "no reason"))
          :hint (str (some-> (:message active) not-empty (str " - "))
                     "Kueue admits no workload through it until it is Active")})
       (:lastTransitionTime active)))))

(defn detect-queues
  [evidence]
  (concat
   (keep #(queue-finding "ClusterQueue" %) (kubectl/items-of (get evidence clusterqueue-type)))
   (keep #(queue-finding "LocalQueue" %) (kubectl/items-of (get evidence localqueue-type)))))

(defn workload-findings
  [workload now]
  (let [by-type (conditions/by-type workload)
        status-of #(:status (get by-type %))
        component (kubectl/namespace-name-of workload)
        created (-> workload :metadata :creationTimestamp)
        reserved (get by-type "QuotaReserved")
        pods-ready (get by-type "PodsReady")
        queue (-> workload :spec :queueName)]
    (cond
      (or (= "True" (status-of "Finished"))
          (false? (-> workload :spec :active))
          (-> workload :metadata :deletionTimestamp))
      nil

      (and (not= "True" (status-of "QuotaReserved")) (contains? never-fits (:reason reserved)))
      [(onset/since
        {:severity :warning
         :component component
         :summary (format "Workload cannot be admitted (%s)" (:reason reserved))
         :hint (str (some-> (:message reserved) not-empty (str " - "))
                    "waiting does not help - change its requests, or the flavors and quotas of "
                    (if queue (str "LocalQueue " queue "'s ClusterQueue") "its ClusterQueue"))}
        (:lastTransitionTime reserved))]

      (and (not= "True" (status-of "QuotaReserved"))
           (some-> (conditions/minutes-since created now) (>= waiting-minutes)))
      [(onset/since
        {:severity :info
         :component component
         :summary (format "Workload has waited %d minutes for quota" (conditions/minutes-since created now))
         :hint (str (some-> (:message reserved) not-empty (str " - "))
                    "the queue is full - it is admitted when running workloads finish, or when the ClusterQueue gets more quota or can borrow")}
        created)]

      (and (= "True" (status-of "Admitted")) (= "False" (:status pods-ready))
           (some-> (conditions/minutes-since (:lastTransitionTime pods-ready) now) (>= pods-ready-minutes)))
      [(onset/since
        {:severity :warning
         :component component
         :summary "Workload is admitted but its pods are not ready"
         :hint (str (some-> (:message pods-ready) not-empty (str " - "))
                    "its pods hold quota without running - the pod findings say why")}
        (:lastTransitionTime pods-ready))]

      :else nil)))

(defn detect-workloads
  [evidence]
  (mapcat #(workload-findings % (:now evidence)) (kubectl/items-of (get evidence workload-type))))

(def detectives
  [{:name "kueue-queues"
    :description "Kueue ClusterQueues and LocalQueues that are not Active"
    :requires [clusterqueue-type localqueue-type]
    :detect detect-queues}
   {:name "kueue-workloads"
    :description "Kueue Workloads that cannot be admitted, wait an hour for quota, or whose pods do not become ready"
    :requires [workload-type]
    :detect detect-workloads}])
