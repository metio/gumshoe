;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.workloads
  "Investigates workload health: controllers, jobs, pods, storage, quotas, overcommit."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.disruption :as disruption]
            [gumshoe.detectives.registry :as registry]))

(detective/book
 {:description "Investigates workload health: controllers, jobs, pods, Services, and their storage"
  :when-to-run "Reach for this when apps are crashing, stuck, or missing replicas - it finds crash loops, pending pods, failed jobs, stuck rollouts, CronJobs that stopped, controllers that cannot create pods, pods that never start, objects stuck terminating, references to missing objects, Services without working backends, and unused storage."
  :scope :workloads
  :options disruption/hpa-grace-option
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["deployments" "statefulsets" "daemonsets" "jobs" "pods" "services" "replicasets" "events" "persistentvolumeclaims" "cronjobs" "ingresses" "configmaps" "serviceaccounts" "horizontalpodautoscalers"]}})
