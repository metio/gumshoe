;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.workloads
  "Investigates workload health: controllers, jobs, pods, storage, quotas, overcommit."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.registry :as registry]))

(detective/book
 {:description "Investigates workload health: controllers, jobs, pods, and their storage"
  :when-to-run "Reach for this when apps are crashing, stuck, or missing replicas - it finds crash loops, pending pods, failed jobs, and unused storage."
  :scope :workloads
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["deployments" "statefulsets" "daemonsets" "jobs" "pods" "persistentvolumeclaims"]}})
