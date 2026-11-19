;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.kueue
  "Investigates Kueue: ClusterQueues, LocalQueues and Workloads."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.kueue :as kueue]
            [gumshoe.tools.kueue]))

(detective/book
 {:description "Investigates Kueue: inactive queues, Workloads that are never admitted"
  :when-to-run "Reach for this when a batch Job stays suspended under Kueue - inactive queues, Workloads that can never fit, and admitted Workloads whose pods do not start."
  :detectives kueue/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [kueue/clusterqueue-type kueue/workload-type]}})
