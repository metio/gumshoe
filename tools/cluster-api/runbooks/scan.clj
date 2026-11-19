;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.cluster-api
  "Investigates Cluster API: Clusters, Machines and MachineHealthChecks."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.cluster-api :as cluster-api]
            [gumshoe.tools.cluster-api]))

(detective/book
 {:description "Investigates Cluster API: unavailable Clusters, stuck Machines, blocked remediation"
  :when-to-run "Reach for this on a Cluster API management cluster - workload Clusters that are not Available, Machines stuck provisioning or deleting, and health checks that may not remediate."
  :detectives cluster-api/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [cluster-api/cluster-type cluster-api/machine-type]}})
