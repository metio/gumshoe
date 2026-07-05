;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.cnpg
  "Investigates CloudNativePG: unhealthy clusters, missing replicas, failing WAL archiving."
  (:require [infra.detective :as detective]
            [infra.detectives.cnpg :as cnpg]))

(detective/book
 {:description "Investigates CloudNativePG: cluster phase, ready instances, WAL archiving"
  :detectives cnpg/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [cnpg/cluster-type]}})
