;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.cnpg
  "Investigates CloudNativePG: unhealthy clusters, missing replicas, failing WAL
   archiving, and poolers left behind by an instance rebuild."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.detectives.pooler :as pooler]
            [gumshoe.tools.cnpg]))

(detective/book
 {:description "Investigates CloudNativePG: cluster phase, ready instances, WAL archiving, poolers"
  :detectives (concat cnpg/detectives pooler/detectives)
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [cnpg/cluster-type]}})
