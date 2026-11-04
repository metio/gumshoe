;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.cnpg
  "Investigates the databases scan scope: CloudNativePG clusters, their
   designated primaries, crashlooping instances, volume pressure, slot
   retention, pending enlargements and poolers.

   The scope is resolved when the book runs, so a detective this package adds
   joins without editing the book - and so does another database package's, when
   one is loaded."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.cnpg :as cnpg]
            [gumshoe.tools.cnpg]))

(detective/book
 {:description "Investigates the databases scope: CloudNativePG clusters, primaries, instance crashloops, volumes, slot retention, poolers"
  :scope :databases
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [cnpg/cluster-type "pods" "persistentvolumeclaims"]}})
