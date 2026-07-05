;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.storage
  "Investigates storage: PersistentVolumeClaims that are pending or unused."
  (:require [infra.detective :as detective]
            [infra.detectives.storage :as storage]))

(detective/book
 {:description "Investigates storage: pending and unused PersistentVolumeClaims"
  :detectives storage/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["persistentvolumeclaims" "pods"]}})
