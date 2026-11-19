;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.velero
  "Investigates Velero: storage locations, backups, schedules, restores and backup repositories."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.velero :as velero]
            [gumshoe.tools.velero]))

(detective/book
 {:description "Investigates Velero: unreachable storage, failed or overdue backups, failed restores"
  :when-to-run "Reach for this when you need to trust a backup - Velero storage locations, the newest backup of every schedule, overdue schedules and recent restores."
  :detectives velero/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [velero/backup-type velero/schedule-type velero/location-type]}})
