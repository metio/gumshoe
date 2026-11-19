;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.suspend-scheduled-backup
  "Suspends a CloudNativePG ScheduledBackup."
  (:require [gumshoe.cnpg.action-books :as books]
            [gumshoe.mutation :as mutation]))

(mutation/book (merge (books/scheduled-backup-suspension true) {:description "Suspends a CloudNativePG ScheduledBackup"}))
