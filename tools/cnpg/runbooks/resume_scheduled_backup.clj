;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.resume-scheduled-backup
  "Resumes a suspended CloudNativePG ScheduledBackup."
  (:require [gumshoe.cnpg.action-books :as books]
            [gumshoe.mutation :as mutation]))

(mutation/book (merge (books/scheduled-backup-suspension false) {:description "Resumes a suspended CloudNativePG ScheduledBackup"}))
