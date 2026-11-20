;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.velero.delete-backup
  "Deletes a Velero backup and its data through a DeleteBackupRequest."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.velero.action-books :as books]))

(mutation/book (merge books/delete-backup {:description "Deletes a Velero backup and its data through a DeleteBackupRequest"}))
