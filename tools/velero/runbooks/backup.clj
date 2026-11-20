;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.velero.backup
  "Starts a Velero backup now from a Schedule's template."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.velero.action-books :as books]))

(mutation/book (merge books/backup {:description "Starts a Velero backup now from a Schedule's template"}))
