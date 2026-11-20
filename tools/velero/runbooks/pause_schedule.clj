;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.velero.pause-schedule
  "Pauses a Velero Schedule."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.velero.action-books :as books]))

(mutation/book (merge (books/schedule-pause true) {:description "Pauses a Velero Schedule"}))
