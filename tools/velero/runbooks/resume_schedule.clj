;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.velero.resume-schedule
  "Resumes a paused Velero Schedule."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.velero.action-books :as books]))

(mutation/book (merge (books/schedule-pause false) {:description "Resumes a paused Velero Schedule"}))
