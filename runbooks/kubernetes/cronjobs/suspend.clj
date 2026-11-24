;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.cronjobs.suspend
  "Suspends a CronJob so no new Job starts."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.steps.workloads :as steps]))

(mutation/book (merge steps/suspend-cronjob {:description "Suspends a CronJob so no new Job starts"}))
