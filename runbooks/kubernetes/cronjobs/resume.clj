;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.cronjobs.resume
  "Resumes a suspended CronJob on its schedule."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.steps.workloads :as steps]))

(mutation/book (merge steps/resume-cronjob {:description "Resumes a suspended CronJob on its schedule"}))
