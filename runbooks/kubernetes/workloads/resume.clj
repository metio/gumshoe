;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.workloads.resume
  "Resumes a paused Deployment and waits for the rollout it releases."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.steps.workloads :as steps]))

(mutation/book (merge steps/resume-deployment {:description "Resumes a paused Deployment and waits for the rollout it releases"}))
