;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.workloads.pause
  "Pauses a Deployment's rollouts, so pod template changes wait until it is resumed."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.steps.workloads :as steps]))

(mutation/book (merge steps/pause-deployment {:description "Pauses a Deployment's rollouts, so pod template changes wait until it is resumed"}))
