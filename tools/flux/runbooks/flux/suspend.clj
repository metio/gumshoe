;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.flux.suspend
  "Suspends a Flux HelmRelease, Kustomization, GitRepository or OCIRepository."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.steps.flux-suspension :as flux-suspension]))

(mutation/book (merge flux-suspension/suspend {:description "Suspends a Flux HelmRelease, Kustomization, GitRepository or OCIRepository"}))
