;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.flux.resume
  "Resumes a suspended Flux HelmRelease, Kustomization, GitRepository or OCIRepository."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.steps.flux-suspension :as flux-suspension]))

(mutation/book (merge flux-suspension/resume {:description "Resumes a suspended Flux HelmRelease, Kustomization, GitRepository or OCIRepository"}))
