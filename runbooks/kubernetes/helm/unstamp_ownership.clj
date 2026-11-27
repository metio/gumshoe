;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.helm.unstamp-ownership
  "Strips Helm's ownership of a release from objects it adopted or was about
   to adopt, so the release no longer claims them."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.steps.helm-ownership :as steps]))

(mutation/book (merge steps/unstamp-named {:description "Strips Helm ownership of a release from objects, so the release no longer claims them"}))
