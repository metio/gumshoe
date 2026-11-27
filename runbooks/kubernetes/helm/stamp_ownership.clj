;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.helm.stamp-ownership
  "Stamps Helm's ownership of a release onto objects that already exist, so
   the release's next helm install adopts them in place instead of aborting
   on invalid ownership metadata."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.steps.helm-ownership :as steps]))

(mutation/book (merge steps/stamp-named {:description "Stamps Helm ownership of a release onto existing objects, so helm install adopts them in place"}))
