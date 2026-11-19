;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.restart
  "Restarts every instance of a CloudNativePG cluster, one at a time."
  (:require [gumshoe.cnpg.action-books :as books]
            [gumshoe.mutation :as mutation]))

(mutation/book (merge books/restart {:description "Restarts every instance of a CloudNativePG cluster, one at a time"}))
