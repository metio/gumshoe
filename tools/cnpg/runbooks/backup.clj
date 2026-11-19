;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.backup
  "Takes an on-demand backup of a CloudNativePG cluster."
  (:require [gumshoe.cnpg.action-books :as books]
            [gumshoe.mutation :as mutation]))

(mutation/book (merge books/backup {:description "Takes an on-demand backup of a CloudNativePG cluster"}))
