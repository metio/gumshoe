;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.rehydrate
  "Rehydrates a hibernated CloudNativePG cluster."
  (:require [gumshoe.cnpg.action-books :as books]
            [gumshoe.mutation :as mutation]))

(mutation/book (merge (books/hibernation false) {:description "Rehydrates a hibernated CloudNativePG cluster"}))
