;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.reload
  "Reloads a CloudNativePG cluster, rolling out changed configuration."
  (:require [gumshoe.cnpg.action-books :as books]
            [gumshoe.mutation :as mutation]))

(mutation/book (merge books/reload {:description "Reloads a CloudNativePG cluster, rolling out changed configuration"}))
