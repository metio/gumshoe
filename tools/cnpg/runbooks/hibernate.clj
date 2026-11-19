;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.cnpg.hibernate
  "Hibernates a CloudNativePG cluster, keeping its volumes."
  (:require [gumshoe.cnpg.action-books :as books]
            [gumshoe.mutation :as mutation]))

(mutation/book (merge (books/hibernation true) {:description "Hibernates a CloudNativePG cluster, keeping its volumes"}))
