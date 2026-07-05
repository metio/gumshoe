;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.databases
  "Investigates the database scope: CloudNativePG clusters and backups plus db-operator databases."
  (:require [infra.detective :as detective]
            [infra.detectives.registry :as registry]))

(detective/book
 {:description "Investigates the database scope: CloudNativePG and db-operator"
  :when-to-run "Reach for this when a database is unreachable or degraded - CloudNativePG cluster health and db-operator databases."
  :scope :databases
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []}})
