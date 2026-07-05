;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.events
  "Investigates cluster signals: Warning events from the last hour."
  (:require [infra.detective :as detective]
            [infra.detectives.registry :as registry]))

(detective/book
 {:description "Investigates cluster signals: Warning events from the last hour"
  :when-to-run "Reach for this for a quick pulse - every Warning event across the cluster in the last hour, newest trouble first."
  :scope :signals
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["events"]}})
