;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.nodes
  "Investigates node health: readiness, resource pressure, cordoned nodes."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.nodes :as nodes]))

(detective/book
 {:description "Investigates node health: readiness, memory/disk/PID pressure, cordoned nodes"
  :detectives nodes/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get ["nodes"]}})
