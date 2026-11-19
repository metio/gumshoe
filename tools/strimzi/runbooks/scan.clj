;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.strimzi
  "Investigates Strimzi: Kafka clusters, Connect, connectors, topics and users."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.strimzi :as strimzi]
            [gumshoe.tools.strimzi]))

(detective/book
 {:description "Investigates Strimzi: resources the operators could not reconcile, failed connectors"
  :when-to-run "Reach for this when Kafka on Strimzi misbehaves - clusters, Connect, MirrorMaker 2, topics or users that are not ready, and connectors or tasks that failed."
  :detectives strimzi/detectives
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [strimzi/kafka-type]}})
