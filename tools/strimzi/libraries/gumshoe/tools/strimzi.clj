;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.strimzi
  "The Strimzi tool package: its detectives fill the :messaging scan scope (and
   so the cluster-wide scan), and it teaches the setup wizard the :strimzi
   capability - both through one plugin/provide!."
  (:require [gumshoe.detectives.strimzi :as strimzi]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:messaging strimzi/detectives}
  :capabilities {:strimzi #(kubectl/serves-crd? strimzi/kafka-type)}})
