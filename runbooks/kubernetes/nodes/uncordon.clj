;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.nodes.uncordon
  "Marks a single cordoned node as schedulable again. The step is shared with the node maintenance playbook."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.steps.nodes :as nodes]))

(mutation/book (merge nodes/uncordon {:description "Marks a single cordoned node as schedulable again"}))
