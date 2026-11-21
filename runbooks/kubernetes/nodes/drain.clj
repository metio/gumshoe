;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.nodes.drain
  "Drains a single node: cordons it and evicts every pod running on it. The step is shared with the node maintenance playbook."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.steps.nodes :as nodes]))

(mutation/book (merge nodes/drain {:description "Drains a single node: cordons it and evicts every pod running on it"}))
