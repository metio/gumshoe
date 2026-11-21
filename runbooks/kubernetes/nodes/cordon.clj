;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.kubernetes.nodes.cordon
  "Marks a single node as unschedulable. The step is shared with the node maintenance playbook."
  (:require [gumshoe.mutation :as mutation]
            [gumshoe.steps.nodes :as nodes]))

(mutation/book (merge nodes/cordon {:description "Marks a single node as unschedulable"}))
