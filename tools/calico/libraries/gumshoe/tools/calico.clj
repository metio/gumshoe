;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.calico
  "The Calico tool package (as managed by the tigera-operator): its detectives
   join the :platform scan scope (and so the cluster-wide scan), and it teaches
   the setup wizard the :calico capability - both through one plugin/provide!."
  (:require [gumshoe.detectives.calico :as calico]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:platform calico/detectives}
  :capabilities {:calico #(kubectl/resource-exists? "customresourcedefinition" "installations.operator.tigera.io")}})
