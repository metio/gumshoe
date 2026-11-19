;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.karpenter
  "The Karpenter tool package: its detectives join the :platform scan scope
   (beside the node detectives), and it teaches the setup wizard the :karpenter
   capability - both through one plugin/provide!."
  (:require [gumshoe.detectives.karpenter :as karpenter]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:platform karpenter/detectives}
  :capabilities {:karpenter #(kubectl/serves-crd? karpenter/nodepool-type)}})
