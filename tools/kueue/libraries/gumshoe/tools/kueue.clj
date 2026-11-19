;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.kueue
  "The Kueue tool package: its detectives join the :workloads scan scope
   (beside the Job detectives), and it teaches the setup wizard the :kueue
   capability - both through one plugin/provide!."
  (:require [gumshoe.detectives.kueue :as kueue]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:workloads kueue/detectives}
  :capabilities {:kueue #(kubectl/serves-crd? kueue/clusterqueue-type)}})
