;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.prometheus
  "The prometheus-operator tool package: its detectives fill the :observability
   scan scope (and so the cluster-wide scan), and it teaches the setup wizard the
   :prometheus-operator capability - both through one plugin/provide!. The
   prometheus query library (gumshoe.prometheus) and the metric/capacity runbooks
   live alongside this namespace."
  (:require [gumshoe.detectives.monitoring :as monitoring]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(plugin/provide!
 {:detectives {:observability monitoring/detectives}
  :capabilities {:prometheus-operator #(kubectl/resource-exists? "customresourcedefinition" "prometheuses.monitoring.coreos.com")}})
