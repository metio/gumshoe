;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.prometheus
  "The prometheus-operator tool package: its detectives fill the :observability
   scan scope (and so the cluster-wide scan), and it teaches the setup wizard the
   :prometheus-operator capability - both through one plugin/provide!. The
   scrape-target detectives read each Prometheus shard through the pod proxy,
   which needs get on pods/proxy, so they go into the opt-in :proxy scope and
   run only in this package's scrape_targets book. The
   prometheus query library (gumshoe.prometheus) and the metric/capacity runbooks
   live alongside this namespace."
  (:require [gumshoe.detectives.monitoring :as monitoring]
            [gumshoe.detectives.scrape-targets :as scrape-targets]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]
            [gumshoe.prometheus.targets :as targets]))

(plugin/provide!
 {:detectives {:observability monitoring/detectives
               :proxy scrape-targets/detectives}
  ;; /api/v1/targets is not in the resource API, so the scrape-target
  ;; detectives bring their own collector, one read per shard
  :collectors {targets/evidence-key targets/collector}
  :capabilities {:prometheus-operator #(kubectl/serves-crd? "prometheuses.monitoring.coreos.com")}})
