;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.scrape-targets
  "Investigates why Prometheus scrape targets are down: reads every shard's
   /api/v1/targets through the pod proxy and classifies each down target by
   its lastError. A shard that could not be read is reported, never left
   out."
  (:require [gumshoe.detective :as detective]
            [gumshoe.detectives.scrape-targets :as scrape-targets]
            [gumshoe.prometheus.targets :as targets]
            [gumshoe.tools.prometheus]))

(detective/book
 {:description "Investigates down Prometheus scrape targets: the cause each lastError names, every shard read"
  :detectives scrape-targets/detectives
  :when-to-run "Reach for this when a Prometheus target is down: it tells a dropped connection (network), a refused one (workload) and a failure above the transport (TLS, auth, path) apart."
  :prerequisites {:installed-tools ["kubectl"]
                  :cluster-capabilities []
                  :kubectl-can-get [targets/prometheus-type "pods" "pods/proxy"]}})
