<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-prometheus

The prometheus-operator tool package for [gumshoe](../../README.md).

- **Detectives** - fill the `:observability` scan scope (Prometheus not Available,
  paused instances), so they also run in the cluster-wide scan.
- **Capability** - `:prometheus-operator`, detected from its CRDs.
- **Library** - `gumshoe.prometheus` (PromQL query helpers) and
  `gumshoe.rightsizing` (usage against requests).
- **Books** - `scan` plus operational runbooks: `set_capacity`, `delete_metric`,
  the `list_*_metrics` reports, and `right_size`, which compares each workload
  container's requests with its p95 CPU and peak memory working set in
  Prometheus (cAdvisor metrics) and prints the `kubectl set resources` commands
  that would apply its recommendations - it changes nothing itself.

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/prometheus"}}}
;; env.edn
{:plugins [gumshoe.tools.prometheus]}
```
