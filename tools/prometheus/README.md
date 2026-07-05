<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-prometheus

The prometheus-operator tool package for [gumshoe](../../README.md).

- **Detectives** - fill the `:observability` scan scope (Prometheus not Available,
  paused instances), so they also run in the cluster-wide scan.
- **Capability** - `:prometheus-operator`, detected from its CRDs.
- **Library** - `gumshoe.prometheus` (PromQL query helpers).
- **Books** - `scan` plus operational runbooks: `set_capacity`, `delete_metric`,
  and the `list_*_metrics` reports.

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/prometheus"}}}
;; env.edn
{:plugins [gumshoe.tools.prometheus]}
```
