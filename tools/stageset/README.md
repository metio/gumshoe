<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-stageset

The [StageSet](https://stageset.projects.metio.wtf/) tool package for
[gumshoe](../../README.md). StageSet is a Flux controller for ordered, gated,
multi-stage delivery.

- **Detectives** - fill the `:delivery` scan scope (StageSets not Ready, classified
  by reason: hard failures critical, budget/promotion gates warning, soak/approval
  waits info; plus revisions held by an update window), so they also run in the
  cluster-wide scan.
- **Capability** - `:stageset`, detected from the StageSet CRD.
- **Drill-down** - `StageSet` and `StageInventory` as subjects, with a per-stage
  `stagesetctl get` status probe.
- **Runbooks** - `reconcile` (trigger reconciliation, `--update-now` to override
  the update window) and `promote` (advance a stage held on a manual gate).

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/stageset"}}}
;; env.edn
{:plugins [gumshoe.tools.stageset]}
```
