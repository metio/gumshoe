<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-cnpg

The CloudNativePG tool package for [gumshoe](../../README.md).

- **Detectives** - join the `:databases` scan scope (clusters not in a healthy
  phase, too few ready instances, failing WAL archiving, failed/suspended
  backups), so they also run in the cluster-wide scan.
- **Primary detective** - a cluster whose `currentPrimary` is not among its
  healthy instances accepts no writes however plausible its phase and ready
  count look, and one that has been not-Ready for over an hour is stuck rather
  than rolling. The hint says whether a switchover is under way, because
  `currentPrimary == targetPrimary` on a not-Ready cluster means nothing is
  being promoted.
- **Pooler detectives** - a pooler whose pods predate every instance of its
  cluster, which is what a rebuild leaves behind since the operator does not
  restart poolers; also flagged are a rollout serving two generations at once
  and pooler pods without resource requests or limits. The stale finding stays
  quiet while the cluster itself is unhealthy, and its hint distinguishes
  `password authentication failed` (the pooler) from `connect failed` (the
  backend being unreachable, which a restart will not fix).
- **Capability** - `:cnpg`, detected from the CloudNativePG CRDs.
- **Books** - `runbooks/scan.clj` for the scan, and `runbooks/pooler_console.clj`
  which reads one Pooler's PgBouncer admin console (`SHOW DATABASES`, `POOLS`,
  `SERVERS`) plus its recent log, and reports which of the two look-alike
  faults it shows: a rejected credential, or an unreachable backend.

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/cnpg"}}}
;; env.edn
{:plugins [gumshoe.tools.cnpg]}
```
