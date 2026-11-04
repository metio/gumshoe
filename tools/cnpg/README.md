<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-cnpg

The CloudNativePG tool package for [gumshoe](../../README.md).

- **Detectives** - join the `:databases` scan scope (clusters not in a healthy
  phase, too few ready instances, failing WAL archiving, failed/suspended
  backups), so they also run in the cluster-wide scan.
- **Volume detective** - PVC usage is absent from the resource API, where
  `status.capacity` reports the size requested rather than the size in use, so
  this one brings its own evidence collector reading kubelet's
  `/api/v1/nodes/<node>/proxy/stats/summary` for the nodes that host instance
  pods. An instance pod the kubelet reported nothing for is called unknown, not
  healthy: the pod most likely to be missing from that answer is one whose
  containers are not running.
- **Crashloop detective** - an instance pod stuck in `CrashLoopBackOff` is
  reported with its restart count, which is what separates a pod that will
  recover from one that has been failing for days. A Pooler's own pods carry the
  same `cnpg.io/cluster` label, so the sweep matches on the instance pod role
  and excludes anything labelled with a `poolerName`.
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
- **WAL retention detective** - a cluster with no `max_slot_wal_keep_size`, or
  with it set to `-1`, puts no floor under replication slot retention: one
  standby that stops reading retains WAL until the volume is full, which stops
  postgres rather than slowing it. A cap worth half the volume or more is
  reported too, since it invalidates no slot before the disk fills. The check
  reads the Cluster spec alone, so it fires on every cluster at once while the
  repair is still a parameter change. Postgres sizes are its own binary units
  (`kB`, `MB`, `GB`), so a Kubernetes quantity like `10Gi` is reported as
  unreadable rather than taken as safe.
- **Capability** - `:cnpg`, detected from the CloudNativePG CRDs.
- **Books** - `runbooks/scan.clj` for the scan, and `runbooks/pooler_console.clj`
  which reads one Pooler's PgBouncer admin console (`SHOW DATABASES`, `POOLS`,
  `SERVERS`) plus its recent log, and reports which of the two look-alike
  faults it shows: a rejected credential, or an unreachable backend. A log tail
  spans whatever it spans, so the verdict says whether its failures are current
  or left behind by something already over.
  `runbooks/cnpg_console.clj` reads every instance of one cluster - WAL
  positions, recovery state, replication slots - for the question no detective
  answers: when a cluster has no working primary, which instance should become
  one. It proposes the instance furthest ahead and states what choosing any
  other would discard, then stops. Promoting needs `--promote <instance>`, and
  that path states the cost, asks for the literal word `yes`, and verifies the
  new primary afterwards; `--dry-run` shows the command and writes nothing. An
  instance it could not read is named as a gap in the proposal, since an unread
  instance may hold the furthest position of all. A cluster whose operator
  reports a healthy primary gets no candidate: a primary is not in recovery and
  so ranks below every standby that has a receive position, which would read as
  a promotion worth making.

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/cnpg"}}}
;; env.edn
{:plugins [gumshoe.tools.cnpg]}
```
