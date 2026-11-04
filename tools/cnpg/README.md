<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-cnpg

The CloudNativePG tool package for [gumshoe](../../README.md).

- **Detectives** - join the `:databases` scan scope (clusters not in a healthy
  phase, too few ready instances, failing WAL archiving, failed/suspended
  backups), so they also run in the cluster-wide scan.
- **Pooler detectives** - a pooler whose pods predate every instance of its
  cluster, which is what a rebuild leaves behind since the operator does not
  restart poolers; also flagged are a rollout serving two generations at once
  and pooler pods without resource requests or limits. The stale finding stays
  quiet while the cluster itself is unhealthy, and its hint distinguishes
  `password authentication failed` (the pooler) from `connect failed` (the
  backend being unreachable, which a restart will not fix).
- **Capability** - `:cnpg`, detected from the CloudNativePG CRDs.
- **Book** - `runbooks/scan.clj`.

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/cnpg"}}}
;; env.edn
{:plugins [gumshoe.tools.cnpg]}
```
