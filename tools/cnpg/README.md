<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-cnpg

The CloudNativePG tool package for [gumshoe](../../README.md).

- **Detectives** - join the `:databases` scan scope (clusters not in a healthy
  phase, too few ready instances, failing WAL archiving, failed/suspended
  backups), so they also run in the cluster-wide scan.
- **Pooler detectives** - a pooler whose pods predate its cluster's instances
  fails every pooled login until it is restarted, and the operator does not
  restart it; also flagged are a rollout serving two generations at once and
  pooler pods without resource requests or limits.
- **Capability** - `:cnpg`, detected from the CloudNativePG CRDs.
- **Book** - `runbooks/scan.clj`.

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/cnpg"}}}
;; env.edn
{:plugins [gumshoe.tools.cnpg]}
```
