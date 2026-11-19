<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-velero

The [Velero](https://velero.io) tool package for [gumshoe](../../README.md)
(`velero.io`).

- **Detectives** - fill the `:backups` scan scope: BackupStorageLocations that are
  Unavailable, the newest backup of every schedule (and manual backups from the
  last day) when it failed or partially failed, backups stuck in a running phase
  for 12 hours, schedules that failed validation, are paused or are overdue, failed
  restores from the last day, and BackupRepositories that are NotReady.
- **Capability** - `:velero`, detected from its CRD.
- **Book** - `scan`.

A schedule counts as overdue once its last backup is older than one and a half
times the longest gap its cron expression allows. Expressions restricted by day
of month or month are not judged.

```clojure
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/velero"}}}
;; env.edn: {:plugins [gumshoe.tools.velero]}
```
