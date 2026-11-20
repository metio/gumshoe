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
- **Books** - `scan`, and four actions:
  - `pause_schedule` and `resume_schedule` set a Schedule's `spec.paused`; resuming
    warns that a run missed while paused starts a backup at once, because Velero
    counts the next run from the last backup.
  - `backup` starts a Backup now from a Schedule's template, built as
    `velero backup create --from-schedule` builds it (the template's spec and
    metadata, the `velero.io/schedule-name` label, `<schedule>-<UTC timestamp>`),
    and waits for it to complete, stopping at the first failure. It refuses a
    schedule that failed validation or whose storage location is not Available.
  - `delete_backup` deletes a Backup through a DeleteBackupRequest, as
    `velero backup delete` does: deleting the Backup object alone leaves its data
    in object storage, and the backup sync puts the object back. It refuses a
    running backup and a location that is read-only or not Available, names the
    Restores deleted with it, and waits until the Backup is gone, stopping when
    the request records errors.

A schedule counts as overdue once its last backup is older than one and a half
times the longest gap its cron expression allows. Expressions restricted by day
of month or month are not judged.

```clojure
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/velero"}}}
;; env.edn: {:plugins [gumshoe.tools.velero]}
```
