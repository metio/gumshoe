<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-ceph

The ceph tool package for [gumshoe](../../README.md): cephadm-over-SSH cluster
management, self-contained.

- **Library** - `gumshoe.ceph` (SSH connection + `ceph` CLI helpers).
- **Detective** - `runbooks/scan.clj` investigates a cephadm cluster (health,
  OSDs, PGs, quorum, capacity, services, crashes) over SSH.
- **Runbooks** - `osd_out`, `osd_in`, `status`, `restart_daemon`,
  `archive_crashes`; **playbook** - `upgrade`.
- **Plugin** - `gumshoe.tools.ceph` contributes the ceph cluster-log tailer to
  storage-resize watches (a generic resize seam; ceph is one storage provider).

## Use

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/ceph"}}}
;; env.edn (only needed for the resize-watch log tailer)
{:plugins [gumshoe.tools.ceph]
 :environments {:prod {:select {:kubernetes-cluster "…"} :ceph {:mgr-hosts ["mgr-1"]}}}}
```
