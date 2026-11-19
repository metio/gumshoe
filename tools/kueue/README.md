<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-kueue

The [Kueue](https://kueue.sigs.k8s.io) tool package for [gumshoe](../../README.md)
(`kueue.x-k8s.io`).

- **Detectives** - join the `:workloads` scan scope:
  - ClusterQueues (critical) and LocalQueues (warning) whose `Active` condition is
    False, and queues stopped by their `stopPolicy` (info);
  - Workloads whose QuotaReserved reason says waiting does not help
    (`NoMatchingFlavor`, `ExceedsMaxQuota`, `Misconfigured`, `AssignmentError`,
    `UnrecordableResourceUsage`);
  - Workloads that have waited an hour for quota (info);
  - admitted Workloads whose `PodsReady` has been False for 15 minutes.
- **Capability** - `:kueue`, detected from its CRD.
- **Book** - `scan`.

```clojure
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/kueue"}}}
;; env.edn: {:plugins [gumshoe.tools.kueue]}
```
