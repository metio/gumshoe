<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-cluster-api

The [Cluster API](https://cluster-api.sigs.k8s.io) tool package for
[gumshoe](../../README.md) (`cluster.x-k8s.io`, v1beta2 conditions), run against
a management cluster.

- **Detectives** - join the `:platform` scan scope:
  - Clusters that are not Available for 10 minutes, naming the first False of
    RemoteConnectionProbe, InfrastructureReady, ControlPlaneAvailable and
    WorkersAvailable; Failed Clusters; paused Clusters (info);
  - Machines that are Failed, still provisioning after 30 minutes (naming
    BootstrapConfigReady, InfrastructureReady or NodeHealthy), deleting for 30
    minutes (drain or volume detach), or failing their MachineHealthCheck;
  - MachineHealthChecks whose RemediationAllowed is False.
- **Capability** - `:cluster-api`, detected from its CRD.
- **Book** - `scan`.

```clojure
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/cluster-api"}}}
;; env.edn: {:plugins [gumshoe.tools.cluster-api]}
```
