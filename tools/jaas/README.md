<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-jaas

The [JaaS](https://jaas.projects.metio.wtf/) tool package for
[gumshoe](../../README.md). JaaS evaluates Jsonnet in the cluster: a
`JsonnetSnippet` renders against the `JsonnetLibrary` bundles it enumerates and
publishes the result as a Flux `ExternalArtifact`.

- **Detectives** - fill the `:gitops` scan scope (so they also run in the
  cluster-wide scan): snippets that are not Ready, classified by reason
  (evaluation and identity failures critical, authoring and reference errors
  warning, `Pending`/`Suspended` info), plus snippets importing a
  `JsonnetLibrary` that is not in the cluster.
- **Capability** - `:jaas`, detected from the `JsonnetSnippet` CRD.
- **Drill-down** - `JsonnetSnippet` and `JsonnetLibrary` as subjects, with a
  facts panel (entry file, source, revision, published artifact) and edges to
  the libraries a snippet imports, the Flux source it renders, and the
  `ExternalArtifact` it publishes.
- **Runbooks** - `jaas/reconcile` (stamp `reconcile.fluxcd.io/requestedAt` and
  wait for the operator to acknowledge it), `jaas/suspend`, `jaas/resume`, and
  `scan` (JaaS on its own, without every Flux source alongside).

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/jaas"}}}
;; env.edn
{:plugins [gumshoe.tools.jaas]}
```

## Following a lead across the delivery chain

The edges are why this package and [gumshoe-stageset](../stageset/README.md) are
worth having together. From a StageSet whose rollout stalled:

```text
StageSet apps/web
  └─ stage 'second' builds from → ExternalArtifact apps/dashboards
       └─ produced by          → JsonnetSnippet apps/dashboards
            ├─ imports         → JsonnetLibrary apps/grafonnet
            └─ renders         → GitRepository apps/config
```

Each hop is one `investigate` step, so "stage two never went healthy" walks to
the snippet that failed to evaluate and the library ref that broke it. The
middle hop is the RFC-0012 `spec.sourceRef` back-pointer, which the
[flux package](../flux/README.md) owns because it is generic to any producer.
