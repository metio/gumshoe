<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-flux

The Flux tool package for [gumshoe](../../README.md): everything the engine knows
about Flux, in one place, registered through a single `plugin/provide!` in
`gumshoe.tools.flux`.

- **Detectives** - fills the `:gitops` scan scope (HelmReleases, Kustomizations,
  sources that fail to reconcile or are suspended).
- **Capability** - `:flux`, detected from the Flux CRDs.
- **Tool profile** - the `flux` CLI (≥ 2.0), inherited by any book that lists it.
- **Drill-down** - the Flux CRD kinds as subjects, plus a `flux reconcile status`
  probe offered when the CLI is installed.
- **Books** - `runbooks/gitops.clj` (the gitops scan) and `runbooks/reconcile.clj`.

## Use

A casebook pins it by subpath and activates its plugin:

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/flux"}}}
;; env.edn
{:plugins [gumshoe.tools.flux]}
```
