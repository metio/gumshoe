<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-flux

The Flux tool package for [gumshoe](../../README.md): everything the engine knows
about Flux, in one place, registered through a single `plugin/provide!` in
`gumshoe.tools.flux`.

- **Detectives** - fills the `:gitops` scan scope: HelmReleases, Kustomizations
  and sources that fail to reconcile or are suspended, plus three findings a
  condition check alone does not reach.
  - **parked releases.** Flux defaults install and upgrade to `retries: 0`, so a
    spent release is left failed until its spec or chart changes. Its
    `status.conditions` then keeps describing a cause that may have cleared long
    ago, and nothing retries. Reported as parked, with the attempted revision and
    the `resetAt` annotation that clears the counters. A release using
    `RetryOnFailure` is never parked.
  - **success over a broken workload.** `Ready` on a HelmRelease is a statement
    about Helm, not about the software: a green release and a crash-looping pod
    coexist happily. Reported with the container, its reason and its restart
    count.
  - **inert deliveries.** An object annotated `…/reconcile: disabled` carries
    every mark of a managed object and is not managed — a migration gate left on
    reads as healthy from every angle except the age of what it serves.
- **Capability** - `:flux`, detected from the Flux CRDs.
- **Tool profile** - the `flux` CLI (≥ 2.0), inherited by any book that lists it.
- **Drill-down** - the Flux CRD kinds as subjects, plus a `flux reconcile status`
  probe offered when the CLI is installed.
- **Books** - `runbooks/gitops.clj` (the gitops scan) and `runbooks/flux/reconcile.clj`.

## Use

A casebook pins it by subpath and activates its plugin:

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/flux"}}}
;; env.edn
{:plugins [gumshoe.tools.flux]}
```
