<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-crossplane

The [Crossplane](https://crossplane.io) tool package for [gumshoe](../../README.md)
(`pkg.crossplane.io`).

- **Detectives** - join the `:platform` scan scope: Providers, Functions and
  Configurations
  - whose `Installed` condition is False (critical), unpacking for 10 minutes
    (critical), or inactive (warning);
  - whose first False of `Healthy`, `RevisionHealthy` and `RuntimeHealthy` says
    the package does not work (critical).

  Managed and composite resources are reported by the core condition fallback,
  which reads their `Ready` and `Synced` conditions.
- **Capability** - `:crossplane`, detected from its CRD.
- **Book** - `scan`.

```clojure
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/crossplane"}}}
;; env.edn: {:plugins [gumshoe.tools.crossplane]}
```
