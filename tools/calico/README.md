<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-calico

The [Calico](https://docs.tigera.io/) tool package for [gumshoe](../../README.md),
as managed by the tigera-operator.

- **Detectives** - join the `:platform` scan scope (tigera components that are not
  Available, Degraded, or Progressing), so they also run in the cluster-wide scan.
- **Capability** - `:calico`, detected from the tigera-operator Installation CRD.
- **Book** - `runbooks/scan.clj`.

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/calico"}}}
;; env.edn
{:plugins [gumshoe.tools.calico]}
```
