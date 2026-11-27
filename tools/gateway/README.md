<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-gateway

The Gateway API tool package for [gumshoe](../../README.md).

- **Detectives** - fill the `:traffic` scan scope (Gateways not Programmed,
  listeners with no routes or bad refs, HTTPRoutes accepted by nobody, dangling
  ListenerSets), so they also run in the cluster-wide scan.
- **Capability** - `:gateway-api`, detected from the Gateway API CRDs.
- **Books** - `runbooks/scan.clj`, and `runbooks/rename_listeners.clj`, which
  renames a ListenerSet's listeners to the section names its routes attach to
  (routes reporting `NoMatchingParent`). It patches the names alone, with a
  JSON patch that tests each old name at its index, and refuses where several
  numbered listeners (`https-0`, `https-1`) could be the one the routes mean.

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/gateway"}}}
;; env.edn
{:plugins [gumshoe.tools.gateway]}
```
