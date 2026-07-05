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
- **Book** - `runbooks/scan.clj`.

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/gateway"}}}
;; env.edn
{:plugins [gumshoe.tools.gateway]}
```
