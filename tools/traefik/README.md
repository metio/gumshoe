<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-traefik

The [Traefik](https://traefik.io) tool package for [gumshoe](../../README.md)
(`traefik.io`).

- **Detectives** - join the `:traffic` scan scope: IngressRoutes, Middlewares
  and TraefikServices that refer to a Service, TraefikService or Middleware
  that does not exist, or to a Service port the Service does not have
  (critical). That covers route services and middlewares, the middlewares of a
  service reference, chain middlewares, the errors middleware's service, and
  weighted, highest-random-weight and mirroring services. References are
  resolved as Traefik resolves them: an empty kind is a Service, an empty
  namespace is the referring object's, and a name with `@` belongs to another
  provider and is skipped.
- **Capability** - `:traefik`, detected from its CRD.
- **Book** - `scan`.

```clojure
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/traefik"}}}
;; env.edn: {:plugins [gumshoe.tools.traefik]}
```
