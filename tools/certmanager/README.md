<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-certmanager

The cert-manager tool package for [gumshoe](../../README.md).

- **Detectives** - fill the `:tls` scan scope (certificates not Ready or expiring
  soon, sour ACME orders), so they also run in the cluster-wide scan.
- **Capability** - `:cert-manager`, detected from the cert-manager CRDs, so the
  setup wizard recognises a cert-manager cluster.
- **Book** - `runbooks/scan.clj` investigates cert-manager directly.

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/certmanager"}}}
;; env.edn
{:plugins [gumshoe.tools.certmanager]}
```
