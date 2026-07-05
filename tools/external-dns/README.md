<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-external-dns

The external-dns tool package for [gumshoe](../../README.md): do all the hostnames
declared across Ingresses, HTTPRoutes, Gateways, and DNSEndpoints actually resolve?
A standalone detective book; uses the core DNS helper.

```clojure
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/external-dns"}}}
```
