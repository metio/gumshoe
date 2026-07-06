<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-loki

The loki tool package for [gumshoe](../../README.md): Loki readiness and
component ring health (port-forward). A standalone detective book - it
registers no scope or capability, so depending on it simply puts the book on
the classpath (no env.edn :plugins entry needed).

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/loki"}}}
```
