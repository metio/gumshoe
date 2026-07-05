<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-thanos

The thanos tool package for [gumshoe](../../README.md): Thanos query layer: readiness, stores, rules (port-forward). A standalone
detective book - it registers no scope or capability, so depending on it simply
puts the book on the classpath (no env.edn :plugins entry needed).

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/thanos"}}}
```
