<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-db-operator

The [db-operator](https://github.com/db-operator/db-operator) tool package for
[gumshoe](../../README.md) (`databases.kinda.rocks`).

- **Detectives** - join the `:databases` scan scope (Database resources not ready).
- **Capability** - `:db-operator`, detected from its CRD.
- **Books** - `admin_connect`, `connect`, `dump`, `restore`.

```clojure
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/db-operator"}}}
;; env.edn: {:plugins [gumshoe.tools.db-operator]}
```
