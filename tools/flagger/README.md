<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-flagger

The [Flagger](https://flagger.app) tool package for [gumshoe](../../README.md)
(`flagger.app`).

- **Detectives** - join the `:delivery` scan scope:
  - Canaries in phase Failed, whose analysis rolled the target back (critical);
  - Canaries whose running analysis has failed checks (warning);
  - Canaries Initializing for 15 minutes (warning);
  - Canaries waiting an hour at a confirm-rollout or confirm-promotion gate (info).

  Suspended Canaries are not judged.
- **Capability** - `:flagger`, detected from its CRD.
- **Book** - `scan`.

```clojure
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/flagger"}}}
;; env.edn: {:plugins [gumshoe.tools.flagger]}
```
