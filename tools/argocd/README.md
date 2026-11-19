<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-argocd

The [Argo CD](https://argo-cd.readthedocs.io) tool package for
[gumshoe](../../README.md) (`argoproj.io`).

- **Detectives** - join the `:gitops` scan scope:
  - Applications whose conditions report an error (critical) or a shared or
    repeated resource (warning);
  - Applications whose health is Degraded or Missing;
  - a last sync that failed or errored, or one running for 30 minutes;
  - an automated Application that stays OutOfSync 10 minutes after its last sync;
  - ApplicationSets whose generators fail (`ErrorOccurred`).
- **Capability** - `:argocd`, detected from its CRD.
- **Book** - `scan`.

```clojure
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/argocd"}}}
;; env.edn: {:plugins [gumshoe.tools.argocd]}
```
