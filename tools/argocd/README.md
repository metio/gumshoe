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
- **Books** - `scan`, and two that act through the Application object, so no
  `argocd` CLI or API server login is needed:
  - `refresh` (`--application <ns>/<name>`, `--hard` to regenerate manifests)
    sets `argocd.argoproj.io/refresh` and waits until the application
    controller removes it;
  - `sync` (`--prune` to delete what was removed from Git) writes an
    `operation` and waits until `status.operationState` reports it Succeeded,
    ending the wait with the controller's message when it fails or errors. It
    refuses while another operation runs.

```clojure
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/argocd"}}}
;; env.edn: {:plugins [gumshoe.tools.argocd]}
```
