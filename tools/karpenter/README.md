<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-karpenter

The [Karpenter](https://karpenter.sh) tool package for [gumshoe](../../README.md)
(`karpenter.sh/v1`).

- **Detectives** - join the `:platform` scan scope:
  - NodePools that are not Ready, naming the failing ValidationSucceeded or
    NodeClassReady condition;
  - NodePools whose NodeRegistrationHealthy is False;
  - NodePools that use as much of a resource as `spec.limits` allows;
  - NodeClaims that are not Ready 15 minutes after creation, naming the step
    (Launched, Registered, Initialized) they are stuck on;
  - NodeClaims Drifted for an hour without being replaced.
- **Capability** - `:karpenter`, detected from its CRD.
- **Book** - `scan`.

Node removals that do not finish are reported by the core `node-removals`
detective.

```clojure
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/karpenter"}}}
;; env.edn: {:plugins [gumshoe.tools.karpenter]}
```
