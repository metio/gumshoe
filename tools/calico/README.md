<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-calico

The [Calico](https://docs.tigera.io/) tool package for [gumshoe](../../README.md),
as managed by the tigera-operator.

- **Detectives** - join the `:platform` scan scope (tigera components that are not
  Available, Degraded, or Progressing), so they also run in the cluster-wide scan.
- **Capability** - `:calico`, detected from the tigera-operator Installation CRD.
- **Books** - `runbooks/scan.clj` for the operator's components, and
  `runbooks/flows.clj` for flow logs.

## Flow logs

`runbooks/flows.clj` reads Calico flow logs from Whisker's backend over HTTPS -
no cluster access, no port-forward, no mTLS to goldmane. Whisker's nginx proxies
`/whisker-backend/` to the backend, so the book needs only the Whisker URL and
the credentials guarding it:

```shell
bb flows --whisker https://whisker.example.org --secret infra/whisker --window 1800
```

The URL may live in `env.edn` under `[:whisker :url]` instead. Credentials come
from the password store: the password of the entry, and the htpasswd user from a
`login`, `user` or `username` field beside it, because that user is usually a
generated string. `--user` covers an entry that carries none.

It answers which policy decided a path rather than whether the path works. The
detectives:

| Detective | Finds |
|---|---|
| `calico-denied-flows` | denials grouped by source namespace, destination namespace and port, with the deciding policy |
| `calico-staged-disagreement` | flows a staged policy would decide differently - run this before enforcing a staged default-deny |
| `calico-silent-denies` | denials with packets out and none back, which reach the client as a timeout rather than a refusal |
| `calico-end-of-tier-denies` | denials where no rule matched at all, the signature of a rule written against the wrong port or selector |

A pod-to-Service rule naming the Service's port is the common case for the last
one: Calico matches egress after kube-proxy's DNAT, so the packet carries the
backend's target port and the rule never fires.

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/calico"}}}
;; env.edn
{:plugins [gumshoe.tools.calico]}
```
