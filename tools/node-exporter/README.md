<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-node-exporter

The [node_exporter](https://github.com/prometheus/node_exporter) tool package
for [gumshoe](../../README.md): host facts read through PromQL from
Prometheus, Thanos Query or Mimir, using `gumshoe.promql` from core.

- **Book** - `facts`: one row per host with its CPU count, memory, kernel, OS,
  boot time and whether its exporter is up, and a comparison with a file of
  declared host names.
- **Detectives** - a query that failed (critical, so its facts are not read
  as absent), an exporter that is down (critical), a host reporting no CPU
  count or memory (warning), a declared host that reports nothing (critical),
  and a reporting host that is not declared (warning). A selector that matches
  no host reports that nothing was checked and exits 1.

```shell
bb tools/node-exporter/runbooks/facts.clj --url https://thanos.example.org --flavor thanos \
   --host-label node --selector 'cluster="staging"' --declared staging-hosts.txt --output json
```

A host is the value of `--host-label`: `node` where a ServiceMonitor relabels
the node name in, `hostname` or `nodename` for hosts scraped by name, or
`instance` (the default), whose port is stripped. Every query aggregates by
that label, so a host scraped over IPv4 and IPv6, or by two replicas, counts
once. `--job` is a regular expression naming the node_exporter scrape jobs for
the up check. Credentials come from `--username` and `--password`, else from
the secret whose path matches the URL. The JSON and EDN reports carry the rows
as `hosts` beside the findings.

```clojure
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/node-exporter"}}}
```
