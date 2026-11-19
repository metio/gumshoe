<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-strimzi

The [Strimzi](https://strimzi.io) tool package for [gumshoe](../../README.md)
(`kafka.strimzi.io`).

- **Detectives** - fill the `:messaging` scan scope:
  - Kafka, KafkaConnect, KafkaMirrorMaker2, KafkaBridge and KafkaConnector
    resources the operators could not reconcile (critical), and KafkaNodePool,
    KafkaTopic, KafkaUser and KafkaRebalance resources (warning). A failure is a
    `NotReady` condition that is True, or a `Ready` condition that is False, held
    for 10 minutes (30 for a new cluster that is still `Creating`);
  - `Warning` and `ReconciliationPaused` conditions (info);
  - KafkaConnectors whose connector or tasks are FAILED in Kafka Connect, with
    the first line of the trace.
- **Capability** - `:strimzi`, detected from its CRD.
- **Book** - `scan`.

```clojure
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/strimzi"}}}
;; env.edn: {:plugins [gumshoe.tools.strimzi]}
```
