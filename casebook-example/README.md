<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# casebook-example

A template for a **casebook** - a team's own collection of books (runbooks,
playbooks, firebooks, detectives) and plugins that runs on the
[gumshoe](../README.md) engine. It is a gumshoe **extension**: you clone it next
to gumshoe and register its path, and gumshoe treats it as a first-class peer of
the built-ins - no dependency resolution, no JVM, just babashka.

## Bootstrap

1. Copy this directory into a fresh git repo, e.g. `casebook-storage`.
2. Add your books under `runbooks/` (and `playbooks/`, `firebooks/`). Each book
   `(:require [gumshoe.detective ...])` or `[gumshoe.runbook ...]` - the engine is on
   the classpath because gumshoe puts it there.
3. Keep the `gumshoe.edn` manifest at the root. Declare `:book-paths` (where your
   books live), optionally `:paths` (shared code your books require), and
   `:plugins` (namespaces that register announcers/detectives/capability
   detectors/tool support).

## Wire it into gumshoe

Clone this repo alongside gumshoe and point at it from your `env.edn`:

```clojure
:extensions ["../casebook-storage"]
```

Now `gumshoe`, `./detect`, and `./run` discover its books, and its plugins load,
exactly as the built-ins do. Run a book directly through gumshoe's launcher:

```shell
$ ./run                              # your casebook's books appear in the list
```

Everything is pure babashka - the extension's code is added to the classpath at
runtime, so no `java`/dependency resolution is ever needed.
