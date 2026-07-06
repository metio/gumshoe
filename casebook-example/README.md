<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# casebook-example

A template for a **casebook** - a team's own books and plugins on the
[gumshoe](../README.md) engine. gumshoe is a git dependency; you never clone it.

## Bootstrap

1. Copy this directory into a fresh git repo, e.g. `casebook-storage`.
2. In `bb.edn`, pin a gumshoe release (`:git/tag` + `:git/sha`).
3. Add your books under `runbooks/` and your plugins under `libraries/`. Books
   `(:require [gumshoe.detective ...])` etc.; the engine comes from the dep.

## Run

```shell
bb gumshoe      # front door; your books sit beside gumshoe's built-ins
bb detect       # scan for symptoms
bb run cordon   # launch a book by name
```

gumshoe's `catalog` discovers books from the whole classpath, so your `runbooks/`
and gumshoe's built-ins (arriving via the dep) are found together - no manifest,
no `:book-paths`. Register announcers / detectives / capability detectors by
listing your namespaces under `:plugins` in `env.edn`; they are already on the
classpath via the dep graph.

The first `bb` run resolves the git dep through tools.deps (needs a JVM - the
gumshoe devShell provides one) and caches it under `~/.gitlibs`; later runs are
offline and instant.
