<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe-ui-charm

A babashka-native UI backend for [gumshoe](../../README.md), built on
[charm.clj](https://github.com/TimoKramer/charm.clj). Selecting it replaces the
fzf and gum binaries with an in-process TUI - one fewer thing to install, and the
picker themes consistently with the rest of gumshoe.

## Enable

```clojure
;; bb.edn
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/ui-charm"}}}
;; env.edn
{:plugins [gumshoe.ui.charm]
 :ui :charm}
```

With `:ui :charm`, `gumshoe.ui` routes every pick / prompt / confirm through
charm.clj; without it (or unset) gumshoe keeps the built-in fzf/gum.

## Status

This backend needs verification on a real terminal — a TUI can't run in a headless
test suite, so it is intentionally **not** on the monorepo's `bb test` classpath.
The Elm loops follow charm.clj's documented `program/run` API; two accessors that
read a value out of a component's final state (`selected-item`, `input-value`) are
isolated at the top of `gumshoe.ui.charm` and are the place to adjust if the
installed charm.clj names them differently. Fuzzy-filtering (fzf's incremental
match) is a follow-up; the first cut is a scrollable select.
