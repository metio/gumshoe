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

A TUI can't run in a headless test suite, so the backend is not on the
monorepo's `bb test` classpath. Its programs' states and update functions are
pure, and `bb test` in `tools/ui-charm` runs them against the pinned charm.clj
release without a terminal; the loop that drives them, `program/run`, still
needs a real terminal to check. A confirmation starts on "no", so a bare Enter
declines, as the built-in `gum confirm --default=false` does. The backend
declares `:confirm-options?`, so with `:confirmations {:destructive :picker}`
in `env.edn` it also asks for destructive actions, offering the negative label
first and the consequence gumshoe names as the affirmative one. Space marks
values in a multi-select. Fuzzy-filtering (fzf's incremental match) is a
follow-up; the first cut is a scrollable select.
