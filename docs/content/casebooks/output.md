---
title: Output primitives
---
<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

Every book prints through a small set of primitives in `gumshoe.stdout`,
and every mark those primitives draw comes from the active theme. A book
that prints only through them changes its appearance when the theme
changes, with no edit to the book. Set the theme with `:theme` in
`env.edn` (`:default`, `:ascii` or `:plain`), or register your own from a
plugin.

## Outcomes

The theme names eight outcomes. Each has a glyph, a colour and, optionally,
a plain fallback drawn when colour is off.

| Outcome | Use it for | `:default` | `:ascii` |
|---|---|---|---|
| `:success` | a check passed | ✅ | `[OK]` |
| `:failure` | a check failed | ❌ | `[ERROR]` |
| `:finding` | something that needs the operator's attention | 🔶 | `[WARN]` |
| `:info` | what is being read or probed | ▸ | `>` |
| `:done` | a piece of work is complete | ✅ | `[OK]` |
| `:not-yet` | a piece of work is still to do | ⬜ | `[TODO]` |
| `:in-progress` | waiting for something to finish | ⏳ | `[WAIT]` |
| `:skipped` | deliberately left out | ⏭ | `[SKIP]` |

The first four say what a check concluded. The last four say how far a
piece of work got, so a status report shows a step that has not run yet as
`:not-yet` rather than as a failure.

## Report lines

```clojure
(require '[gumshoe.stdout :as stdout])

(stdout/print-section "Adoption status")
(stdout/line :done "database restored")
(stdout/line :not-yet "ingress switched")
(stdout/item :finding "certificate expires in 5 days")
(stdout/note "run the book again once the ingress has switched")
```

- `print-section` draws a titled rule; `print-section-marker` an untitled one.
- `line` prints the outcome's mark and the text. `item` does the same one
  level in, for a check in a list under a heading.
- `note` prints a plain line with no mark, and `detail` a plain line one
  level in: an explanation under an item, a command to run by hand, a row of
  a list you lay out yourself.
- `banner` prints a full-width banner in the outcome's colour,
  `(stdout/banner :failure "the restore did not finish")`.

All of them write to stderr, so a book's result on stdout stays clean for
`--output json | jq` and redirects.

The older names keep working and print exactly the same:

| Older name | Prints as |
|---|---|
| `(stdout/ok …)` | `(stdout/line :success …)` |
| `(stdout/error …)` | `(stdout/line :failure …)` |
| `(stdout/warn …)` | `(stdout/line :finding …)` |
| `(stdout/check-ok …)` | `(stdout/item :success …)` |
| `(stdout/check-error …)` | `(stdout/item :failure …)` |
| `(stdout/err-println …)` | `(stdout/note …)` |

## Task lists

`gumshoe.progress` draws a live task list that repaints in place on a
terminal and prints each row once anywhere else:

```clojure
(require '[gumshoe.progress :as progress])

;; steps one after another; stops at the first failure unless told not to
(progress/checklist [["kubectl installed" #(command/installed? "kubectl")]
                     ["helm installed" #(command/installed? "helm")]]
                    {:stop-on-failure? false})

;; tasks running in parallel; returns their values in order
(progress/watch-futures! [["pods" (future (read-pods))]
                          ["events" (future (read-events))]])
```

A pending row shows the `:not-yet` mark, a running one the theme's spinner,
a finished one `:done`, and a failed one `:failure`. A step's thunk may
return `{:ok? true :label "kubectl (v1.36)"}` to replace its label with what
it found.

## Tables and commands

```clojure
(stdout/print-table [["POD" :pod] ["NODE" :node]] rows)
(stdout/print-command "kubectl" "get" "pods" "-n" "web")
```

`print-table` aligns the columns, colour codes excluded, and prints nothing
for an empty table. `table` returns the same text as a string.
`print-command` echoes a command before it runs, so the tooling underneath
stays learnable.

## Glyphs in headings and menus

A heading or a menu label that wants an outcome's mark takes it from the
theme, so it follows the theme like every other mark:

```clojure
(require '[gumshoe.theme :as theme])

(stdout/print-section (str (theme/glyph :in-progress) " Terminating namespaces"))
```

Decoration that is not an outcome, such as 📋 or 🔍 in a section title, and
the finding severities (🔥) are ordinary text.

## Rendering without printing

`gumshoe.render` holds the pure functions behind the printers. Each takes a
context and returns the string, so a test can render under any theme and
colour setting, and a book can compose one rendered line inside another:

```clojure
(require '[gumshoe.render :as render])

(render/line (render/context :default false) :success "fine")
;; => "✅ fine"

(stdout/detail (render/item (stdout/context) :failure "could not read the pods"))
```

`stdout/context` is the context of the current run: the active theme, and
whether colour is on.

## Warnings from child commands

A warning a command writes to stderr is shown under the step that ran it.
kubectl prints an API server warning as `Warning: <message>`: a deprecated
API version, a Warn-mode admission policy, the DaemonSet pods a drain
ignores. Other tools log warnings as klog lines (`W1126 10:15:30.123456 …]`).

- Each effect of a plan that changes something gets its own line,
  `▸ kubectl label ingress web …`, before it runs. Its output streams live
  beneath it, and the warnings its command wrote follow it two levels in,
  marked `:finding`. stdout and every other stderr line are not held back, so
  a long drain stays visibly alive and a failed effect shows its full error.
- A post-check prints the warnings its reads wrote once, under its
  `verified:` or `NOT verified:` line, however many attempts it took.
- An investigation prints, after its evidence list, each warning named by
  the read that provoked it: `🔶 endpoints: Warning: v1 Endpoints is
  deprecated in v1.33+; use discovery.k8s.io/v1 EndpointSlice`.

Your own step gets the same treatment by running its work inside
`shell/collecting-warnings`. Every command the work runs through
`gumshoe.shell`, from any thread it starts, hands its warnings to the
collection, and the step prints them where they belong:

```clojure
(let [{:keys [value warnings]} (shell/collecting-warnings #(read-the-ingresses context))]
  (stdout/line (if value :success :failure) "ingresses read")
  (stdout/child-warnings warnings)
  value)
```

`gumshoe.child-output/split-stderr` splits a captured stderr into its
warnings and everything else, for a book that captures a command itself.

An effect that hands the terminal to the operator, such as an interactive
shell, is wrapped in `effect/interactive`. It keeps stdin, stdout and
stderr on the terminal, so its prompts work, and its warnings are not
collected:

```clojure
(effect/plan (effect/interactive (effect/kubectl-plugin context "netshoot" "debug" (str "node/" node))))
```

gumshoe does not pass kubectl's `--warnings-as-errors`. kubectl applies that
flag after the request has run, so a change that tripped a Warn-mode policy
would be applied and still reported as failed.

## Your own theme

A theme sets only what it changes; everything else comes from the default.

```clojure
(gumshoe.plugin/provide!
 {:themes [{:name :dots
            :outcome {:success {:glyph "●" :color :green :plain "[ok]"}
                      :failure {:glyph "●" :color :red :plain "[fail]"}}}]})
```

Select it with `:theme :dots` in `env.edn`. The colours are `:red`,
`:green`, `:yellow`, `:blue`, `:bold` and `:dim`. Give a `:plain` fallback
whenever two outcomes share a glyph and differ only by colour: output
without colour (a pipe, a log file, `NO_COLOR`, `--no-color`) draws the
fallback, so success and failure stay distinguishable. A theme written with
the single-glyph keys `:ok`, `:error`, `:warn`, `:check-ok`, `:check-error`,
`:pending` and `:bullet` still applies; each sets the outcome it stands for.

## Conformance

`gumshoe.conformance` names an outcome glyph written by hand inside a
printed string - in `println`, `print`, `str`, `format`, `err-println` or one
of the `gumshoe.stdout` printers - as a `glyph` line with the line of the
call. Such a glyph ignores the theme: under `:ascii` it still prints the
emoji, and a reader sees two marks for one meaning.

By default these lines are notes and do not fail `bb verify`. Once your
books and libraries print through the primitives above, make the check
gate:

```clojure
verify {:doc "Check the casebook's books and libraries"
        :requires ([gumshoe.conformance :as conformance])
        :task (conformance/verify! {:license "0BSD" :glyphs :require})}
```

Files below a `tests` directory are exempt, because a test compares against
the glyphs it expects.
