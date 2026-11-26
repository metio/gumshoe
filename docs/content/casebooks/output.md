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

`gumshoe.progress` draws a live task list: on a terminal each row repaints
in place as its work runs and settles; anywhere else each row prints once,
when it settles. A mutation book's post-checks and the prerequisites of
every book run through it, and a book with its own `:action` renders its
phases with the same calls.

### Steps one after another

`run-steps` takes a list of steps, runs them in order and returns one result
per step:

```clojure
(require '[gumshoe.progress :as progress])

(progress/run-steps
 [{:label "database restored" :run #(restored? ctx)}
  {:label "ingress switched"  :run #(switched? ctx)}]
 {:stop-on-failure? false})
;; => [{:label "database restored" :status :done :ok? true :value true}
;;     {:label "ingress switched" :status :failed :ok? false :value false}]
```

What `:run` returns decides how its row ends:

| `:run` returns or throws | Row | `:ok?` |
|---|---|---|
| `true`, or any other truthy value that is not a map | `:done` | yes |
| `false` or `nil` | `:failure` | no |
| `{:ok? true :value v :label "…"}` | `:done`, label replaced | yes |
| `{:ok? false :reason "…"}` | `:failure`, reason underneath | no |
| `{:unread? true :reason "…"}` | `:not-yet`, reason underneath | no |
| `{:skipped? true}` | `:skipped` | yes |
| a map without `:ok?` | `:failure` | no |
| a throw | `:failure` with its message | no |
| a throw from a read that did not answer | `:not-yet`, "could not read: …" | no |

A step is drawn `:done` only when it said so. One that could not read its
answer, threw, or returned a map that names no verdict never is. By default
the first step that is not ok stops the rest, which stay `:not-yet` and come
back with `:status :pending`; `{:stop-on-failure? false}` runs every step.
`checklist` is the same list for `[label thunk]` pairs, answering only
whether every step held.

### Phases that are not a static list

A book that discovers its phases as it goes, or runs some of them in
parallel, opens a board and drives the rows itself. This is the shape of a
status report that reads its answer through about 48 kubectl calls in four
phases:

```clojure
(progress/with-board [board]
  (let [phases [["database restored" database-reads]
                ["secrets copied" secret-reads]
                ["deployment adopted" deployment-reads]
                ["ingress switched" ingress-reads]]]
    (mapv (fn [[label reads]]
            (progress/run! board label
                           (fn []
                             (let [answers (vec (map-indexed
                                                 (fn [i read]
                                                   (progress/status! (format "%d of %d reads" (inc i) (count reads)))
                                                   (read))
                                                 reads))]
                               (cond
                                 (some nil? answers) {:unread? true :reason "a read did not answer"}
                                 (every? true? answers) {:ok? true}
                                 :else {:ok? false})))))
          phases)))
```

- `(progress/add! board label)` adds a row, not yet started, and returns its
  index; `(progress/run! board label-or-index thunk)` runs work as that row
  and settles it with what the work returned or threw.
- `(progress/start! board i)` and `(progress/settle! board i returned)` do
  the same in two calls, for work that does not fit one function.
- `(progress/status! "12 of 48 reads")` replaces the running row's status
  line. Off a terminal it prints the line at once, so a log still shows the
  wait as it happens. Outside a list it prints an `:in-progress` item.
- `with-board` returns what its body returned. When the body ends, normally
  or by a throw, every row is printed for good, rows never started as
  `:not-yet`.

### Printing inside a step

What a step prints on stderr while the list is open is kept and drawn under
its row, so a repaint never overwrites it; on a terminal the last lines show
while the step runs. A step's result on stdout still goes to stdout when
stdout is a pipe or a file, so `--output json | jq` keeps working. A list or
spinner started inside a step prints plain lines into that step's text.

A step must not run a child process that writes to the terminal directly
(an effect streaming its output): that output bypasses the list and the next
repaint overwrites it.

### Parallel tasks

`watch-futures!` animates a set of tasks running at once and returns their
values in order once all are done:

```clojure
(progress/watch-futures! [["pods" (future (read-pods))]
                          ["events" (future (read-events))]])
```

### Output off a terminal

When stderr is not a terminal, or `NO_COLOR` or `--no-color` is set, a list
writes plain lines: no cursor movement and no colour, also in what its
steps printed. Pass `{:live? false}` to `run-steps` or `with-board` to ask
for plain lines anyway.

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
