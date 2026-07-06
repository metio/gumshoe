<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# Distribution: git deps (this branch)

This branch sketches gumshoe distributed as a **tools.deps git dependency**
instead of a cloned repo. The trade for a one-time JVM requirement is real
versioning, reproducibility, Renovate bumps, and the deletion of gumshoe's
hand-rolled extension layer.

## The shape

- **gumshoe** is a library. Its `deps.edn`/`bb.edn` put `libraries` **and** the
  book dirs on `:paths`, so a dependent gets the engine (`gumshoe.*`) and the
  built-in books on its classpath.
- **A casebook** is a normal bb project that depends on gumshoe:

  ```clojure
  ;; casebook bb.edn
  {:deps  {io.github.metio/gumshoe {:git/tag "2026.7.5" :git/sha "…"}}
   :paths ["libraries" "runbooks" "playbooks" "firebooks"]
   :tasks {gumshoe {:task (exec 'gumshoe.main/front-door)}
           detect  {:task (exec 'gumshoe.main/detect)}
           run     {:task (exec 'gumshoe.main/run)}}}
  ```

- **Book discovery is classpath-based.** `gumshoe.catalog/book-roots` reads every
  classpath entry named `runbooks`/`playbooks`/`firebooks` - gumshoe's own (from
  `~/.gitlibs`), the casebook's, and any tool package's. tools.deps resolved the
  graph; catalog just reads it. `short-name` strips up to the book-root segment,
  so a dependency's book (`/…/gitlibs/…/runbooks/x.clj`) reads the same as a local
  one (`runbooks/x.clj`).

## What this deletes

- `gumshoe.extensions` (manifest reading, runtime `add-classpath`,
  `BABASHKA_CLASSPATH` launch plumbing) - **gone**; tools.deps owns the classpath.
- `env.edn :extensions`, the `gumshoe.edn` manifest, `:book-paths` - **gone**.
- The entry points stop shelling `bb <path>` with a doctored classpath; a book
  runs under the casebook's own `bb.edn`, so its deps are already present.

## What stays

- Every registry seam (`announce-via`, detective/capability/summary registries).
- `:plugins` in `env.edn` - still names namespaces to `require` for their
  registrations; they are on the classpath via the dep graph now, not add-classpath.
- The CalVer GitHub release - this is what a casebook pins.

## Monorepo channels (stable / experimental)

A gumshoe monorepo can offer channels by subpath, pinned per casebook:

```clojure
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "meta/all-stable"}
        ;; opt into new packages, additively:
        io.github.metio/gumshoe-edge {:git/tag "…" :deps/root "meta/experimental"}}}
```

`meta/*/deps.edn` aggregate the tool packages with `:local/root` (intra-repo).

## The one cost

babashka resolves git deps by shelling to tools.deps, which needs `java`. The
flake's devShell carries a JDK, and the resolution is cached after the first
run - but a user on bare babashka with no JDK cannot resolve. That single fact
is the tradeoff of the git-deps model: it assumes a JDK is available in every
environment that resolves dependencies.

The cache lives at `~/.gitlibs` by default. tools.gitlibs honors the `GITLIBS`
environment variable (and the `clojure.gitlibs` system property), so the devShell
points it at the XDG cache - `export GITLIBS="$XDG_CACHE_HOME/gitlibs"` - rather
than dotting `$HOME`; the container image sets the same and `.ilo.rc` mounts the
host's copy so it survives across runs. Wherever this doc says `~/.gitlibs`, read
"the `GITLIBS` cache."

## Not yet done on this branch

- The front doors are still repo-root scripts; the casebook `bb.edn` tasks above
  assume a `gumshoe.main` namespace exposing `front-door`/`detect`/`run`. Moving
  the four scripts' bodies into that namespace is the remaining mechanical step.
- Splitting gumshoe into `core/` + `tools/<tool>/` + `meta/` for real per-tool
  packages and the stable/experimental channels.
