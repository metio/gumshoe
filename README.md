<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe

The SRE detective. An engine for **books** - runbooks, playbooks, firebooks,
and detectives - that helps you investigate what's wrong, fix it safely, and
announce the change. This repo is the engine plus a library of generic,
reusable books; a team's own books and configuration live in a **casebook**
repo that depends on gumshoe (see [Casebooks & plugins](#casebooks--plugins)).

## Definitions

- **Runbook**: Low-level book that performs a single action with a single
  return value.
- **Playbook**: High-level book that performs multiple tasks and often requires
  manual user interaction.
- **Firebook**: Book used during fire drills that introduces some sort of
  (predictable) error that a team wants to resolve during the drill.
- **Detective**: Read-only book that extracts data from a cluster and points at
  possible issues. Detectives compose: run one, run a scope (workloads,
  gitops, ...), or run the whole registry at once.

## Getting started

The five-minute version, no prior knowledge assumed.

**With nix (recommended)** - one command installs every tool any book needs:

```shell
git clone <this repository> && cd gumshoe
nix develop        # babashka, kubectl, flux, psql, dig, fzf, gum, ...
kubectl krew install netshoot  # once, only for the netshoot books
./gumshoe                      # the guided menu - it asks what you're doing
```

**With a container (ilo)** - if you would rather not install anything on the
host, `dev/Containerfile` builds an image with every tool on board. It builds
the same toolset the flake defines, so it never drifts from `nix develop`. Copy
`dev/ilo.rc.example` to `.ilo.rc` (gitignored - it holds host-specific paths),
adjust the mounts to your machine, and:

```shell
ilo shell        # builds the image once, drops you in with every tool
./gumshoe        # then work exactly as you would on the host
```

The mounts in `.ilo.rc` are the host-specific wiring: `--network host` so books
reach private hosts over your VPN, plus your kubeconfig, SSH keys, GnuPG, and
gopass config. Nothing host-specific is baked into the image.

**Without nix** - install [babashka](https://babashka.org/) plus whatever the
book you want asks for. You do not need to guess: every book checks its
prerequisites first and tells you exactly which tool is missing. The usual
suspects: `kubectl`, `fzf`, `dig`, `gopass`. For everything else, run the book
and read the checklist it prints.

```shell
bb runbooks/detectives/cluster.clj    # it tells you if a tool is missing
```

**Your first steps, in order:**

1. `./gumshoe` - the one front door. Not sure where to start? Run it and it
   asks what you are doing and routes you: **follow a lead** (a URL a customer
   gave you, a pod, a node - drill from there object to object, crash logs and
   events surfaced for you, until you reach the cause), **scan for symptoms**
   ("what hurts?" across an area), **run a book** (a single action), **run a
   playbook** (a multi-step procedure), or **practice a fire drill** (break
   something on purpose to train on). Already know the mode? Skip the menu with
   a shortcut: `./gumshoe --host moodle.example.org` drills straight in,
   `./gumshoe cordon` fuzzy-launches a book by name. The three modes are also
   directly reachable as their own scripts - `./investigate`, `./detect`, and
   `./run` - for muscle memory and scripting; each is a thin wrapper over the
   same front-door function.
2. `bb <any book> --help` - every book explains itself, its options, and its
   defaults.
3. Run any detective - they are read-only and can never break anything.
4. Run a runbook without flags - it walks you through selection interactively,
   states exactly what it is about to do, and asks before doing it. Nothing
   happens without your explicit approval, and destructive actions require
   literally typing `yes`. Add `--dry-run` to any change to see the exact
   commands it would run without touching anything.
5. Watch the last line: every book ends with a green DONE or a red FAILED
   banner. If it is not green, nothing half-happened silently - read the
   messages above it.

Every book checks its own prerequisites before doing anything: required tools,
connectivity to the systems it touches, and the permissions of the user running
it.

**Host-specific config (`env.edn`).** Optional, gitignored, per machine. Run
`bb runbooks/setup/init.clj` and it builds one *with* you - detecting the
Kubernetes cluster you are on and the VPN-looking interfaces on this host,
asking for your ceph mgr host, and writing a tidy env.edn. Or copy
`env.edn.example` to `env.edn` and edit by hand. It lets you name your VPN
interface and your ceph mgr hosts without touching code. When set, the
storage-resize books tail the ceph cluster log over SSH while they wait (gated
on your VPN being up), so a resizer error surfaces live; on a machine with no
`env.edn` those books simply watch the namespace events instead. A missing or
broken file degrades to no config, never an error.

If you operate more than one environment, declare each as a named bundle under
`:environments` and tie it to a cluster with `:select`. The active one is
chosen by the kubectl context you are already on - so switching your context to
staging automatically uses staging's ceph host and VPN, and production uses
production's. Selecting the cluster selects the rest. Run the setup wizard once
per cluster and the environments accumulate; a flat single-environment
`env.edn` still works unchanged. See `env.edn.example` for both forms.

**Nothing crashes ugly, and nothing hangs forever.** The execution layer never
throws: a missing tool, an unreachable host, malformed output from a command,
or an unexpected exception anywhere in a book all resolve to a clean message
and the red FAILED banner - never a raw stack trace - and the exit code stays
honest. A single broken detective reports itself and the rest of the
investigation still runs. A resource type the cluster does not have simply
yields no findings. Every quick read is time-bounded so a wedged API server or
DNS server slows a book by seconds instead of hanging it - while genuinely
long-running work (port-forwards, drains, `exec`, upgrade watches, interactive
sessions) streams without a timeout and keeps running for as long as it needs.
Every terminal state ends with a banner and a next step: what to install, what
to fix, or what to run.

**Reproducers.** After a read-only book finishes, it writes the exact sequence
of CLI calls it made to a throwaway shell script and copies the run command to
your clipboard (wl-copy on Wayland, xclip/xsel on X11, pbcopy on macOS). Hand
it to a colleague, paste it into an incident ticket, or run it yourself to
regenerate the same data by hand - the tooling stays transparent and learnable.
Secrets never travel in command arguments, so the reproducer is always safe to
keep.

**Recordings.** Every run leaves a timestamped audit trail under
`recordings/<book>/<when>/` (gitignored): `input.edn` (the flags it was given,
with any token/password/secret value redacted), `selections.edn` (what you
picked interactively, and how you answered each confirmation), `commands.sh`
(the CLI calls), and `meta.edn` (who ran it, on which branch/commit, and the
outcome). The path is printed in the run summary right under the completion
banner, so you always know where the record is; when you need to answer "what
did that change, and with what inputs" a week later, it is on disk.

**Investigate together.** When a read-only book turns something up, it offers
(on a real terminal, only if [upterm](https://upterm.dev/) is installed,
defaulting to No) to open a shared terminal. upterm prints an SSH join command
you hand to your team and they attach instantly - access is key-gated. Start
one proactively any time with `bb runbooks/collaborate/share.clj` (restrict who
joins with `--with alice --with bob`).

## Execution

```shell
bb runbooks/<GROUP>[/<SUB_GROUP>]/<ACTION>.clj [options]
```

The `bb <path>` form runs a book by its file path - the way you work **inside a
clone of this repo**, where every book is present on disk. From a **casebook**
(where gumshoe is a git dependency), its built-in books live in the dependency
cache (`GITLIBS`, i.e. `~/.gitlibs`), not your working tree, so a repo-relative
`bb runbooks/...` finds nothing. Launch them through the front door instead -
`bb run <name>`, `bb detect`, `bb investigate`, or `bb gumshoe <search>` - which
resolves a book from the classpath wherever it physically sits and runs it. Your
casebook's own books (under its `:paths`) run by path either way. The examples
below use the in-repo path form; from a casebook, read them as `bb run <book>`.

Every book behaves the same way:

- `-h`/`--help` prints a description and all options.
- Every flag is optional where interactive selection can fill the gap: omit
  `--node`, `--namespace`, etc. and pick the item with fuzzy selection (fzf)
  instead. An invalid flag value falls back to selection instead of failing.
- **`--dry-run` on any book that changes something.** It walks the selection,
  shows exactly the commands it *would* run (`would run: kubectl cordon
  worker-3`), and touches nothing - no prompt, no announcement. Books whose
  mutation is expressed as an effect plan get this for free; a book that can not
  be safely dry-run refuses under `--dry-run` rather than risk it.
- Changes to a running system are verified twice: first you select or provide
  the item, then the book states exactly what it is about to do and asks for
  approval. Destructive actions get an unmissable red banner and always require
  a literal typed `yes`; other changes use
  [gum](https://github.com/charmbracelet/gum)'s yes/no picker when gum is
  installed.
- Every change ends with a **post-check**: the book waits until the intended
  state actually holds (node unschedulable, PVC gone, HelmRelease Ready, dump
  complete, ...) and fails loudly if it cannot verify the result. "Probably
  worked" is not a result. A few checks are *best-effort* - for state that only
  converges on a pod restart or a background compaction (a filesystem resize, a
  TSDB tombstone cleanup) - where a timeout is a clear caveat, not a red
  failure, because the operation itself already succeeded.
- **The wait is spent watching.** While a post-check polls, it quietly watches
  the infrastructure react - Warning events in the namespace, a PVC's resize
  conditions - and surfaces anything that looks wrong the moment it appears
  (`while waiting — event VolumeResizeFailed on PersistentVolumeClaim/data-0`).
  Dead time becomes live diagnosis: if the CSI resizer errors while you wait for
  a volume to grow, you see it right there.
- Books validate their inputs before acting: a restore refuses a truncated
  dump, a resize refuses non-expandable storage classes and shrinking, a dump
  never overwrites an existing file.
- Changes are announced in the matching Matrix changelog room. A failed
  announcement never blocks a runbook - during an incident the changelog room
  itself might be down.
- The exit code tells the truth: `0` on success, `1` on failure, abort, or a
  failed post-check. Detectives exit `1` when they find something critical, so
  they work in scripts and cron.
- Diagnostics go to stderr, results go to stdout - piping and redirecting always
  stays clean.

## Detectives

Detectives collect evidence from the cluster once, then run pure checks over
it. They compose from single domain to full cluster:

```shell
bb runbooks/detectives/cluster.clj        # everything
bb runbooks/detectives/workloads.clj      # controllers, jobs, pods, storage
bb runbooks/detectives/platform.clj       # nodes + calico
bb runbooks/detectives/gitops.clj         # flux sources + reconciliation
bb runbooks/detectives/databases.clj      # CloudNativePG + db-operator
bb runbooks/detectives/observability.clj  # Prometheus, Alertmanager, Thanos
bb runbooks/detectives/events.clj         # Warning events, last hour
bb runbooks/detectives/rbac.clj           # admins, wildcards, secret readers
bb runbooks/detectives/security.clj       # RBAC + pod security + NetPol
bb runbooks/detectives/dns.clj            # nameservers, SOA replication
bb runbooks/detectives/external_dns.clj   # declared hostnames resolve
bb runbooks/detectives/gateway_api.clj    # Gateways, listeners, HTTPRoutes
bb runbooks/detectives/mail.clj           # MX/SPF/DKIM/DMARC/rDNS + TLS
bb runbooks/detectives/matrix.clj         # client/federation APIs, keys
bb runbooks/detectives/thanos.clj         # Thanos Query: stores, rules
bb runbooks/detectives/loki.clj           # Loki readiness + ring health
bb runbooks/detectives/upload_limits.clj -n moodle    # nginx/php-fpm uploads
bb runbooks/detectives/restic.clj -r s3:backups     # backups landing or stale?
bb runbooks/detectives/opennebula.clj --frontend one-fe   # hosts, VMs, stores
./detect                                  # interactive: pick what hurts
bb runbooks/detectives/cluster.clj --output json    # machine-readable, jq-able
```

Diagnostics go to stderr and results go to stdout, so `--output json | jq
'.summary'` and `> report.txt` stay clean. Evidence is collected in parallel
and shared by all detectives of an investigation. A detective whose resource
types are not installed on the cluster simply reports nothing, so composed
investigations degrade gracefully.

Covered so far: the control plane
(apiserver/scheduler/controller-manager/etcd static pods), nodes, pods
(including pods stuck in Terminating), controllers
(Deployments/StatefulSets/DaemonSets/Jobs), storage, the CSI layer (ceph-csi
attachments and driver registration, Released/Failed volumes, local volumes
pinned to missing nodes - see `runbooks/detectives/csi.clj`),
PodDisruptionBudgets and HPAs, ResourceQuotas, cluster CPU/memory overcommit
(failure-tolerant capacity), cert-manager, flux, CloudNativePG and db-operator,
calico (tigera-operator), the prometheus-operator monitoring stack, and Warning
events. The findings map onto the canonical
[kube-prometheus runbook](https://runbooks.prometheus-operator.dev/) alerts
(KubeControllerManagerDown, KubeQuotaFullyUsed, KubeCPUOvercommit, and so on)
but are read straight from the cluster, so they need no metrics backend.

New detectives are plain data in `libraries/gumshoe/detectives/`: a name, the
resource types they need, and a pure `detect` function from evidence to
findings. Register them in `registry.clj` and they join every composed
investigation automatically.

## Ceph

The ceph cluster runs on our own hardware, so the ceph books work over SSH to a
mgr or admin host, where the ceph CLI and keyring already live - the commands
run directly, no sudo, no container. For a host that only has the cephadm
bootstrap binary, `--cephadm-shell` wraps every command in `sudo cephadm shell
-- ...` instead. Nothing is assumed about the host OS beyond ssh.

```shell
bb runbooks/ceph/status.clj --host mgr-1          # status + health detail
bb runbooks/detectives/ceph.clj --host mgr-1      # health, OSDs, PGs, quorum
bb runbooks/ceph/archive_crashes.clj --host mgr-1 # ack inspected crash reports
bb runbooks/ceph/restart_daemon.clj --host mgr-1 --daemon osd.3
bb runbooks/ceph/osd_out.clj --host mgr-1 --osd 3   # data migrates away
bb runbooks/ceph/osd_in.clj --host mgr-1 --osd 3    # data migrates back
bb playbooks/ceph/upgrade.clj --host mgr-1 --target-version 17.2.9  # upgrade
```

SSH runs unattended (BatchMode, short connect timeout) with strict host key
checking, so books fail fast instead of hanging on a password prompt - set up
ssh keys beforehand. The ceph action books refuse to act on a cluster that is
not `HEALTH_OK` unless `--force` is given, which escalates the confirmation to
the destructive kind - and the last in-OSD can never be taken out.

## Security

Secrets never appear in process arguments where every local user could read
them via `ps`: psql and pg_dump get `PGPASSWORD` via the environment, mimirtool
gets its tokens via `GRAFANA_API_KEY`/`MIMIR_API_KEY`, amtool reads basic auth
from a short-lived owner-only `--http.config.file`, and Grafana/Matrix tokens
travel in request headers. Everything secret comes out of gopass at runtime;
nothing is written to disk beyond owner-only temp files that vanish with the
book.

## Playbooks

Playbooks chain runbooks and detectives into full procedures.
`playbooks/kubernetes/node_maintenance.clj` checks PodDisruptionBudgets, cordons
and drains the node, waits while you do the maintenance, uncordons, and verifies
node health with the detectives.

`playbooks/ceph/upgrade.clj` upgrades a cephadm-managed ceph cluster: preflight
(refusing on HEALTH_ERR), local backups of the config/osd/fs/crush/auth dumps
(it refuses to proceed if any backup is empty - the safety net lives on your
machine, not the cluster being changed), then `ceph orch upgrade start`,
watching the progress, and verifying every daemon reached the target version.
`--require-osd-release` runs the one-way ratchet afterwards as a
separately-confirmed step; `--status` just shows current progress.

## Firebooks

Firebooks start a reproducible, contained problem for fire drills - always
inside the `fire-drill` namespace, so a drill can never touch real workloads:

```shell
bb firebooks/kubernetes/crash_loop.clj                # start the fire
bb runbooks/detectives/cluster.clj                    # the team hunts it down
bb firebooks/kubernetes/crash_loop.clj --extinguish   # clean up
```

The drill is over when the detectives are green again. Available drills:
`crash_loop`, `image_pull`, `pending_pvc`.

## Casebooks & plugins

gumshoe is a small engine with plugin seams. It ships only generic, reusable
books and plugins - nothing tied to any one operator. A team keeps its own
books in a separate **casebook** repo that depends on gumshoe, and anyone
extends the engine with **plugins** - official and third-party packages are
first-class peers, and everything loads from configuration, no fork required.

**Casebooks - the git-deps model.** A casebook is its own repo whose `bb.edn`
depends on a pinned gumshoe release. gumshoe is a tools.deps library: its engine
(`gumshoe.*`) and its built-in books land on the classpath, and gumshoe's
catalog discovers the casebook's own books (under `:paths`) right alongside
them.

```clojure
;; casebook bb.edn
{:deps  {io.github.metio/gumshoe {:git/tag "2026.7.5" :git/sha "…"}}
 :paths ["libraries" "runbooks" "playbooks" "firebooks"]
 :tasks {gumshoe {:task (exec 'gumshoe.main/front-door)}
         detect  {:task (exec 'gumshoe.main/detect)}
         run     {:task (exec 'gumshoe.main/run)}}}
```

Run the front doors as tasks - `bb gumshoe`, `bb detect`, `bb run`. Pinning a
released `:git/tag` (with its `:git/sha`) keeps the set reproducible and lets
Renovate bump it. babashka resolves the dep once via tools.deps - which needs a
JVM, provided by the flake's devShell - and caches it under the `GITLIBS` dir.
Copy [`casebook-example/`](casebook-example/) to start one.

**Tool packages.** The optional tools (flux, ceph, cert-manager, ...) live
under `tools/<name>` as separately-addressable packages. Depend on one directly
by subpath, or pin a meta-package that bundles many:

```clojure
;; one tool
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "tools/flux"}}}
;; or everything stable in a single pin
{:deps {io.github.metio/gumshoe {:git/tag "…" :deps/root "meta/all-stable"}}}
```

`meta/all-stable` bundles every stable tool package; `meta/all-kubernetes` the
in-cluster subset. A tool's books arrive on the classpath automatically; a tool
that registers a scope or capability is activated by listing its plugin
namespace under `:plugins` in `env.edn` (each tool's README says which). See
[docs/git-deps-distribution.md](docs/git-deps-distribution.md) for the full
model.

**One plugin, one call, every seam.** A plugin declares everything it provides
in a single manifest and calls `plugin/provide!` once — so you only ever think
in terms of "plugins", and one can extend the whole engine at the same time:

```clojure
(gumshoe.plugin/provide!
  {:announcers    {:irc      (fn [announcer system data message] …)}
   :detectives    {:workloads [ … ]}
   :capabilities  {:ceph     (fn [] boolean)}
   :tools         {"amtool"  {:version-command […] :min-version "0.25"}}
   :pre-hooks     [ … ]  :post-hooks [ … ]  :secrets [ … ]  :themes [ … ]
   :prerequisites {:change-window (fn [value opts] …)}
   :probes        [ … ]  :kinds {"HelmRelease" {:type "…" :edges (fn [o] …)}}})
```

Every key is optional; adding a seam adds a key. The per-seam register
functions below stay available for direct or conditional use — `provide!` is the
unifying convenience.

**The seams (each is one key above, and one register function):**

- **Announcers** - `:announce` in `env.edn` is a list of announcer configs
  (`:matrix`, `:webhook`, …); a change fans out to all of them. A plugin adds a
  type via the `:announcers` key, or directly with
  `(announce/register-announcer! :irc (fn …))` /
  `(defmethod announce/announce-via :irc …)`.
- **Detectives** - a plugin joins a scan:
  `(registry/register! :workloads [ … ])`, resolved when the scan runs.
- **Capability detectors** - `(capabilities/register-detector! :ceph #(…))`, so
  the setup wizard recognises what a cluster can do and books can require it
  with `:cluster-capabilities [:ceph]`.
- **Tool support** - a tool's profile: `(command/register-tool! "mytool"
  {:version-command ["--version"] :min-version "2.0" :prerequisites (fn [opts]
  …)})`. A book that lists the tool in `:installed-tools` inherits its version
  floor and brought checks (a service it must reach, a login) for free, instead
  of repeating them. `register-version-command!` is the version-only shorthand.
  When a tool's CLI differs across majors, `(command/dispatch-by-version "flux"
  {"2.0" … "1.0" …})` picks the right form at call time, so one package stays
  agnostic.
- **Summary providers** - where a scan's findings can be sent. The clipboard
  and a HedgeDoc pad are built in; a plugin adds more with
  `(summary/register-provider! { … })` - Slack, a ticket, a file, a Matrix
  post.
- **Themes** - how output looks. `:default` (emoji + colour), `:ascii` (no
  emoji, for logs/CI), and `:plain` (no colour) are built in; a plugin registers
  more with `(theme/register! { … })`. Selected by `env.edn :theme`.
- **Execution hooks** - `(hooks/register-post-hook! (fn [ctx] …))` observes
  every finished book (outcome, recording path) to push a metric or forward the
  audit trail; `(hooks/register-pre-hook! (fn [ctx] …))` is a global gate that
  can *veto* a run before it starts (a change freeze, a required ack) by
  returning `{:allow? false :reason …}`. Both are bounded; pre-hooks fail *open*
  so a broken gate never blocks emergency response. Distinct from announcers
  (fire on confirmation) and prerequisites (a single book declares its own).
- **Secrets** - which password manager reads secrets at runtime. `:gopass`,
  `:pass`, `:passage`, and `:pasejo` are built in (select with `env.edn :secrets
  {:provider …}`); a `:command` provider drives any other CLI by templates, and
  a plugin registers a native backend with `(secrets/register-provider! { …
  })`.
- **Prerequisite checks** - the built-ins (tools, versions, connectivity,
  secrets, cluster capabilities, permissions) plus any a plugin registers with
  `(prerequisites/register-check! :change-window (fn [value opts] …))` - gate a
  book on org policy the core can't know: a change-freeze window, a ticket, an
  on-call ack. They animate in the Prerequisites checklist like the built-ins.
- **Drill-down probes & subject edges** - extend the investigation. A probe is a
  live action for a subject:
  `(investigation/register-probe! {:kinds #{"HelmRelease"} :tools ["flux"]
  :args (fn [ctx subject] …)})` - offered only when its tools are installed, so
  a tool package brings its probes. A kind teaches the drill-down to traverse a
  CRD: `(subject/register-kind! "HelmRelease" {:type "…" :edges (fn [object]
  …)})` - a Rollout to its ReplicaSets, a CNPG Cluster to its pods.

[`examples/example/plugin.clj`](examples/example/plugin.clj) is a worked plugin
that extends *every* seam through one `provide!` call.

## Tests

The pure logic - resource transformations, detectives, message rendering, plans
and validations - is covered by unit tests. On top of that, a conformance suite
parses every book and enforces the repository contract by machine: license
headers, namespace docstrings, exactly one harness entry point per book, a
description on every book, a description on every option, and no colliding
option aliases.

```shell
bb test
```

## How books are written

Most books are *data*, not code. There are three declarative forms, all held to
the same machine-enforced contract:

- **A detective** is `(detective/book {:description … :detectives …
  :prerequisites …})` - it names a set of detectives and gets the whole
  read-only investigation, `--output`, and reporting for free.
- **A uniform mutation** is `(mutation/book {:select … :confirm … :effect …
  :verify …})` - it declares what to list and pick, the effect plan to run, and
  the post-checks; the select → confirm → announce → execute → verify → banner
  flow is provided once and tested once.
- **A bespoke book** calls `runbook/execute!` directly with its own `:action`,
  for the handful with genuinely custom flow (multi-step orchestration,
  port-forwards, two-step selection). Even these express their change as an
  `:effect` plan where they can, so they dry-run and their mutation is tested.

A change is a plan of **effects as data** (`[[:kubectl "cordon" node]]`),
interpreted three ways: run it, show it (`--dry-run`), or collect it in a test.
That is why a book's mutation can be asserted without a cluster, and why
`--dry-run` came for free.

## Structure

- `./firebooks`: Directory for all **firebooks**
- `./libraries/gumshoe`: shared building blocks (the runbook/mutation/detective
  engines, effects, kubectl helpers, interaction, detectives, watchers)
- `./playbooks`: Directory for all **playbooks**
- `./runbooks`: Directory for all **runbooks**
- `./tools`: optional tool packages (flux, ceph, cert-manager, ...), each a
  git-deps subpath
- `./tests`: unit tests for the shared libraries
