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

**With [breto](https://github.com/metio/breto) (recommended)** - one command
installs every tool any book needs:

```shell
git clone <this repository> && cd gumshoe
eval "$(breto env)"            # babashka, kubectl, flux, psql, fzf, gum, ... on PATH
krew install netshoot          # once, only for the netshoot books
gumshoe                        # the guided menu - it asks what you're doing
```

`git`, `dig`, `openssl`, `ping` and `ssh` come from your system.

**Without breto** - install [babashka](https://babashka.org/) plus whatever the
book you want asks for. You do not need to guess: every book checks its
prerequisites first and tells you exactly which tool is missing. The usual
suspects: `kubectl`, `fzf`, `dig`, `gopass`. For everything else, run the book
and read the checklist it prints.

```shell
bb run runbooks/detectives/cluster.clj    # it tells you if a tool is missing
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
2. `bb run <path> --help` - every book explains itself, its options, and its
   defaults.
3. Run any detective - they are read-only and can never break anything.
4. Run a runbook without flags - it walks you through selection interactively,
   states exactly what it is about to do, and asks before doing it. Nothing
   happens without your explicit approval, and destructive actions require
   literally typing `yes`. Add `--dry-run` to any change to see the exact
   commands it would run without touching anything.
5. Watch the last line: every book ends with a banner that says what
   happened. Green DONE says whether the change was applied and verified,
   applied without post-checks, or found nothing; yellow marks a dry run, an
   abort at the confirmation, or a report with findings; red FAILED says how
   many effects were applied before it stopped and where. Only a green banner
   that says "verified" promises the post-checks held.

Every book checks its own prerequisites before doing anything: required tools,
connectivity to the systems it touches, the directories its options name, and
the permissions of the user running it.

**Host-specific config (`env.edn`).** Optional, gitignored, per machine. Run
`bb run runbooks/setup/init.clj` and it builds one *with* you - detecting the
Kubernetes cluster you are on and the VPN-looking interfaces on this host,
asking for your ceph mgr host, and writing a tidy env.edn. Or copy
`env.edn.example` to `env.edn` and edit by hand. The first file found is
used: the one `GUMSHOE_ENV` points to, then `./env.edn`, then
`$XDG_CONFIG_HOME/gumshoe/env.edn`, then `~/.config/gumshoe/env.edn`. It
lets you name your VPN interface and your ceph mgr hosts without touching
code. When set, the
storage-resize books tail the ceph cluster log over SSH while they wait (gated
on your VPN being up), so a resizer error surfaces live; on a machine with no
`env.edn` those books simply watch the namespace events instead. A missing or
broken file degrades to no config, never an error.

If you operate more than one environment, declare each as a named bundle under
`:environments` and tie it to a cluster with `:select`. The active one is
chosen by the kubectl context you are already on - so switching your context to
staging automatically uses staging's ceph host and VPN, and production uses
production's. Selecting the cluster selects the rest, and
`GUMSHOE_ENVIRONMENT=<name>` overrides the selection. Run the setup wizard
once per cluster and the environments accumulate; a flat single-environment
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
keep. A book that changes something gets no reproducer, even one that posts
nothing to the changelog (a netshoot debug pod), and neither does any run that
reached a change, because replaying its calls would make the change again.

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
you hand to your team and they attach instantly. Start one proactively any time
with `bb run runbooks/collaborate/share.clj --with alice --with bob`: only the
keys GitHub lists for the named users can join (`--with gitlab:carol` names
another provider). Without `--with` the book refuses, because upterm lets anyone
holding the connect string join a session that names no keys, into a shell with
your credentials; `--open` asks for such a session explicitly.

## Execution

```shell
bb run runbooks/<GROUP>[/<SUB_GROUP>]/<ACTION>.clj [options]
```

`bb run <path>` runs a book by the path `bb run --list` (or `bb gumshoe --list`)
prints, such as `runbooks/detectives/cluster.clj`, so a line of the listing
runs as printed. It works the same in a clone of this repo and in a **casebook**
(where gumshoe is a git dependency): the run door resolves the path from the
classpath wherever the book physically sits, the dependency cache (`GITLIBS`,
i.e. `~/.gitlibs`) included, the path stays the same when the gumshoe pin moves,
and a casebook book at the same path wins over the built-in. `bb gumshoe <path>`
does the same, and `bb run <name>`, `bb detect` and `bb gumshoe <search>` find a
book for you. Bare `bb <path>` is babashka loading a file from the working
directory: it runs gumshoe's books only inside a clone of this repo, and from a
casebook it finds only the casebook's own files.

Every book behaves the same way:

- `-h`/`--help` prints a description and all options.
- Every flag is optional where interactive selection can fill the gap: omit
  `--node`, `--namespace`, etc. and pick the item with fuzzy selection (fzf)
  instead. An invalid flag value falls back to selection instead of failing.
- A selecting flag has the same short alias in every book: `-n` for
  `--namespace` (and `--node`), `-N` for `--name`, `-p` for `--pick`, and so
  on (`gumshoe.flags/aliases`). `--name` answered to `-r` in the Flux books
  and to `-s` in the StageSet and JaaS books, and Thanos' `--service` to
  `-m`; those still work, print a deprecation note on stderr naming the
  current alias, and `--help` lists them as deprecated.
- **`--dry-run` on any book that changes something.** It walks the selection,
  shows exactly the commands it *would* run (`would run: kubectl cordon
  worker-3`), and touches nothing - no prompt, no announcement. Books whose
  mutation is expressed as an effect plan get this for free; a book that can not
  be safely dry-run refuses under `--dry-run` rather than risk it. A dry run
  still checks each step's precondition against the current state and warns
  when the real run would refuse that step, then rehearses the steps after it,
  since an earlier step may be what the warned step waits for. The banner
  names those steps and the exit code is 1.
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
- A check whose evidence all came back empty-handed - a CRD the cluster does
  not serve, a read RBAC refuses - is reported as having read nothing, grouped
  by the capability of the tool package it comes from, instead of being
  counted as clean (`▸ 2 checks read nothing on this cluster - argocd (…)`).
  The `json` and `edn` outputs mark it `:unread` in `:checked`. A detective
  that judges a chosen set of subjects - the hosts a scope selected - names
  their evidence keys as `:judges`, and reads nothing when they are all empty.
  A detective that joins several reads names them as `:joins`, and reads
  nothing when any one of them did not answer.
- A run in which no check read anything has not passed: it prints that
  nothing was checked, marks the `json` and `edn` output `:nothing-checked`,
  and exits `1`.
- On a live terminal, section rules and banners fill the terminal's width
  (clamped to 60-120 columns) and finding hints wrap under their text. Piped,
  redirected or recorded output keeps a fixed 80 columns and one line per hint,
  so transcripts stay comparable. `GUMSHOE_WIDTH=<columns>` sets the width
  everywhere.

## Detectives

Detectives collect evidence from the cluster once, then run pure checks over
it. They compose from single domain to full cluster:

```shell
bb run runbooks/detectives/cluster.clj        # everything
bb run runbooks/detectives/workloads.clj      # controllers, jobs, pods, Services, storage
bb run runbooks/detectives/platform.clj       # nodes, cluster autoscaler + calico
bb run runbooks/detectives/upgrade.clj        # before a minor upgrade
bb run runbooks/detectives/databases.clj      # CloudNativePG + db-operator
bb run runbooks/detectives/observability.clj  # Prometheus, Alertmanager, Thanos
bb run runbooks/detectives/events.clj         # Warning events, last hour
bb run runbooks/detectives/rbac.clj           # admins, wildcards, secret readers
bb run runbooks/detectives/security.clj       # RBAC + pod security + NetPol
bb run runbooks/detectives/posture.clj        # probes, image tags, replicas, PDBs, deprecated APIs, template drift
bb run runbooks/detectives/dns.clj            # nameservers, SOA replication
bb run runbooks/detectives/mail.clj           # MX/SPF/DKIM/DMARC/rDNS + TLS
bb run runbooks/detectives/fabric.clj         # EVPN-VXLAN MAC ownership, over ssh
./detect                                  # interactive: pick what hurts
bb run runbooks/detectives/cluster.clj --output json    # machine-readable, jq-able
```

Each [tool package](#casebooks--plugins) brings a scan of the one system it
knows, and registers its detectives into the composed scopes above - so
`platform` already covers calico and `databases` already covers CloudNativePG.
Reach for the focused scan when you know which system is in trouble:

```shell
bb run tools/flux/runbooks/gitops.clj         # flux sources + reconciliation
bb run tools/argocd/runbooks/scan.clj         # Argo CD Applications, syncs, ApplicationSets
bb run tools/stageset/runbooks/scan.clj       # StageSet staged rollouts
bb run tools/flagger/runbooks/scan.clj        # Flagger Canaries
bb run tools/jaas/runbooks/scan.clj           # JaaS snippets + published artifacts
bb run tools/certmanager/runbooks/scan.clj    # certificates, sour ACME orders
bb run tools/gateway/runbooks/scan.clj        # Gateways, listeners, HTTPRoutes
bb run tools/external-dns/runbooks/scan.clj   # declared hostnames resolve
bb run tools/cluster-api/runbooks/scan.clj    # Cluster API Clusters, Machines, MHCs
bb run tools/crossplane/runbooks/scan.clj     # Crossplane packages
bb run tools/traefik/runbooks/scan.clj        # Traefik routes and middlewares that do not resolve
bb run tools/node-exporter/runbooks/facts.clj # host facts from node_exporter, declared vs reporting hosts
bb run tools/karpenter/runbooks/scan.clj      # Karpenter NodePools, NodeClaims
bb run tools/strimzi/runbooks/scan.clj        # Strimzi Kafka, Connect, connectors, topics
bb run tools/kueue/runbooks/scan.clj          # Kueue queues, Workloads
bb run tools/velero/runbooks/scan.clj         # Velero storage, schedules, backups, restores
bb run tools/calico/runbooks/scan.clj         # tigera-operator components
bb run tools/cnpg/runbooks/scan.clj           # CloudNativePG clusters, WAL archiving, connection ceiling (exec)
bb run tools/prometheus/runbooks/scan.clj     # Prometheus, Alertmanager, ThanosRuler
bb run tools/thanos/runbooks/scan.clj         # Thanos Query: stores, rules
bb run tools/loki/runbooks/scan.clj           # Loki readiness + ring health
bb run tools/matrix/runbooks/scan.clj         # client/federation APIs, keys
bb run tools/upload/runbooks/scan.clj -n moodle             # nginx/php-fpm uploads
bb run tools/restic/runbooks/scan.clj -r s3:backups         # backups landing or stale?
bb run tools/opennebula/runbooks/scan.clj --frontend one-fe # hosts, VMs, datastores
bb run tools/ceph/runbooks/scan.clj --host mgr-1            # health, OSDs, PGs, quorum
```

Diagnostics go to stderr and results go to stdout, so `--output json | jq
'.summary'` and `> report.txt` stay clean. Evidence is collected in parallel
and shared by all detectives of an investigation. A detective whose resource
types are not installed on the cluster simply reports nothing, so composed
investigations degrade gracefully. A finding about a pod whose workload (or,
failing that, node) has a finding at least as severe carries that object as its
`parent`; the text report lists it under the parent's finding, and json and edn
keep it with the `parent` field. Where the API records when a problem began - a
condition's transition, a deletion, a creation - the finding carries it as
`since`, and the text report shows how long it has held (`for 40m`, `for 5h`,
`since 2026-11-02`). The posture book's findings are left out of the
whole-cluster scan: they describe risks that may be deliberate choices, so
they run only when asked for. Checks that exec into pods are left out the same
way, and run only in the book that asks for them and its `pods/exec` grant.

Covered so far: the control plane
(apiserver/scheduler/controller-manager/etcd static pods), nodes, pods
(including pods stuck in Terminating), controllers
(Deployments/StatefulSets/DaemonSets/Jobs), storage, the CSI layer (ceph-csi
attachments and driver registration, Released/Failed volumes, local volumes
pinned to missing nodes - see `runbooks/detectives/csi.clj`),
PodDisruptionBudgets and HPAs, ResourceQuotas, cluster CPU/memory overcommit
(failure-tolerant capacity), cert-manager, flux, CloudNativePG and db-operator,
calico (tigera-operator), the prometheus-operator monitoring stack, Warning
events, and - over ssh rather than kubectl - an EVPN-VXLAN fabric's MAC
ownership (guest MACs the fabric attributes to a VTEP that is not their host's,
guest MACs no scanned host owns that only a gateway VTEP reaches, hosts holding both a local tap and a remote entry for one MAC - each with how
long FRR has held the MAC local and whether a BGP route from the stale VTEP still
stands behind it, so an orphan reads apart from a live misadvertisement - and
static fdb entries that outrank what EVPN learns; see
`runbooks/detectives/fabric.clj`). Its hosts, VNIs, gateway VTEPs and guest MAC
prefixes come from `--hosts`, `--vni`, `--gateway-vteps` and
`--guest-mac-prefixes`, else from env.edn's `:evpn` block; without gateway VTEPs
the gateway check is reported as unjudged, never as clean.
The findings map onto the canonical
[kube-prometheus runbook](https://runbooks.prometheus-operator.dev/) alerts
(KubeControllerManagerDown, KubeQuotaFullyUsed, KubeCPUOvercommit, and so on)
but are read straight from the cluster, so they need no metrics backend.

New detectives are plain data: a name, the resource types they need, and a pure
`detect` function from evidence to findings. A detective about the cluster
itself lives in `libraries/gumshoe/detectives/` and is registered in
`registry.clj`; one about a particular operator belongs in that tool's package,
declared under `:detectives` in its `plugin/provide!`. Either way it joins every
composed investigation automatically.

## Ceph

The ceph cluster runs on our own hardware, so the ceph books work over SSH to a
mgr or admin host, where the ceph CLI and keyring already live - the commands
run directly, no sudo, no container. For a host that only has the cephadm
bootstrap binary, `--cephadm-shell` wraps every command in `sudo cephadm shell
-- ...` instead. Nothing is assumed about the host OS beyond ssh.

```shell
bb run tools/ceph/runbooks/status.clj --host mgr-1          # status + health detail
bb run tools/ceph/runbooks/scan.clj --host mgr-1            # health, OSDs, PGs, quorum
bb run tools/ceph/runbooks/archive_crashes.clj --host mgr-1 # ack inspected crash reports
bb run tools/ceph/runbooks/restart_daemon.clj --host mgr-1 --daemon osd.3
bb run tools/ceph/runbooks/osd_out.clj --host mgr-1 --osd 3 # data migrates away
bb run tools/ceph/runbooks/osd_in.clj --host mgr-1 --osd 3  # data migrates back
bb run tools/ceph/playbooks/upgrade.clj --host mgr-1 --target-version 17.2.9  # upgrade
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

A playbook is `(playbook/book {:select … :confirm … :steps […]})`. Its steps are
the same mutation specs the runbooks run (`gumshoe.steps.nodes/cordon`), checks
that read and report, and pauses where the operator acts outside gumshoe. Each
step declares the prerequisites and options it needs, and the playbook checks
all of them before its first step. The checks before the first change run
before the one confirmation, which lists every later step. Before asking, the
playbook judges every change's pick and the refusal of its first change, so it
does not ask for a playbook whose first change will be refused; a change after
another change or after a pause is judged when the run reaches it. A change
that declares `:done` is skipped when it is already done, so a playbook stopped
part-way is run again from the top: the confirmation marks the steps that took
effect, each is judged again when reached, and a run with nothing left asks
nothing. The pauses split
the playbook into segments: without a terminal it stops at a pause and names
the `--from-segment` that continues it, and `playbook/run-segment!` takes one
segment across a batch of subjects.

A run stops at the first step that fails and keeps what the steps before it
applied, so a step whose failure leaves a system parked or half-owned carries
`:recovery`: a string, or a function of the step's context that can read the
state the failure left. The run prints it under the `FAILED` banner when it
stops at that step. The playbook's own `:recovery` covers its steps without
one once something changed, and a mutation book takes `:recovery` as well.

Steps that recur across playbooks come from step libraries: `gumshoe.steps.objects/delete`
deletes a list of objects and proves each one gone, always behind a typed yes (a
step saying `:destructive? false` beside it is refused), and `gumshoe.steps.flux/suspend`,
`resume` and `reconcile` act on one Flux object of any kind. A library playbook
whose steps run in a fixed order offers named positions where a casebook adds its
own steps, through `playbook/with-extra`; `gumshoe.tools.flux-storage` composes
both, and a casebook calls its `(playbook-with {:after-volumes [step]})`.

`--help` of a playbook or a mutation book describes it from its spec: what it
picks, every step in order with where the confirmation and the pauses fall,
what it verifies, and the tools and permissions it needs.

`tools/ceph/playbooks/upgrade.clj` upgrades a cephadm-managed ceph cluster: preflight
(refusing on HEALTH_ERR), local backups of the config/osd/fs/crush/auth dumps
(it refuses to proceed if any backup is empty - the safety net lives on your
machine, not the cluster being changed), then `ceph orch upgrade start`,
watching the progress, and verifying every daemon reached the target version.
`--require-osd-release` runs the one-way ratchet afterwards as a
separately-confirmed step; `--status` just shows current progress.

## Firebooks

Firebooks start a reproducible, contained problem for fire drills. A cluster drill
burns inside the `fire-drill` namespace, so extinguishing it is one namespace
delete and it can never touch real workloads:

```shell
bb run firebooks/kubernetes/crash_loop.clj                # start the fire
bb run runbooks/detectives/cluster.clj                    # the team hunts it down
bb run firebooks/kubernetes/crash_loop.clj --extinguish   # clean up
```

A fabric drill has no namespace to burn in - it edits a hypervisor's forwarding
table - so it is contained by its values instead: the MAC and the destination both
come from RFC 5737 documentation ranges, so no guest can hold that MAC and nothing
is attracted to that destination. It is inert on any fabric, and still exactly the
shape `evpn-static-fdb` reports:

```shell
bb run firebooks/fabric/stale_fdb_entry.clj -H hv-1 -n 4208               # start
bb run runbooks/detectives/fabric.clj --vni 4208                          # hunt
bb run firebooks/fabric/stale_fdb_entry.clj -H hv-1 -n 4208 --extinguish  # clean up
```

The drill is over when the detectives are green again. Available drills:
`crash_loop`, `image_pull`, `pending_pvc`, `stale_fdb_entry`.

A firebook proves its fire with the detective meant to find it: after lighting
it, the book runs that detective until it reports the fire, and after
`--extinguish` until it no longer does, so a drill whose detective has stopped
seeing the fault fails instead of lighting green. An empty or unanswered
reading never counts as the fire being out. Both changes run through the same
engine as every other change, so `--dry-run` shows them without touching
anything, and a cluster drill refuses to burn in or delete a `fire-drill`
namespace that no drill created. A kubernetes drill is a spec naming its
manifests and the detective:

```clojure
(firebook/book
 {:description "Firebook: starts a crash-looping deployment for the team to find"
  :drill "crash-loop"
  :fire "a crash-looping deployment"
  :manifests [(firebook/deployment-manifest {:name "crash-loop" :image "…" :command […]})]
  :prerequisites {:kubectl-can-create ["deployments"]}
  :expect {:detectives pods/detectives
           :fire? (fn [{:keys [summary]} _] (str/includes? summary "CrashLoopBackOff"))
           :timeout 180}})
```

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
 :tasks {gumshoe {:requires ([gumshoe.main :as main])
                  :task (System/exit (main/front-door *command-line-args*))}
         detect  {:requires ([gumshoe.main :as main])
                  :task (System/exit (main/detect *command-line-args*))}
         run     {:override-builtin true
                  :requires ([gumshoe.main :as main])
                  :task (System/exit (main/run *command-line-args*))}}}
```

Run the front doors as tasks - `bb gumshoe`, `bb detect`, `bb run`. `--list`
on `bb gumshoe` or `bb run` prints every book's checkout-relative path, and
either door runs a listed path verbatim. Pinning a
released `:git/tag` (with its `:git/sha`) keeps the set reproducible and lets
Renovate bump it. babashka resolves the dep once via tools.deps - which needs a
JVM, which `@jdk25` in `.breto.edn` provides - and caches it under `~/.gitlibs`.
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
namespace under `:plugins` in `env.edn` (each tool's README says which).

**One plugin, one call, every seam.** A plugin declares everything it provides
in a single manifest and calls `plugin/provide!` once — so you only ever think
in terms of "plugins", and one can extend the whole engine at the same time:

```clojure
(gumshoe.plugin/provide!
  {:announcers    {:irc      (fn [announcer system data message] …)}
   :detectives    {:workloads [ … ]}
   :capabilities  {:ceph     (fn [] true|false|nil)}
   :tools         {"amtool"  {:version-command […] :min-version "0.25"}}
   :pre-hooks     [ … ]  :post-hooks [ … ]  :secrets [ … ]  :themes [ … ]
   :prerequisites {:change-window (fn [value opts] …)}
   :probes        [ … ]  :kinds {"HelmRelease" {:type "…" :edges (fn [o] …)}}
   :fetched-edges {"StageSet" (fn [context subject object] …)}})
```

Every key is optional; adding a seam adds a key. The per-seam register
functions below stay available for direct or conditional use — `provide!` is the
unifying convenience.

**The seams (each is one key above, and one register function):**

- **Announcers** - `:announce` in `env.edn` is one announcer config or a list
  of them (`:matrix`, `:webhook`, …); a change fans out to all of them. A plugin adds a
  type via the `:announcers` key, or directly with
  `(announce/register-announcer! :irc (fn …))` /
  `(defmethod announce/announce-via :irc …)`.
- **Detectives** - a plugin joins a scan:
  `(registry/register! :workloads [ … ])`, resolved when the scan runs.
- **Capability detectors** - `(capabilities/register-detector! :ceph #(…))`, so
  the setup wizard recognises what a cluster can do and books can require it
  with `:cluster-capabilities [:ceph]`. A detector returns true when the
  capability is present, false when the cluster answered that it is absent,
  and nil when the probe did not answer; `kubectl/serves-crd?`,
  `kubectl/resource-exists?` and `kubectl/any-matching?` answer that way.
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
  returning `{:allow? false :reason …}`. A pre-hook's `:change?` is true for
  every book that changes something, including mutation books that post nothing
  to the changelog; a bespoke book that posts nothing declares `:change? true`,
  or `:change? (fn [opts] …)` when only a flag makes it change something. Both are bounded; pre-hooks fail *open*
  so a broken gate never blocks emergency response. A post-hook runs after a
  scan printed its report on stdout, so hooks, like announcers, print to
  stderr. Distinct from announcers (fire on confirmation) and prerequisites (a
  single book declares its own).
- **Secrets** - which password manager reads secrets at runtime. `:gopass`,
  `:pass`, `:passage`, and `:pasejo` are built in (select with `env.edn :secrets
  {:provider …}`); a `:command` provider drives any other CLI by templates, and
  a plugin registers a native backend with `(secrets/register-provider! { …
  })`. A book that looks up the credentials for a URL takes only a store path
  naming the URL's host as a whole segment (`monitoring/a.example.com`), never
  one that merely contains it (`api/data.example.com`), so no host receives
  another host's credentials.
- **Prerequisite checks** - the built-ins (tools, versions, connectivity,
  secrets, directories an option names, cluster capabilities, permissions) plus any a plugin registers with
  `(prerequisites/register-check! :change-window (fn [value opts] …))` - gate a
  book on org policy the core can't know: a change-freeze window, a ticket, an
  on-call ack. They animate in the Prerequisites checklist like the built-ins.
- **Drill-down probes & subject edges** - extend the investigation. A probe is a
  live action for a subject:
  `(investigation/register-probe! {:kinds #{"HelmRelease"} :tools ["flux"]
  :args (fn [ctx subject] …)})` - offered only when its tools are installed, so
  a tool package brings its probes. A kind teaches the drill-down to traverse a
  CRD: `(subject/register-kind! "HelmRelease" {:type "…" :edges (fn [object]
  …)})` - a Rollout to its ReplicaSets, a CNPG Cluster to its pods. Those edges
  are pure, read off the object in hand. For a relation only the apiserver can
  answer - a reverse lookup, a label query - there is
  `(investigation/register-fetched-edges! "StageSet" (fn [context subject object]
  …))`, which is how a StageSet reaches the StageInventories it applied through:
  a shard's name carries a hash, so nothing about it can be derived. Such a
  builder is isolated and capped, so a query against an unreachable apiserver
  costs its own edges and the drill-down carries on.

[`examples/example/plugin.clj`](examples/example/plugin.clj) is a worked plugin
that extends *every* seam through one `provide!` call.

## Agents

The repository is a Claude Code plugin. Its skill in
[`skills/gumshoe/`](skills/gumshoe/SKILL.md) tells an agent what gumshoe
covers and how to use it without a terminal. The agent lists the books with
`./run --list`, runs detectives with `--output json`, and prepares a change
with `--dry-run` for a human to confirm. Install it with:

```text
/plugin marketplace add metio/gumshoe
/plugin install gumshoe@gumshoe
```

## Tests

The pure logic - resource transformations, detectives, message rendering, plans
and validations - is covered by unit tests. On top of that, a conformance suite
parses every book and enforces the repository contract by machine: license
headers, namespace docstrings, exactly one harness entry point per book, a
description on every book, a description on every option, no colliding
option aliases, and every book loading (without running) so a symbol that
resolves to nothing fails the suite. Every mutation spec a book or library
defines is judged as it evaluates: it states a confirmation action, whether
it is destructive, its prerequisites, an effect, and a post-check or the
reason it has none (`:no-verify "why"`), and a declared `:pin` follows the
picked namespace. The rules live in `gumshoe.conformance`, so a casebook runs
the same checks against its own books and libraries with `bb verify` (see
`casebook-example/bb.edn`).

```shell
bb test
```

The remediation runbooks also have a smoke test that runs them against a kind
cluster and checks what each leaves behind: the change when it was confirmed,
nothing when it was refused or declined. It refuses any context that is not a
kind cluster.

```shell
kind create cluster --name gumshoe
bb smoke-runbooks
```

## How books are written

Most books are *data*, not code. There are five declarative forms, all held to
the same machine-enforced contract:

- **A detective** is `(detective/book {:description … :detectives …
  :prerequisites …})` - it names a set of detectives and gets the whole
  read-only investigation, `--output`, and reporting for free. A scan whose
  evidence comes from somewhere other than the cluster's resources - DNS,
  SSH, an HTTP API - adds `:collect (fn [opts] evidence)`, and `:around (fn
  [opts run] …)` when the collection needs a port-forward or a connection
  check around it. Its checks that read nothing are still marked, so a
  collection that came back empty fails with "nothing was checked". A
  playbook gates on a set of detectives with `(detective/check-step {:title …
  :detectives …})`.
- **The options a select reads** are derived from it:
  `(select/options selection {…})` gives each select flag its conventional
  alias, its coercion and a description from the `:label`, and merges the
  book's own options and rewordings over them.
- **A uniform mutation** is `(mutation/book {:select … :confirm … :effect …
  :verify …})` - it declares what to list and pick, the effect plan to run, and
  the post-checks; the select → confirm → announce → execute → verify → banner
  flow is provided once and tested once. A change to one object declares it as
  `:subject` and says why it must not run with `:refusal`; the engine parses
  the pick, reads and pins the object, and prints the refusal. The refusal
  is judged before the confirmation, so the operator is not asked to approve
  a change that will be refused, and again after it over fresh reads. A
  change with `:done true` is skipped, asking nothing, when its post-checks
  already hold; a `:done` function narrows them and never replaces them. A change to
  several objects declares them as `:subjects`, and each is read, pinned and,
  when it moved after the confirmation, named in the refusal. Live state the
  judgement rests on that no subject covers is read again after the
  confirmation with `:recheck`, whose map `:refusal` and `:precondition` see
  over what `:derive` read before it. Within each of the two phases, before
  and after the confirmation, an object is fetched once and independent
  reads run concurrently (`gumshoe.concurrent/values` for a `:derive` of
  its own); post-checks read the cluster on every poll. A suspend/resume
  or pause/resume pair is one `gumshoe.toggle/pair` declaration whose `:on` and
  `:off` specs differ only in the wanted state and their words
  (`gumshoe.steps.workloads/cronjob-suspending`). A change acting over SSH or another API
  names its target with `:connect`, which returns the context the callbacks
  and the effect receive and the names the confirmation and the changelog
  show (`gumshoe.ceph/connect`); a debug session that announces nothing sets
  `:announce? false`. These keys are aliases of one canonical change map,
  which `gumshoe.change/normalize` builds before the engine runs; a spec
  that names one thing two ways is refused when the book loads (see the
  namespace docstring for the map and the aliases). A spec a playbook shares lives in a
  library, and the runbook names its description beside it:
  `(mutation/book (merge nodes/cordon {:description "…"}))`, so the catalog
  reads it without loading the book.
- **A playbook** is `(playbook/book {:select … :confirm … :steps […]})` - the
  steps are mutation specs, checks and pauses, and the prerequisites and
  options come from the steps.
- **A dispatcher** is `(dispatch/book {:select … :classify … :routes {…}})` -
  it picks a subject, asks it what it is, and runs the playbook that kind
  routes to, applying that route's option defaults once the route is known.
- **A bespoke book** calls `runbook/execute!` directly with its own `:action`,
  for the handful with genuinely custom flow (multi-step orchestration,
  port-forwards, two-step selection). Even these express their change as an
  `:effect` plan where they can, so they dry-run and their mutation is tested,
  and pick their subject with `(select/pick spec opts context)`, which lists
  the candidates under strict reads, so a cluster that did not answer is
  reported as such and never as one with nothing to pick. A book that validates a plan
  and refuses before the confirmation hands the change it planned to
  `mutation/run-on!`, which pins the subject, dry-runs the plan and prints
  the change's `:recovery` when it stops part-way; the volume resize books
  do this.

A change is a plan of **effects as data** (`[[:kubectl "cordon" node]]`),
interpreted three ways: run it, show it (`--dry-run`), or collect it in a test.
That is why a book's mutation can be asserted without a cluster, and why
`--dry-run` came for free.

Detectives and post-checks read status conditions through `gumshoe.conditions`,
one set of pure functions with one meaning: a missing condition is neither True
nor False, the first of a repeated type counts, staleness against
`metadata.generation` is asked separately with `stale?`, and an unreadable
timestamp yields nil rather than an exception.

## Changes written as data

**The format is experimental.** Its vocabulary has not been reviewed yet,
and any word of it may change in any release. Every change written in it
declares `:gumshoe/dsl :v0-experimental`, a change declaring anything else
is refused, and every run of an EDN book prints a note on stderr saying the
format may change.

A change to one object can be written as plain EDN, and
`gumshoe.dsl/compile` turns it into the change map `mutation/book` and
`mutation/run-on!` run. The data holds no code: reads, judgements and
effects are words of a small vocabulary, and anything beyond it is a
library function registered under a keyword. Every safeguard gumshoe has is
on unless the change opts out of it with a reason. This is the CronJob
suspend book `runbooks/kubernetes/cronjobs/suspend.clj` written as data, in
`tests/edn-books/runbooks/kubernetes/cronjobs/suspend.edn`:

```clojure
{:gumshoe/dsl :v0-experimental
 :description "Suspends a CronJob so no new Job starts"
 :target {:pick :namespaced :label "CronJob" :type "cronjobs"
          :namespace-flag :namespace :name-flag :job
          :candidates {:unless :state :order :listed}
          :empty "every CronJob is suspended already"}
 :prerequisites {:installed-tools ["kubectl" "fzf"]
                 :cluster-capabilities []
                 :kubectl-can-get ["cronjobs"]
                 :kubectl-can-patch ["cronjobs"]}
 :state {:holds [:field [:spec :suspend] := true]
         :says "CronJob {target} is suspended"
         :timeout 30 :interval 3}
 :refuse [{:when [:state] :says "it is suspended already"}]
 :confirm {:action "suspend a CronJob" :destructive? false}
 :items [:call :gumshoe.workloads/suspend-items [:object] true]
 :announce "Suspend CronJob {target}"
 :effect [[:kubectl :patch {:resource "cronjob" :merge {:spec {:suspend true}}}]]}
```

From this, `compile` derives:

- **A pinned subject.** The picked CronJob is read strictly before the
  confirmation and again after it, and the change is refused when it was
  replaced, its spec changed or it is gone ("the CronJob no longer
  exists", or the target's `:gone`). A target names its object's `:type`,
  or a registered `:parse` that makes a ref of the pick; one with neither
  is refused.
- **A post-check from `:state`.** It judges `:holds` on the object read
  again until `:timeout` passes, and never holds for an object no read
  found. `:holds :absent` holds only when a read answered NotFound or the
  object is terminating.
- **The skip.** A run whose post-check already holds is skipped as done.
- **The candidates.** `{:unless :state}` offers every object of the type
  not in the target state yet.
- **The confirmation.** A `[:kubectl :delete]` makes it destructive, so it
  asks for "yes"; a destructive change declares `:recovery`. A delete beside
  `:confirm {:destructive? false}` is refused, as `gumshoe.change/normalize`
  refuses any effect marked destructive beside it.

Strict reads have no opt-out. The others do: `:opt-out {:verify "why"}`
drops the post-check (the change then has no `:state`), `{:done "why"}`
the skip, `{:destructive "why"}` the typed confirmation of a delete and
`{:recovery "why"}` the recovery text. An opt-out that opts out of nothing
is refused. So is every key, word, registered name or argument count
`compile` does not know, with a message naming where:
`[:state :holds 0]: :matches is no predicate - the predicates are …`.

The vocabulary:

- expressions: `[:target]`, `[:ref :namespace]`, `[:object :spec :schedule]`,
  `[:opt :replicas]`, `[:value :name …]`, `[:read :name …]`, and `[:context]`
  where reads stand;
- reads, under `:reads` (read again after the confirmation) and `:values`
  (computed once): `[:list "type" {:namespace expr}]`, `[:get "type" name
  {:namespace expr}]`;
- predicates: `[:field [:spec :replicas] := 0]` and `:not=`, `:<`, `:<=`,
  `:>`, `:>=`; the same comparisons over two expressions; `[:some? e]`,
  `[:nil? e]`, `[:not p]`, `[:and …]`, `[:or …]`; and in `:refuse`,
  `[:state]` and `[:absent]`;
- effects on the picked object: `[:kubectl :patch {:merge {…}}]`,
  `[:kubectl :scale {:replicas e}]`, `[:kubectl :annotate {:annotations
  {"k" e}}]`, `[:kubectl :delete]`;
- templates: `"Scale {target} to {value.replicas} replica(s)"`;
- registered functions: `[:call :gumshoe.workloads/scale-items [:ref :kind]
  [:object] [:value :replicas]]`. `gumshoe.dsl.library` lists gumshoe's;
  a casebook registers its own with `gumshoe.dsl.registry/register!`,
  naming the kind (`:predicate`, `:reason`, `:items`, `:value`, `:read`,
  `:candidates` or `:parse`), so a function that reads the cluster can only
  stand where reads stand.

### Running an EDN book

A file holding such a change is a book. `bb run` (and `bb gumshoe`) runs it
by its path, with the flags its target derives, exactly as it runs the
Clojure book it mirrors:

```sh
bb run tests/edn-books/runbooks/kubernetes/cronjobs/suspend.edn --help
bb run tests/edn-books/runbooks/kubernetes/cronjobs/suspend.edn --namespace shop --job nightly --dry-run
```

```text
note: tests/edn-books/runbooks/kubernetes/cronjobs/suspend.edn is written in the experimental EDN book format (:gumshoe/dsl :v0-experimental), which may change in any release
```

An `.edn` file below a book root (`runbooks/`, `playbooks/`, `firebooks/`)
of gumshoe or a casebook is listed by `bb run --list` and the menus, and
`bb verify` loads it the way `bb run` does: a file `bb run` refuses is a
finding, and the change it compiles to is held to the same spec contract,
guard rule and fail-closed post-checks as a Clojure book's spec. Data a
casebook's books read therefore lives outside its book roots. gumshoe's own
EDN forms of `cronjobs/suspend`, `cronjobs/resume`, `workloads/scale` and
`objects/delete` live under `tests/edn-books/` rather than `runbooks/`, so
the catalog lists each of those books once, as its Clojure form, while the
format is experimental; `tests/gumshoe/dsl_books_test.clj` holds each to
every transcript its book keeps in the golden harness.

A book file may come from anywhere on disk, and reading one never runs
code from it:

- it is read with `clojure.edn`, so `#=` read-eval, reader conditionals,
  syntax quote, regexes and anonymous functions are refused as syntax;
- every tagged literal is refused, `#inst` and `#uuid` included;
- a file over 64 KiB, nested over 32 collections deep, holding more than
  one form, or not valid UTF-8 is refused before it is compiled;
- `compile` refuses every symbol, unknown key and word, and calls only
  functions registered in `gumshoe.dsl.registry`. The plugins named in
  `env.edn` load before the book compiles, so a casebook registers its own
  functions from a plugin namespace.

A refused file prints the path and the reason and exits 1, for example
`refused book.edn: [:gumshoe/dsl]: the change declares :gumshoe/dsl 0, and
this compiles :v0-experimental - …`.

## Structure

- `./firebooks`: Directory for all **firebooks**
- `./libraries/gumshoe`: shared building blocks (the runbook/mutation/detective
  engines, effects, kubectl helpers, interaction, detectives, watchers)
- `./playbooks`: Directory for all **playbooks**
- `./runbooks`: Directory for all **runbooks**
- `./tools`: optional tool packages (flux, ceph, cert-manager, ...), each a
  git-deps subpath
- `./tests`: unit tests for the shared libraries

## License

[0BSD](LICENSES/0BSD.txt).
