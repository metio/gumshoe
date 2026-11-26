---
name: gumshoe
description: Investigate and operate Kubernetes clusters and the systems around them with gumshoe's books. Use when a repository is gumshoe or a casebook that depends on io.github.metio/gumshoe, when the user mentions gumshoe, runbooks, playbooks, firebooks, detectives or casebooks, or when asked to find out what is wrong with a cluster, a workload, a hostname, DNS, mail, storage, databases, GitOps controllers or backups. Covers finding the right book, running it without a terminal, reading its JSON findings, and handing changes to a human.
---

<!--
SPDX-FileCopyrightText: The gumshoe Authors
SPDX-License-Identifier: 0BSD
-->

# gumshoe

gumshoe is a library of **books**: babashka scripts that read a system and
report what is wrong, or change it safely after a human approves. As an
agent, run the read-only books (detectives and scans) freely with
`--output json`, and prepare every change with `--dry-run`. The change itself
runs only after a human confirms it at their own terminal, because every
change asks for approval on standard input and a destructive one asks for
the typed word `yes`. Never pipe an answer into that prompt; give the human the command
instead.

## Kinds of books

- **Detectives** are read-only. They collect evidence once, run pure checks
  over it, and report findings. They can never change anything.
- **Runbooks** perform one action: cordon a node, restart a workload, take a
  backup. A runbook that changes something confirms first, verifies the
  result afterwards, and announces the change.
- **Playbooks** chain runbooks and detectives into a procedure, such as node
  maintenance or a ceph upgrade, and usually need a human at several steps.
- **Firebooks** start a contained, predictable fault for a fire drill and
  remove it again with `--extinguish`.

## Finding the book

```sh
bb run --list                # in a gumshoe clone, or a casebook with a run task
bb gumshoe --list            # in any casebook with the gumshoe task
```

Each line is a book's path, a tab, and its description, for every book on
the classpath: gumshoe's built-ins, every tool package the project depends
on, and the casebook's own books. The path is relative to the checkout the
book comes from (`runbooks/detectives/fabric.clj`,
`tools/flux/runbooks/gitops.clj`), the same in a gumshoe clone, in a casebook,
and for every gumshoe pin, so a listed path runs as printed:

```sh
bb run runbooks/detectives/fabric.clj --help
bb gumshoe runbooks/detectives/fabric.clj --help
```

Either door resolves the path to the casebook's own file of that path first
and to the classpath book listed under it otherwise, so a casebook overrides
an upstream book by placing a file at the same path; the listing then shows
the casebook's book only. A path that names no book exits 1 and prints the
closest listed paths. An absolute path runs as given.

A listed path ending in `.edn` is a book written in the experimental EDN
change format (`:gumshoe/dsl :v0-experimental`). It runs through the same
doors and takes the same `--help` and `--dry-run`; its first stderr line is
a note that the format may change, which is not a failure. Only `bb run`
or `bb gumshoe` runs one: bare `bb <file>.edn` does not. A file the reader
or `compile` refuses exits 1 with `refused <file>: <path>: <reason>`
before any prerequisite or cluster read.

Bare `bb <path>` is babashka loading a file from the working directory, so
from a casebook it runs only the casebook's own files and answers `File does
not exist` for an upstream book. Where a casebook's shell banner or notes
say `bb <path-to-book>`, run the listed path as `bb run <path>` instead, and
write `bb run <path-to-book> --help` into such a banner. Every hint a book
prints names a book the same way.

`bb run` in a casebook is a task of its `bb.edn`, and babashka's own `run`
answers when the task is missing. Check `bb tasks` first; the `gumshoe` task
needs no override and offers `--list` and paths too. A casebook without the
tasks needs them, as in `casebook-example/bb.edn`:

```clojure
:tasks {gumshoe {:requires ([gumshoe.main :as main])
                 :task (System/exit (main/front-door *command-line-args*))}
        detect  {:requires ([gumshoe.main :as main])
                 :task (System/exit (main/detect *command-line-args*))}
        run     {:override-builtin true
                 :requires ([gumshoe.main :as main])
                 :task (System/exit (main/run *command-line-args*))}}
```

`System/exit` carries the book's exit code to the shell, and
`:override-builtin true` silences babashka's warning that the task replaces
its built-in `run`.

Without a path, `bb run` and `bb gumshoe` take search terms for the fuzzy
finder. An option after a search term (`bb run --no-color drain --dry-run`)
exits 1 with `"--dry-run" was not read`, so the book chosen by the search
never runs without it; put the options first, or run the book by its path.

Filter the list with `grep`, then read the book's options:

```sh
bb run --list | grep -i velero
bb run <path> --help
```

`--help` lists every option with a description. Prefer this over guessing
flags, because the set of books grows with every release and differs between
casebooks.

## Running without a terminal

An agent's shell has no terminal, so give a book everything it would
otherwise ask for:

- **Pass every selecting flag.** A book picks a missing or invalid node,
  namespace or object with fzf, which fails at once without a terminal.
  `--help` names the flags (`--node`, `--namespace`, `--pod`, `--host`, …).
  A selecting flag has one short alias in every book (`-n` namespace and
  node, `-N` name, `-p` pick, …; `gumshoe.flags/aliases` holds the list).
  The aliases `--name` and Thanos' `--service` had before (`-r`, `-s`,
  `-m`) still parse, and print a deprecation note on stderr naming the
  current one; write the current alias or the long flag. A change book,
  playbook or dispatcher takes a target its flags name without asking when
  its candidates offer it, and never falls back to the picker when they do
  not. A named target the candidates leave out exits 0 only when the
  change's own already-done check holds for it (`skipped, already done:
  <step>`); otherwise it exits 1: `no <kind> <namespace>/<name> - not
  found`, the change's `refusing: <reason>` (an unhealthy cluster, a
  running operation), `<kind> <name> is present but not eligible for this
  change`, `could not tell whether <kind> <name> is already done - the
  done-check could not be read: <the read>` (run it again once the API
  answers), or, where the book cannot read the object (no typed subject, a
  multi-pick, a name without its namespace), `not found among the
  candidates`. With no target named, a book with nothing to change says so
  and exits 0.
- **Pass options only.** A book, and `./detect`, refuses any positional
  argument with exit 1 before anything runs, naming the word: babashka.cli
  stops reading options at the first one, so a `--dry-run` or `--output
  json` after it would otherwise be dropped. A boolean flag takes no value
  (`--dry-run`, or `--dry-run=false`), so `--dry-run yes` is refused too.
- **Use `--output json`** (or `edn`) on detectives and scans. Results go to
  standard output and diagnostics to standard error, so `2> scan.log | jq`
  parses the report while the log keeps the prerequisite checklist, any
  error and a change's "refusing: …" reason. A report that could not be
  written to standard output (a full disk, a closed pipe) fails the run
  with exit 1. A json or edn run, or one without a console, never asks to
  share its findings.
- **Read the exit code.** `0` means success or nothing critical; `1` means a
  critical finding, a failure, any abort, or a failed post-check. Every abort
  exits 1 whether or not anything changed: a declined confirmation, a
  precondition that no longer held after it, or a confirmation refused under
  `--dry-run`. The final banner agrees with it: a green `DONE` only exits 0,
  and a `FAILED`, `NOT VERIFIED`, `ABORTED` or `DRY RUN failed` banner only
  exits 1. `ABORTED` is yellow when nothing was changed and red after a
  change had run.
- **Skip the interactive front doors.** `./gumshoe`, `./detect`,
  `./investigate` and `./run` without `--list` are fzf menus for people. The
  drill-down of `./investigate` follows one object to the next by asking at
  every step, so an agent runs the detectives it would reach instead.
- **Check which cluster is current** with `kubectl config current-context`
  before running anything. Kubernetes books act on the current context, and
  `env.edn` picks its environment (ceph host, VPN interface) by that context.

A read-only run writes a reproducer script of the CLI calls it made and
copies it to the clipboard when a clipboard tool is installed, so running
books overwrites the user's clipboard. A change book, even one that posts
nothing to the changelog (a netshoot debug pod), and any run that reached a
change write none. Every run also leaves an audit trail
under `recordings/<book>/<timestamp>/` with its inputs, selections, commands,
and outcome; the path is printed after the final banner.

## Reading findings

```json
{"findings": [{"severity": "critical", "component": "...", "summary": "...",
               "hint": "...", "parent": "...", "since": "..."}],
 "checked": [{"name": "...", "description": "...", "capabilities": [...], "unread": true}],
 "summary": {"critical": 1, "warning": 0, "info": 0}}
```

- `severity` is `critical`, `warning` or `info`. Start with critical.
- `hint` is the book's suggestion for what to do next.
- `parent` marks a finding that follows from another at least as severe,
  and names that finding's object as `Kind namespace/name`: a pod finding
  under its workload or node, a finding under a cause on its own object (a
  missing reference, or a controller whose pods the API server refuses), and
  a Deployment's replica or rollout finding under the `pod-creation` refusal
  of its current ReplicaSet. Fix the parent first.
- `since` is when the problem began, where the API records it.
- An unschedulable pod's finding also carries `causes`, every reason the
  scheduler gave with its node count. Its summary names the first: what
  rejected the nodes the pod could run on. A cause marked `structural` only
  counts nodes the pod's own nodeSelector or node affinity excludes, such as
  the other nodes of a DaemonSet pod pinned to one node, and comes last.
- `checked` lists every detective that ran. An entry with `unread` read
  nothing, for example because the cluster does not serve that CRD or RBAC
  refused the read, or because the scope it judges selected no subjects, or
  because one of the reads it joins did not answer, so an empty finding list
  from it proves nothing; its only findings are ones naming a read that did
  not answer. Say so when you report a clean result.
- A book that judges against values from its options or env.edn names a
  missing one on stderr. The fabric detective without gateway VTEPs marks
  `evpn-gateway-only-mac` `unread`, adds an `evpn-inputs` warning naming it,
  and lists where each value came from under `inputs` (`option`, `env.edn`
  or `missing`). env.edn picks its environment by kubectl context, so a
  shell without one supplies nothing from it; pass every value as a flag
  there.
- `nothing-checked: true` means no detective read anything. The book exits 1
  and the run is not a clean result; report it as unchecked, then look for
  the scope or capability that left every detective without evidence.
- `crashed: [..]` names the detectives whose check threw. Each has one
  `critical` finding, `detective failed to run - investigate its input`,
  with the error as its `hint`. The book exits 1 whatever else it found,
  because a check that could not judge its evidence has not passed. A
  finding whose severity is not `critical`, `warning` or `info` counts as
  `critical`, and its hint names the value the detective returned.
- An evidence read that does not answer within 300 s counts as not read,
  so the detectives that need it are `unread`, and stderr names the read.

## What gumshoe covers

The detectives compose from one domain to the whole cluster:

| Book | Looks at |
| --- | --- |
| `runbooks/detectives/cluster.clj` | every registered detective, except posture, the checks that exec into pods, and the mail and fabric checks, whose evidence only their own books collect; takes `--hpa-grace-minutes` like `workloads.clj` |
| `runbooks/detectives/workloads.clj` | controllers, jobs, pods, Services, their storage; an Ingress backend Service that is missing is critical, and info when ingress-nginx redirects every request of that Ingress (`permanent-redirect`, `temporal-redirect`, or a top-level catch-all redirecting `rewrite` or `return 30x` in `configuration-snippet`); an HPA whose AbleToScale or ScalingActive condition is False is critical once it has held for `--hpa-grace-minutes` (default 10, read from the condition's `lastTransitionTime` at collection time) and a warning naming its age before that, while one whose age cannot be read stays critical |
| `runbooks/detectives/pods.clj` | restart loops, image pulls, OOM kills, Pending and Failed pods |
| `runbooks/detectives/nodes.clj` | readiness, pressure, cordoned nodes |
| `runbooks/detectives/platform.clj` | control plane, nodes, calico, CSI storage |
| `runbooks/detectives/storage.clj`, `csi.clj` | pending and unused claims, attachments, orphaned volumes |
| `runbooks/detectives/events.clj` | Warning events of the last hour |
| `runbooks/detectives/upgrade.clj` | removed APIs, webhooks, version skew, drain blockers; an admission webhook whose Service has no ready endpoint is critical when it fails closed (`webhook-backends`), and the check is unread when either webhook configuration read, the Service read or the EndpointSlice read did not answer, or no webhook configuration exists |
| `runbooks/detectives/apiversions.clj` | CRD discovery drift |
| `runbooks/detectives/security.clj`, `rbac.clj` | RBAC, pod security, network segmentation |
| `runbooks/detectives/posture.clj` | probes, image tags, replicas, PDBs, deprecated APIs, template drift |
| `runbooks/detectives/databases.clj` | CloudNativePG and db-operator; a CNPG Pooler declaring instances whose Service resolves to no ready EndpointSlice address is critical (`cnpg-pooler-endpoints`, component the Pooler), and the check is unread when the Pooler, Service or EndpointSlice read did not answer; it never execs into a pod |
| `tools/cnpg/runbooks/scan.clj` | the databases scope plus the opt-in `cnpg-connections`: a CNPG cluster whose client backends reach `max_connections` less the superuser-reserved and reserved slots is critical (component the Cluster), naming the holders split into Running pods, addresses no Running pod holds and the local socket. It reads `pg_stat_activity` with one constant read-only `psql` statement through `kubectl exec` into the primary's `postgres` container, so this book alone needs `create` on `pods/exec` and declares it as a prerequisite; a cluster it could not read is a warning naming why (exec denied or unanswered, no primary, answered from recovery outside a replica cluster), and the check is unread when no cluster was read or the Cluster, pod or connection read did not answer |
| `runbooks/detectives/observability.clj` | Prometheus, Alertmanager, ThanosRuler |
| `runbooks/detectives/dns.clj` | nameservers, SOA replication, answer consistency |
| `runbooks/detectives/mail.clj` | MX, SPF, DKIM, DMARC, rDNS, live SMTP/POP3/IMAP with TLS; without `--host` the services and rDNS checks are unread and no port is probed, without `--domain` the record checks are, and `mail-dkim` is unread when no selector answered |
| `runbooks/detectives/fabric.clj` | EVPN-VXLAN MAC ownership, over SSH; each `evpn-fdb-conflict` finding carries `local-uptime`, `bgp-route` (`present`/`absent`/`unread`) and `classification` (`live-misadvertisement`/`orphan`/`unknown`); takes `--hosts`, `--vni`, `--gateway-vteps` and `--guest-mac-prefixes`, each overriding env.edn's `:evpn` value; when no host answered, the checks over the MAC and fdb tables are unread |

Posture findings describe risks that may be deliberate choices, so they run
only when posture is asked for. A check that execs into a pod registers into
the opt-in `:exec` scope, so neither its scope's book nor the whole-cluster
scan runs it; the one book that adds it with `:detectives` beside its `:scope`
declares `:kubectl-can-exec ["pods"]`. The `:mail` and `:fabric` scopes are
opt-in as well: their evidence comes from dig, openssl and SSH in their own
books, and a cluster scan has no collector for it.

Each tool package under `tools/<name>/` brings a `scan.clj` for its system,
registers its detectives into the scopes above, and adds runbooks for its
common actions. In the cluster they cover Argo CD, calico (including Whisker
flow logs), cert-manager, Cluster API, CloudNativePG, Crossplane, db-operator,
external-dns, Flagger, Flux, Gateway API, JaaS, Karpenter, Kueue, Loki,
Prometheus, StageSet, Strimzi, Thanos, Traefik, and Velero. Outside it they
cover ceph and OpenNebula over SSH, Matrix homeservers, restic repositories,
the nginx/php-fpm upload limit chain of an app (PHP's limits come from
php-fpm itself, `php-fpm -i` with each pool's overrides from `php-fpm -tt`,
and a warning names limits read only from the CLI's `php -i`; when nginx or
PHP did not answer, `upload-limits` warns that the effective limit is
unknown and judges only the side that answered), and host facts from
node_exporter: `tools/node-exporter/runbooks/facts.clj` reads every host's
CPU count, memory, kernel, OS and exporter health through PromQL and compares
the reporting hosts with a file of declared names (`--declared`), with the
rows as `hosts` in its JSON report. A scope covers a tool only when
its plugin namespace is listed under `:plugins` in `env.edn`. The focused
`scan.clj` works without that.

The generic runbooks under `runbooks/` act on nodes (cordon, drain, taints,
apiserver restart) and workloads (scale, restart, pause, resume, rollback).
Others roll back Helm releases, drive CronJobs, remove finalizers, delete
objects and volumes, renew certificates, recreate ACME orders, query etcd,
silence alerts, export Grafana dashboards, and open netshoot debug sessions. `./run --list` is the complete
and current list.

## Preparing a change

1. Find the book and read `--help`. For a playbook or a mutation book it
   lists what the book picks, each step in order, where it asks for
   confirmation and pauses, what it verifies, and what it needs.
2. Run it with every selecting flag and `--dry-run`. It prints the
   confirmation it would ask and each command it would run (`would run:
   kubectl cordon worker-3`), and changes nothing: no prompt, no announcement,
   no post-check. It still checks each step's precondition. A banner naming
   steps whose precondition does not hold, with exit code 1, means the real
   run would stop there, unless an earlier step of the same playbook creates
   what that step needs. A banner ending `not rehearsed: <step>`, also with
   exit code 1, names steps whose work is a legacy `:execute!` function:
   nothing of them ran and the rehearsal cannot show what they would do
   (ceph `upgrade`, db-operator `connect`, `admin_connect` and `restore`,
   prometheus `delete_metric`); the steps after them are still shown. A `DRY RUN stopped by an unexpected error` banner
   means the rehearsal itself broke and the plans after the error were never
   shown, and a `DRY RUN failed` banner means it stopped before showing its
   plan, for example because a read did not answer; read the error before
   relying on the plan. A book that changes something without posting to
   the changelog (etcd `check_performance`, netshoot `run`) is bannered as
   a change too. A playbook passes its
   pauses under `--dry-run`; a
   real run without a terminal stops at the first pause with a `PAUSED`
   banner naming the `--from-segment` that continues after the human acted.
   A change whose `--help` says `skips it, asking nothing, when …` is
   already done when its post-checks hold: the run, rehearsal included,
   prints `already holds: <check>` and `skipped, already done: <step>`
   instead of a plan. A playbook stopped part-way can therefore be run
   again from the top: its confirmation marks the steps that took effect
   `(already done)`, each is judged again when reached, and a playbook with
   nothing left prints `every change of this playbook is already done -
   nothing to confirm` and asks nothing. A rehearsal judges no change done
   after one it would run or a pause it would wait at.
3. Show the human the plan and the command without `--dry-run`, written so
   they can run it in their own session, for example
   `! bb run runbooks/kubernetes/nodes/cordon.clj --node worker-3`.
4. After they run it, check the outcome with the matching detective. The
   final banner says what the run did: `DONE - applied and verified` is the
   only one that promises its post-checks held (with `; skipped as already
   done: …` naming the steps it found done), `DONE - applied, NOT confirmed
   (best effort): <check>` exits 0 but names a `:soft?` post-check that ran
   out its time without holding, or an outcome nothing records (cnpg
   `reload`: CloudNativePG writes nothing when it acts on one), so confirm
   that by hand, `DONE - already done, nothing
   was changed - skipped: …` changed nothing because every change was
   already done, and the run's `recordings/…/meta.edn` lists those steps
   under `:skipped`; `DONE - applied; this book has
   no post-checks` verified nothing, and a `FAILED after N effects` banner
   names where it stopped while the effects before it stay applied. Notes
   in a plan only print and are never counted as applied effects. A banner
   naming `a step whose effects are unknown` means a legacy `:execute!`
   step ran before the failure, so something may have changed. Lines
   under a failed banner headed `Recovery:` are the stopped step's guidance:
   what the half-applied state means and how to bring the system back. A
   mutation book without its own `:recovery` that fails after applying an
   effect prints generic guidance there instead, starting `This change
   declares no recovery guidance`: which effects stayed applied, where it
   stopped or that its post-check did not hold, and where to look (those
   messages, the changelog entry, the object on its cluster). In a
   playbook the stopped step's guidance is followed by the playbook's own,
   which names what the run still leaves parked (a HelmRelease suspended, a
   node cordoned), or, when the playbook declares none, by the effects its
   earlier steps applied. Read them to the human before anything else is
   run.

A book whose change cannot be expressed as a plan says under `--dry-run` that
it does not support a dry run, runs nothing and exits 1: a confirmation
reached under `--dry-run` is refused, never answered. The etcd performance
check, the setup wizard and `share` print what they would run or write
instead and exit 0. Interactive sessions
(`netshoot`, `psql` connections, `upterm` sharing) are for the human
entirely. `runbooks/collaborate/share.clj` refuses to share a terminal unless
`--with <github-user>` names who may join or the human passes `--open`; never
pass `--open` on their behalf, since an open session lets anyone holding the
connect string type into a shell with their credentials.

## Prerequisites and configuration

Every book checks its tools, their versions, connectivity, secrets, the
directories its options name, and permissions before it acts, and prints a checklist naming what is missing.
With breto, `eval "$(breto env)"` puts every tool a book needs on `PATH`.

`env.edn` (optional, gitignored, built by `bb run runbooks/setup/init.clj`)
holds per-host settings: environments selected by kubectl context, ceph mgr
hosts, VPN interfaces, the secrets provider (`:gopass`, `:pass`, `:passage`,
`:pasejo` or a `:command`), announcers, plugins, and the theme. A missing
file means no configuration and never an error. The first env.edn that exists
in the search order (`GUMSHOE_ENV`, `./env.edn`,
`$XDG_CONFIG_HOME/gumshoe/env.edn`, `~/.config/gumshoe/env.edn`) is the one
read, even when it is broken: a file that does not read as an EDN map is
named on stderr with the parse error, nothing of it applies, and the
"suitable Kubernetes cluster" prerequisite refuses every cluster until it is
fixed. A part of the wrong
shape (`:environments` or `:defaults` that is not a map, a `:select` that is
not a map, `:plugins` that is neither a namespace nor a list of them) is ignored with a warning on
stderr naming it, and the rest of the file still applies. A part that could
name a cluster fails closed: a `:clusters` that is not a cluster name string
or a list of them, or a dropped `:environments`, environment or `:select`,
makes the "suitable Kubernetes cluster" prerequisite refuse every cluster the
readable part does not name. `:announce` takes one announcer map with a keyword
`:type` or a list of them; any other shape refuses a change
before anything runs. The setup wizard refuses to rewrite an `env.edn` that
does not read whole, names the sections to fix, and writes nothing. Secrets come from the
password manager at runtime and never appear in process arguments.

## Extending gumshoe

A team keeps its own books in a **casebook**, a repository whose `bb.edn`
depends on a pinned gumshoe release and puts its own `runbooks`, `playbooks`
and `firebooks` on `:paths`; `casebook-example/` is a template. Books are
mostly data:

- a detective book is `(detective/book {:description … :detectives …})`.
  Evidence from outside the cluster's resources comes from `:collect (fn
  [opts] evidence)`, returning nil when it can not collect after printing
  why; `:around (fn [opts run] …)` wraps it in a port-forward or connection
  check and calls `(run opts)`, and `:extra (fn [evidence] map)` adds rows to
  the json/edn report. Checks whose `:requires` all came back nil are marked
  unread, so a collection that read nothing fails with "nothing was
  checked". `:requires` must name the evidence keys as the collection
  returns them, keywords or strings. A check that judges a chosen set of
  subjects names their keys as `:judges` and is unread when every one is
  empty; a check that joins several reads (a Pooler to its Service and the
  Service's EndpointSlices) names them as `:joins` and is unread when any
  one of them is nil, so a read that did not answer never reads as an empty
  side of the join. The engine keeps no finding of an unread check except
  those its `:detect` marks `:reports-unread true` - findings that say which
  read did not answer and why - and a crash; the marker is removed from the
  report. Guard `:detect` anyway, returning nothing unless
  `(gumshoe.evidence/joined? evidence reads)` holds, so it is correct when
  called directly; name the reads once in a var and use it for `:joins`,
  `:requires` and that guard.
  Join every read whose absence would turn an object into a missing one or
  leave part of what the check covers unjudged (a kind of binding, a kind
  of owner), and leave out a read judged only when it answered, one that
  only adds to a hint, or one that a cluster may not serve.
  Reads judged only when they answered, of which any one is enough (the
  ServiceAccount, Secret, Service and namespace reads a pod template is
  judged against), go in `:consults`: the check is unread when every one is
  nil, an empty list still counts, and its `:detect` guards with
  `(gumshoe.evidence/consulted? evidence reads)`.
  A collection reading a command's output out of a container uses
  `kubectl/exec-answer`, nil unless the command exited 0 (a PostgreSQL
  server's connections come from `gumshoe.postgres/connection-reading`,
  which runs the constant `connection-sql` read-only and parses it with
  `parse-connections`, nil unless the rows add up to the total); `exec-stdout`
  returns whatever reached stdout, so a failed command reads as an empty
  answer. Keep a value nobody read nil in the evidence rather than
  substituting the program's default for it, and apply the default only
  when the program answered without setting the value.
  `(detective/check-step {:title … :detectives … :blocking? false})` is the
  playbook step for the same;
- a book derives the options its select reads instead of writing them:
  `(def selection {:mode … :label … :flag …})` and `:options
  (select/options selection {…})`, where the map adds the book's own
  options and rewords a select flag's `:desc` without touching its alias or
  coercion. Each select flag gets its short alias from
  `gumshoe.flags/aliases`, so it is the same in every book and steps from
  different tools join in one playbook; a new select flag gets its entry
  there, and `select/options` refuses two options sharing an alias when the
  book loads. A bespoke book declares its select once without
  `:candidates` and adds them at the pick, `(select/pick (assoc
  node-select :candidates …) opts context)`;
- `--help` lists options in the order of the option map's keys: a select's
  derived flags first, then the book's own, then the standard flags. A map
  literal of more than eight entries is a hash map and loses that order, so
  a book with more options writes them as `(flags/ordered :url {…} :flavor
  {…} …)` and joins option maps with `flags/merge-ordered`, never a plain
  `merge`; conformance notes a book whose options lost their order
  (`:option-order`), once per book file however many of the option maps
  it declares or merges lost it, naming the size of each;
- a uniform change is `(mutation/book {:select … :confirm … :effect …
  :verify …})`. Conformance requires each spec to carry a readable
  `:confirm {:action …}`, an explicit `:destructive?` true or false,
  `:prerequisites`, an `:effect`, and a `:verify` or, for a change with
  nothing to check afterwards, `:no-verify "why"`. A destructive spec also
  needs a guard judged again after the confirmation: a `:refusal` over the
  state it rests on, or a `:subject`/`:subjects` the engine reads again and
  pins, or `:no-guard "why"`; give it both where you can, so a target that
  was deleted, replaced or left the state it was offered in is refused with
  a reason naming it. The engine judges `:refusal` (and a `:precondition`,
  folded into it) twice with the same `(fn [ctx])`: before the confirmation
  over the derived context, with `:recheck`'s map merged over it, so the
  operator is never asked to approve a change that will be refused, and
  again after it over fresh reads; a subject or pinned object that is not
  there is refused before the confirmation too. Write the refusal as a
  pure judgement of `ctx` (it may read, never write); what it prints shows
  only when it refuses before the confirmation, and after the confirmation
  as before. A refusal that must tell the two apart reads
  `mutation/*phase*` (`:derive` before, `:judge` after). A rehearsal and a
  change a playbook's confirmation covered judge once. Its effect plan (`[[:kubectl "cordon" node]]`) runs,
  dry-runs, or is collected in a test. An `:effect` returning one effect,
  such as a bare `(effect/note …)`, runs as a plan of one; anything else
  that is not a plan, a keyword, a map or a plan holding a non-effect, is
  refused with an error naming the step and the fix, `effect/plan`, before
  the step asks for confirmation or runs any of it. A plan holding an op no
  interpreter knows (a typo, or a custom effect whose plugin did not load)
  is refused the same way, naming the op, under `--dry-run` too. In a
  playbook the steps before it have already run. Its `:derive` reads the cluster
  under `kubectl/*strict-reads*`, as `:select` does, so a read there that
  did not answer stops the book with "could not read what the change rests
  on" before the confirmation, and the `:precondition` never judges an
  object nobody could read (a `NotFound` answer still derives nil). The
  `:precondition` reads strictly too: a read in it that did not answer
  refuses the change with "refusing: could not read what the precondition
  rests on". Every post-check in `:verify` runs under the same binding, so a
  read that did not answer counts as not yet verified: a check asserting an
  absence (a taint gone, a resource resumed) waits for an answer and fails
  at its timeout instead of holding on the nil a failed read returns, while
  a `NotFound` answer still reads nil and confirms a deletion. A `:soft?`
  check still turns that timeout into a warning, and the run's banner then
  says the change was not confirmed instead of verified. An outcome the
  system records nowhere is declared with `(verify/unobservable "<what>"
  "<why>")` beside the checks that can be observed: a `:soft?` check that
  never holds, so the banner says NOT confirmed instead of a check passing
  on the change's own write. Reads over SSH, among them
  every `gumshoe.ceph` read, ignore the binding, so a check built on them
  still has to tell a failed read from an empty one itself
  (`ceph/ceph-json-result` returns `{:ok? :doc}` for that, and
  `ceph/unarchived-crashes` shows the shape a book reads: `{:crashes [..]}`
  when ceph answered, `{:unread "why"}` when it did not, so a book refuses
  instead of reporting "nothing to do"). A hand-written book that reports on
  every object of a type reads it with `(kubectl/read-all context type)` in
  the same shape, `{:doc ..}` or `{:unread "why"}`, and fails the run with
  "could not read the <type> of the cluster - <why>" instead of printing an
  all-clear;
- a post-check fails closed. A check about the state of an object (resumed,
  schedulable, unfenced, Ready) judges it through
  `(verify/object-holds? object pred)`, which is false for nil, because a
  `NotFound` answer reads nil and an object that is gone is in no state.
  Only a check whose success is an absence (deleted, gone, nothing left)
  holds on `NotFound`; it confirms the absence with
  `kubectl/confirmed-gone?`, which needs an answer whatever the binding, and
  declares `:verifies-absence? true`. `(verify/fail-closed-findings checks)`
  runs checks once against a cluster that does not answer and once against
  one holding nothing, with `gumshoe.shell/execute` stubbed, and names each
  check that holds where it must not; a casebook can call it from its own
  tests, as gumshoe's mutation-book harness does for every book;
- a post-check never takes the change's own write for its result: a claim
  requesting the new size proves only that the patch landed. It waits for
  what the cluster reports in turn (a capacity, a status, a condition), and
  judges conditions and events against the object as read right before the
  effect, so evidence the object carried already decides nothing. A check
  can name a state it can no longer reach with `:abort (fn [] -> reason)`,
  ending the wait at once, and say how it last stood with `:detail (fn [] ->
  text)`, printed under a timeout. `storage/kept-claim-verdict` is the
  pattern: a pure verdict over the claim now, the events and the baseline
  the change's `:refusal` read (`storage/kept-claims-baseline`);
- a change that acts on one object declares it as `:subject {:type
  "cronjobs"}`, with `:parse` for a pick of another shape (`gumshoe.pick`
  has `namespaced`, `cluster-scoped`, `(kinded kind->type)` and `(typed
  types)`; a parser may name the `:type` itself, and whatever else it
  returns, such as a finalizer or an instance, stays on the ref), `:invalid` for the reason
  a pick that does not parse is refused, and `:facts` for what outside the
  spec the change rests on. The engine reads the object strictly before
  `:derive`, hands every callback `:ref` and `:object`, pins the ref, and
  reads the object again before `:refusal`, a `(fn [ctx] reason-or-nil)`
  whose reason it prints as "refusing: …"; a post-check reads it with
  `(mutation/read-subject ctx)`. So the book writes no fetch, pin or refusal
  printer of its own, and keeps `:precondition` for any other check
  (`tools/argocd/runbooks/refresh.clj` is the short example). The
  confirmation items, the plan and the post-checks are built before the
  refusal runs, but never for a pick that does not parse: `mutation/parse-pick`
  judges every pick first, in `run!`, `run-on!` and each playbook step, and
  refuses one that does not parse before anything is read or confirmed. With
  a `:subject` it is refused with `:invalid`; without one a `:namespaced`
  pick must be `<namespace>/<name>`, a `:many` pick a non-empty collection of
  non-blank strings and any other pick a non-blank string, so a book that
  reads `:target` itself never sees nil or blank text.
  The pin compares uid and generation. An object the API server gives no
  generation is compared by resourceVersion, so any write to it, status
  included, refuses the change; for a type that splits spec from status
  but has no generation, such as a Node, whose status the kubelet writes
  every few seconds, add `:compare :spec` to the subject (or to a
  `:subjects`/`:pin` ref) and the pin compares uid and spec instead,
  refusing as "its spec changed after the confirmation: taints"
  (`gumshoe.steps.nodes/node-subject`; the values are in
  `gumshoe.change/comparisons`, and an unknown one is refused). Metadata is
  outside `:spec`, so judge a label the change needs in `:refusal`, which
  sees the object read again, or name it with `:facts`.
  A change whose subject is no single object, such as a Helm release kept
  as many Secrets, uses `:refusal` alone (`runbooks/kubernetes/helm/rollback.clj`);
- a change that acts on several objects at once - an adoption stamping a
  volume claim, a database and the host's routes - declares them as
  `:subjects (fn [ctx] -> refs)`, each ref `{:type :name :namespace?}` with
  optional `:facts` and `:optional? true` for an object that may be absent.
  The function sees the pick as `:target` (and `:ref`/`:object` with a
  `:subject`), and a `:many` select may declare it. The engine reads every
  object strictly before `:derive`, hands the callbacks `:refs` and
  `:objects` in the declared order (nil for an absent optional one), reads
  them all again before `:refusal` and `:precondition`, and pins each, so
  the change is refused naming the object that moved, as "refusing:
  databases.kinda.rocks moodle/moodle: its spec changed after the
  confirmation (generation 1, now 2)". A required object that did not exist
  at the confirmation refuses, and so does an optional one that appeared
  after it. It composes with `:subject` and `:pin`; a post-check reads the
  set with `(mutation/read-subjects context refs)`;
- a change splits what it derives by when it is true. `:reads {name (fn
  [ctx] -> value)}` are live state the change rests on and no `:subject` or
  `:subjects` covers (which storage locations are available, whether a
  listener drifted): each is read strictly before the confirmation, after
  the subjects, all at once, bound in `ctx` under its name, and, when the
  spec has a `:refusal`, read again after the confirmation so the refusal
  judges the fresh value under the same name. `:items`, `:announce`,
  `:effect` and `:verify` keep the value the confirmation showed; a
  post-check reads the live one with `(mutation/read-again ctx :name)`.
  `:values {name (fn [ctx] -> value)}` are fixed by the run - a timestamp, a
  generated backup name, an operator's choice, a snapshot the confirmation
  lists - and computed once, after the reads, never again; a value sees the
  subjects and the reads, never another value. In a playbook the first
  change keeps the values its judgement before the playbook's confirmation
  computed. Classify a read that only feeds the confirmation items as a
  value, so it is not read again for nothing
  (`tools/velero/libraries/gumshoe/velero/action_books.clj` has both). A
  name is a keyword, never one the engine binds itself (`:context`,
  `:target`, `:ref`, `:object`, `:refs`, `:objects`, …), and never in both
  maps;
- a change that is idempotent and whose end state is cheap to read
  declares `:done true`: when every post-check its `:verify` returns already
  holds, each attempted once with strict reads before the confirmation, the
  run skips the change, asks nothing, records it under `:skipped` and
  returns true. `:refusal` is judged only for a change that is not done, so
  "it is suspended already" becomes a skip. `:done (fn [ctx] -> bool)` adds
  a judgement over the derived context to the post-checks and never
  replaces them; a destructive change with a `:done` function needs a
  `:verify`, and `:satisfied?` is an alias. A read that did not answer is
  never done: the change then runs as one that is not done, after saying it
  could not tell. Toggle pairs declare it for both sides (`:done false` in
  the declaration opts out), and so do the node steps, `workloads/scale`
  and the cnpg fence books. Write `:done` only where a post-check that holds
  means the change has nothing left to do: a restart's "rolled out" holds
  before the restart too. In a playbook a change judged done alters
  nothing, so the change after it is judged before the confirmation as the
  first one would be;
- `:derive (fn [ctx] -> map)` and `:recheck (fn [ctx] -> map)` are the
  older spelling and keep working; a spec writes them or named `:reads` and
  `:values`, never both, since a map they return could hide a named entry.
  A change whose judgement rests on reads no `:subject` or `:subjects`
  covers reads them
  again after the confirmation with `:recheck (fn [ctx] -> map)`. `:derive`
  runs once, before the confirmation, so without it `:refusal` and
  `:precondition` judge the state the operator was shown, not the state the
  change meets. `:recheck` runs right before `:refusal`, after the subject
  and subjects are read again, receives that context (the derived values
  and the fresh `:object`/`:objects`), and its map is merged over it for
  `:refusal` and `:precondition` only; `:items`, `:announce`, `:effect` and
  `:verify` keep the derived values the plan was built from. Where every
  derived value is a live read, name one function as both `:derive` and
  `:recheck`; to compare the confirmed value with the current one, return
  the current one under its own key. Its reads are strict, so a read that
  did not answer refuses before any judgement, and a spec with `:recheck`
  needs a `:refusal` or `:precondition` (else it throws). The drift checks
  of `:subject`, `:subjects` and `:pin` still follow the judgement. Reads
  before the confirmation run with `mutation/*phase*` bound to `:derive`,
  reads after it to `:judge`;
- each of those two phases binds a fresh `kubectl/*read-cache*`, so an
  object read by the subject read and again by `:derive`, or by `:refusal`
  and again by a drift check, is fetched once per phase. No read after the
  confirmation is served from a read before it, and post-checks run outside
  both phases, so every poll reads the cluster. Subjects are read
  concurrently, and the `:judge` phase fetches the objects it is about to
  judge at once. A `:derive` or `:recheck` that makes several independent
  reads issues them together with `(gumshoe.concurrent/values {:a #(read-a
  context) :b #(read-b context)})`, which returns `{:a … :b …}`; its
  futures carry the phase, the cache and the strict binding along, and it
  rethrows a strict read's `::kubectl/unread` unwrapped;
- a change that acts on something other than the cluster of the kube context
  - an SSH host, a storage cluster - names it with `:connect (fn [opts] …)`,
  which checks the connection and returns `{:context … :cluster …
  :announce-as …}` or nil. The candidate query, every callback and the
  effect receive that `:context` (an ssh connection for `gumshoe.ceph`), the
  confirmation names `:cluster`, and the changelog names `:announce-as`,
  defaulting to `:cluster`; nil stops the book before anything is listed.
  `gumshoe.ceph/connect` is the ceph one (`tools/ceph/runbooks/osd_in.clj`).
  `:subject`, `:subjects` and `:pin` read with kubectl, so a spec with
  `:connect` takes none of them and re-checks its object in `:precondition`. A change nobody else
  needs to hear of, such as a netshoot debug session, sets `:announce?
  false` and runs as a read-only book does (`runbooks/kubernetes/netshoot/node.clj`);
- every spec shape above is an alias of one canonical change map, and
  `gumshoe.change/normalize` maps a spec onto it before the engine reads it
  (its docstring lists the map and the aliases): `:subject`, a `:subjects`
  function and `:pin` become entries of a `:subjects` vector, each naming
  `:from :pick`, `:target` or `:reads`; `:precondition` becomes part of
  `:refusal`, refusing in flow's generic words; a top-level `:empty-message`
  moves into `:select` (so a select's own `:empty-message` now shows for a
  mutation book too); `:announce? false` becomes `:announce false`; `:derive`
  and `:recheck` become the reserved entries `change/derive-entry` of
  `:values` and `change/recheck-entry` of `:reads`. Old keys
  keep working. A spec naming one thing two ways - a canonical `:subjects`
  beside `:subject` or `:pin`, two different empty messages, an `:announce`
  message beside `:announce? false` - or one the engine cannot run (a
  `:recheck` with no `:refusal` or `:precondition`, a `:subject` on a `:many`
  select, `:connect` with any subject, `:derive` or `:recheck` beside named
  `:reads`/`:values`, a name both maps bind or one the engine binds) is
  refused when the book loads, and
  the conformance gate reports it as `is refused: …`;
- a change to one object can be written as plain EDN and compiled with
  `(gumshoe.dsl/compile data)` into that canonical map (README "Changes
  written as data"; `tests/edn-books/runbooks/kubernetes/cronjobs/suspend.edn`
  is a whole one). The format is experimental and may change in any
  release: a change declares `:gumshoe/dsl :v0-experimental`, and anything
  else is refused. Saved as an `.edn` file below a book root, it is a book
  that `bb run` lists and runs and `bb verify` judges like a Clojure book,
  so keep data files outside book roots. The file is read as plain EDN
  only (no tagged literals, at most 64 KiB and 32 levels deep), and
  functions a casebook registers load from a plugin named in `env.edn`.
  Prefer a Clojure book for anything an operator relies on until the
  vocabulary is reviewed. Declare the target with its `:type` (or a registered `:parse`;
  add `:compare :spec` for a target such as `"nodes"` that has no generation), the
  `:state` the change brings about, `:refuse` entries, `:confirm` and
  `[:kubectl :patch|:scale|:annotate|:delete …]` effects; `compile` derives
  the pin, the fail-closed post-check, the skip as done and, for a delete,
  the typed confirmation, and requires `:recovery` for a destructive change.
  A delete beside `:confirm {:destructive? false}` is refused in the words
  `normalize` uses, unless the change opts out with `:destructive`; a
  compiled delete carries the destructive mark, an opted-out one none.
  Opting out of any of these takes `:opt-out {<safeguard> "why"}`; strict
  reads have no opt-out. Logic beyond the vocabulary is a library function
  registered with `gumshoe.dsl.registry/register!` under a keyword and its
  kind, and called as `[:call :ns/name arg …]`, never inline code. An
  unknown key, word, name or argument count is refused when it compiles,
  with a message naming its path; read that path rather than guessing;
- a pair of books that set one boolean state and set it back - suspend and
  resume, pause and resume - is one `(toggle/pair {…})` declaration from
  `gumshoe.toggle`, returning `{:on <spec> :off <spec>}`: the shared spec
  with its `:subject`, then `:refusal`, `:items`, `:effect` (or a merge
  `:patch`) and `:applied?` as functions of the object or context and the
  wanted state, and each side's words under `:on` and `:off`. The builder
  writes the candidates (by default the objects `:refusal` lets through),
  the fallback items, the announcement and one post-check; each book is
  `(mutation/book (merge steps/suspend-cronjob {:description "…"}))`,
  where the library names each side as a var, `suspend-cronjob` being
  `(:on cronjob-suspending)`, because conformance reads `(merge <var> {…})`
  (`gumshoe.steps.workloads` holds the CronJob and Deployment pairs);
- a playbook is `(playbook/book {:select … :confirm … :steps […]})`, whose
  steps are mutation specs, checks and pauses; it derives its prerequisites
  and options from the steps and asks for one confirmation. Before that
  confirmation it judges every change's pick and the refusal of the first
  change unless a waiting pause comes before it (`playbook/prejudgeable`);
  a change after another change or after a waiting pause is judged only
  when reached, because its evidence is what the run is about to change. A
  driver that confirms a segment itself calls `(playbook/prejudge! steps
  ctx waits-first?)` before its confirmation;
- a playbook or dispatcher whose select leaves out what is already done or
  not eligible declares, beside its `:select`, a `:subject` that reads a
  named target and a `:done` that judges the whole procedure took effect
  on it, so `--namespace X` reports `no Namespace X - not found`, `present
  but not eligible`, or `skipped, already done` (exit 0) instead of the
  generic `not found among the candidates`:

  ```clojure
  (playbook/book
   {:select {:mode :one :label "Namespace" :flag :namespace
             :candidates (fn [context] (unadopted-namespaces context))}
    :subject {:type "namespaces"}
    :done (fn [{:keys [object]}] (= "true" (get-in object [:metadata :labels :adopted])))
    :confirm {:action "adopt the namespace" :destructive? true}
    :steps […]})
  ```

  `:done` receives the context the subject read made (`:object`, `:ref`,
  `:target`, `:opts`); `playbook/pick-spec` builds what `mutation/pick!`
  judges. A playbook without a `:subject` reads the target by the
  `:subject` of its first change step that has no `:select` of its own;
  only the playbook's `:done` can call a named target done;
- any step whose failure leaves a system parked or half-owned - a controller
  suspended, a finalizer stripped, an owner removed before its replacement
  exists - carries `:recovery`, a string or `(fn [ctx] …)` returning one,
  which the run prints under the `FAILED` banner when it stops at that step.
  A function receives the step's context with `:applied`, runs at the
  failure, and can read the cluster to say what is still suspended and give
  the commands that undo it; one that throws leaves the banner intact and
  says the guidance could not be rendered. A mutation book carries
  `:recovery` the same way. A mutation without `:recovery` that fails after
  applying an effect prints the engine's generic guidance
  (`mutation/fallback-guidance`: the effects that stayed applied and where to
  look) in its place, and a dry run prints none. Once a playbook run changed
  something, the playbook's own `:recovery` - its standing guidance for the
  state the steps leave, read at the failure - follows the stopped step's,
  so a caller's step added at a position keeps telling the operator what
  stays suspended; a playbook without one names the effects its earlier
  steps applied instead (`playbook/compose-recovery`, a pure function of the
  run's state). Give a playbook whose steps park something a standing
  `:recovery` that reads that state, and keep each step's own to what its
  failure means. `gumshoe.tools.flux-storage/recovery` (which of the
  HelmRelease and its owner stay suspended) and
  `gumshoe.steps.nodes/maintenance-recovery` (whether the node stays
  cordoned and the pods a drain still evicts) are the examples. A rollback
  step at the end never runs, because the run stops at the first failure;
- a firebook is `(firebook/book {:description … :drill "crash-loop" :fire
  "a crash-looping deployment" :manifests […] :expect {:detectives
  pods/detectives :fire? (fn [finding ctx] …) :timeout 180}})`. A
  kubernetes drill names only its manifests: the builder applies them
  inside the `fire-drill` namespace and deletes that namespace on
  `--extinguish`, both as effect plans behind a confirmation, so
  `--dry-run` shows either and changes nothing. After the ignite the
  burning check runs the `:expect` detectives over fresh evidence until
  one reports a finding `:fire?` accepts about an object of the drill
  namespace; after the extinguish the out check waits until none does.
  Evidence a detective read nothing from never passes either check, and
  the out check never passes on evidence whose required keys all came
  back empty (an fdb table with no rows, a listing with no items) unless
  the drill says why that shows the fire is out, as `:expect
  {:empty-is-out "why"}`; a kubernetes drill says so because its
  extinguish first confirms the namespace is gone. Every expected
  detective therefore declares `:requires`. Both stages are mutation
  specs (`firebook/stages`) run by `mutation/run-on!`, so a drill's
  `:reads` and `:refusal` guard both stages before and after the
  confirmation; a kubernetes drill refuses a `fire-drill` namespace that
  lacks gumshoe's `app.kubernetes.io/managed-by` label, and conformance
  judges each stage as it judges any change spec. Any other drill
  declares `:derive` (returning `:target`, `:system` and, for kubectl,
  `:context`), `:ignite`, `:extinguish` (with its `:recovery`) and an
  `:expect :collect` that throws `(firebook/unread msg)` when it could
  not read; `firebooks/fabric/stale_fdb_entry.clj` is the example;
- a dispatcher is `(dispatch/book {:select … :classify (fn [ctx target] key)
  :routes {key {:playbook …}}})`, one front door that runs the playbook a
  subject's kind routes to; its own `:subject` and `:done` judge a named
  target its select leaves out, as a playbook's do;
- a bespoke or read-only book picks its subject from a cluster read with
  `(select/pick {:mode … :label … :candidates (fn [context] names)
  :empty-message … :on-empty :fail} opts context)`, which lists the
  candidates under strict reads and returns the pick or nil after printing
  why: "could not list the … candidates", the `:empty-message` (as an error
  with `:on-empty :fail`), or "no … selected". A book that needs the objects
  behind the names reads them in a `delay` its `:candidates` derefs and
  derefs it again after the pick; `gumshoe.etcd/pod-select` and
  `etcdctl-action` are the example;
- a library playbook that takes a caller's steps does so at named
  positions, never by matching a title: it pairs each step with the position
  that follows it, `[[position step] …]`, and builds its steps with
  `(playbook/with-extra anchored extra {:of "the … steps" :refuse (fn
  [position step] why-or-nil)})`, which throws on an unknown position as the
  book loads. `gumshoe.tools.flux-storage` builds `(playbook-with
  {:after-volumes [step]})` this way from `positions` (`:after-suspend`,
  `:after-workloads`, `:after-volumes`, `:after-storage`, `:after-resume`),
  refusing a destructive step at `:after-resume`;
- a step a runbook and a playbook share lives in a library, and the runbook
  names its description beside it, `(mutation/book (merge nodes/cordon
  {:description "…"}))`, so `./run --list` reads it without loading the book;
- the step libraries build the acting half of common steps - `:items`,
  `:effect`, `:verify`, and the `:values` they need - which a step combines
  with its own title, confirmation, reads, values, `:refusal` and
  `:recovery` through `(gumshoe.change/compose own-keys (builder …))`:
  `(gumshoe.steps.objects/delete {:objects (fn [ctx] …) :none "…" :timeout
  …})` deletes `{:type :namespace :name}` objects without waiting and proves
  each gone with a read that answered. Its effect is marked with
  `gumshoe.change/destructive`, so the step asks for a typed yes whether
  the caller composes or merges it and leaves `:destructive?` out or says
  true. A caller saying `:destructive? false` beside a marked effect is
  refused when the spec is normalized or composed, in any order or
  grouping of the parts, with `the :effect gumshoe.steps.objects/delete
  destroys what it acts on, and the :confirm says :destructive? false, …`;
  drop the `false`. A mutation book is refused as it loads, a playbook
  before its confirmation, and conformance reports it as `is refused: …`.
  A builder whose effect destroys what it acts on marks its effect the same
  way, `(change/destructive "my.ns/builder" (fn [ctx] …))`, naming itself
  for that refusal, instead of returning a `:confirm` of its own, which a
  caller's `merge` would overwrite with its action; `gumshoe.steps.flux/suspend`,
  `resume` and `reconcile` take `{:object (fn [ctx] {:kind :type :namespace
  :name}) …}` for any kind in `flux/suspendable` or a ResourceSet, and
  `reconcile` waits for `status.lastHandledReconcileAt` to equal the token
  it stamped. `resume` and `reconcile` declare the token as the value
  `:timestamp`, bound to the shared `steps.flux/timestamp`; a step with its
  own token passes `:token :its-value-name` and the builder declares none;
- `gumshoe.change/compose` combines specs key by key, so a builder's
  fragment and a step's own keys never overwrite each other as `merge`
  does: `:reads`/`:values` merge by name (a name bound to two different
  functions is refused, the same function is one entry), every part's
  `:refusal` is judged in part order and the first reason refuses,
  `:verify` and `:items` concatenate, `:done` holds only when every
  part's does (a part without one is neutral), `:target` subjects
  concatenate and a ref two parts compute is refused, `:confirm`'s `:destructive?` and `:separately?` are OR-ed (a part's `:destructive? false` beside a marked effect is refused, even when another part's `true` absorbed it),
  `:prerequisites` and `:options` join, `:no-verify`/`:no-guard` reasons
  join and are refused beside a `:verify` or a guard, and every other key
  (`:effect`, `:select`, `:title`, `:recovery`, …) is single-valued, two
  different values refused. It accepts the aliases, skips nil parts, is
  associative, and leaves the whole-change checks (`:done true` without
  `:verify`, …) to `normalize` when the spec runs or its book loads. A
  plain `merge` keeps one side's `:values`, `:refusal` or `:verify` and
  drops the other's without a word, so a step built from a builder is
  composed, never merged;
- a bespoke book calls `runbook/execute!` with its own `:action`. One whose
  work before the confirmation goes beyond a select - a second pick, a plan
  it validates and refuses before asking - builds a mutation spec from what
  it found and runs it with `(mutation/run-on! spec target opts
  announcement-data)`, so the change still pins its `:subject`, dry-runs and
  records its `:recovery`. A plan whose recovery depends on where it stopped
  is built from steps `{:effect … :stage …}`, and its `:recovery` reads
  `(storage/progress steps applied)` to name what ran and where it stopped
  (`statefulsets/enlarge_volumes`, `prometheus/set_capacity`);
- status conditions are read through `gumshoe.conditions`, never by hand:
  `(conditions/condition o "Ready")`, `by-type`, `holds?` (True), `status?`
  (`(status? o "Ready" "False")` for an explicit failure), `ready?`,
  `first-false`, `stale?` (generation not yet observed), a tolerant
  `minutes-since` and `first-line`. A missing condition is neither True nor
  False, and the first of a repeated type counts. A post-check that waits for
  Ready is `(conditions/ready-check {:description … :timeout … :interval …
  :fetch #(kubectl/get-namespaced-resource …)})`;
- an option naming a checkout or another directory the book reads goes in
  `:prerequisites {:existing-paths [:fahrplan {:option :k8s-deployments
  :writable true}]}`: a keyword requires an existing directory, the map form
  also requires it writable. The value resolves against the working
  directory, with a leading `~` as home, and a missing path, a file or an
  unwritable directory is a failed line in the Prerequisites checklist naming
  the option, so the run stops before the action. An option left without a
  value passes with a note; mark it `:require` in `:options` when the book
  cannot run without it. A book that hands the value to a process resolves
  it with `(prerequisites/directory-path value)`, the path the check judged;
- a PromQL read goes through `gumshoe.promql`: `(promql/endpoint {:url …
  :flavor :thanos})` (or `:prometheus`, `:mimir` with `:tenant`), then
  `(promql/query! ep "…")`, which returns `{:ok true :series […]}` or `{:ok
  false :error …}` and never throws; Thanos is asked to refuse partial
  answers, so a missing store fails the query instead of shrinking a count;
- a book that collects rows passes them to the machine formats with
  `(detective/report! detectives findings output {:hosts rows})`;
- a cluster read no `gumshoe.kubectl` function wraps goes through
  `(kubectl/get-json context "namespaces" "--selector=…")`, nil when the read
  fails, or `get-json-result`, whose `:ok?` tells a failed read from an empty
  one before a gate trusts an absence. Both are strict readers: while a
  `:select` lists candidates, or a subject read, `:derive`, `:refusal`,
  `:precondition` or post-check
  runs, `kubectl/*strict-reads*` is bound and a read
  that did not answer throws `::kubectl/unread`, which a `:select` reports as
  "could not list the … candidates" instead of "nothing to do". A
  `NotFound` answer is not a failure there. `secret-names`, `tls-secrets`,
  `get-raw` and `raw-text` ignore the binding, so a candidate fn that must
  not mistake a failed read for an empty one avoids them;
- Helm's storage is read with `(kubectl/helm-releases context)` across the
  cluster, or with `(kubectl/helm-releases context namespace release)` in
  one storage namespace, selected by `owner=helm,name=<release>` on the
  server. A book that knows where a release is stored uses the namespaced
  form: on a busy cluster the cluster-wide listing can outrun the 30s
  request timeout, and it needs list rights on every namespace's Secrets.
  Both honour `kubectl/*strict-reads*`. The `:kubectl-can-*` prerequisites
  always check a right across every namespace, so a book whose namespace is
  known only once it reads the cluster leaves that read's resource out of
  them and lets the read itself refuse before any effect;
- a `namespace/name` target splits with `(kubectl/namespace-name-map
  target)`, which returns the map `{:namespace … :name …}`, so a book
  destructures it by key, `(let [{:keys [namespace name]} …])`. A vector
  binding throws only when the call runs; gumshoe's conformance suite
  refuses one. `split-namespace-name` is its deprecated alias.

A plugin calls `gumshoe.plugin/provide!` once with any of the seams:
announcers, detectives, capabilities, tools, pre- and post-hooks, secrets,
themes, prerequisites, drill-down probes, kinds and fetched edges.
`examples/example/plugin.clj` uses every one. A fetched-edges builder runs
under strict reads, so a read that does not answer is named in the
drill-down as unread and fails the run, never shown as nothing related. An
owner kind no plugin registered is offered as an unsupported kind and is
never read. Hooks and announcers print to
stderr, because a post-hook runs after a scan printed its report on stdout. In the gumshoe repository,
`bb test` runs the unit and conformance suites; the conformance suite
requires that every file parses, and a license header, a namespace
docstring, one harness entry point and a description on every book and
every option, and refuses a vector binding of `namespace-name-map`.

A casebook gates on the same rules with `gumshoe.conformance`. Wire it as a
`verify` task in the casebook's `bb.edn`, as `casebook-example/bb.edn` does,
and run `bb verify` in CI beside the casebook's tests:

```clojure
verify {:doc "Check the casebook's books and libraries"
        :requires ([gumshoe.conformance :as conformance])
        :task (conformance/verify! {:license "0BSD"})}
```

It reads `runbooks`, `playbooks` and `firebooks` as books and `libraries` as
libraries (override with `:books` and `:libraries`, each a list of
directories), prints one line per finding naming the file, and exits 1 when
there is any; a file with an unbalanced form is reported as `unparseable`,
and a vector binding of `namespace-name-map` or `split-namespace-name` as
`destructuring`. It also prints advisories as `note:` lines, which never fail
it: a mutation book whose spec is a literal map without `:recovery` is named
(`conformance/advisories` returns them as data). After the notes it prints
one count line per check, `note: 3 option-order advisories in 3 file(s), not
gating`.
`:license` names the SPDX identifier every file must carry; without it any
identifier passes. A casebook that carries no SPDX headers passes
`:license false`, which omits the header check and keeps every other one, so
a check gumshoe adds later still runs there. `conformance/findings-in` returns the findings as data
instead, for a test or a bespoke report. Loading the plugins does not parse
a book, so a casebook gate without this check passes a book nobody can
run.

The check, and gumshoe's own suite, also load every book that parses, in the one babashka process
the task runs in, with `gumshoe.runbook/*loading*` bound so the harness
hands back the book's config instead of running it. A book naming a
function that does not exist, `reset/action-in-flight?` after a library
dropped it, is reported as `load` with the book and the symbol named, and
nobody has to run the book to find out. The casebook's `libraries` (or the
`:libraries` roots) go on the classpath first; `:load? false` skips the
step. A book's top-level forms other than its harness call still run while
it loads, so they must not act on anything.

The same pass judges every mutation spec a book or library defines, as it
evaluates: what a book hands `mutation/book`, `playbook/book` (each change
step) or `dispatch/book`, and every library var holding a spec, a
`toggle/pair` or a playbook. Each finding is a `spec-contract` line naming
the spec (`gumshoe.steps.nodes/cordon`, `runbooks.flux.reset`,
`… step 7 "▶  Resume the HelmRelease"`) and what it omits: a confirmation
action, whether it is destructive, `:prerequisites`, an `:effect`, or a
post-check without a `:no-verify "why"` reason. A declared `:pin` is called
with a synthetic pick in its own namespace and must return a ref with a
`:type` and a `:name`, in the picked namespace when it has one; a pin that
returns nil there (it reads derived values) is not judged. A book whose spec
merges a library spec inherits its problems, and they are named once,
against the library. A library that cannot be required is a `load` finding.
A casebook that takes a gumshoe release before fixing or annotating its
specs passes `:spec-contract :advise`, which prints these as `note:` lines;
`conformance/report-in` returns `{:findings :advisories}` as data. A
destructive spec with no `:refusal`, `:precondition`, `:subject`,
`:subjects` or `:pin` and no `:no-guard "why"` is a `guard` line: a note by
default, a finding under `:guards :require`, and a failure in gumshoe's own
gate.
