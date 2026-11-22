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
./run --list                 # in a gumshoe clone
bb run --list                # in a casebook
```

Each line is a book's absolute path, a tab, and its description, for every
book on the classpath: gumshoe's built-ins, every tool package the project
depends on, and the casebook's own books.

`bb run` in a casebook is a task of its `bb.edn`, and babashka's own `run`
answers when the task is missing. Check `bb tasks` first; a casebook without
`run` and `detect` needs them, as in `casebook-example/bb.edn`:

```clojure
:tasks {detect {:requires ([gumshoe.main :as main])
                :task (System/exit (main/detect *command-line-args*))}
        run    {:override-builtin true
                :requires ([gumshoe.main :as main])
                :task (System/exit (main/run *command-line-args*))}}
```

`System/exit` carries the book's exit code to the shell, and
`:override-builtin true` silences babashka's warning that the task replaces
its built-in `run`.

Filter the list with `grep`, then read the book's options:

```sh
./run --list | grep -i velero
bb <path> --help
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
- **Use `--output json`** (or `edn`) on detectives and scans. Results go to
  standard output and diagnostics to standard error, so `2> scan.log | jq`
  parses the report while the log keeps the prerequisite checklist and any
  error.
- **Read the exit code.** `0` means success or nothing critical; `1` means a
  critical finding, a failure, an abort, or a failed post-check.
- **Skip the interactive front doors.** `./gumshoe`, `./detect`,
  `./investigate` and `./run` without `--list` are fzf menus for people. The
  drill-down of `./investigate` follows one object to the next by asking at
  every step, so an agent runs the detectives it would reach instead.
- **Check which cluster is current** with `kubectl config current-context`
  before running anything. Kubernetes books act on the current context, and
  `env.edn` picks its environment (ceph host, VPN interface) by that context.

A read-only run writes a reproducer script of the CLI calls it made and
copies it to the clipboard when a clipboard tool is installed, so running
books overwrites the user's clipboard. Every run also leaves an audit trail
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
- `parent` marks a finding that follows from a more severe one about its
  workload or node. Fix the parent first.
- `since` is when the problem began, where the API records it.
- `checked` lists every detective that ran. An entry with `unread` read
  nothing, for example because the cluster does not serve that CRD or RBAC
  refused the read, or because the scope it judges selected no subjects, so
  an empty finding list from it proves nothing. Say so when you report a
  clean result.
- `nothing-checked: true` means no detective read anything. The book exits 1
  and the run is not a clean result; report it as unchecked, then look for
  the scope or capability that left every detective without evidence.

## What gumshoe covers

The detectives compose from one domain to the whole cluster:

| Book | Looks at |
| --- | --- |
| `runbooks/detectives/cluster.clj` | every registered detective, except posture |
| `runbooks/detectives/workloads.clj` | controllers, jobs, pods, Services, their storage |
| `runbooks/detectives/pods.clj` | restart loops, image pulls, OOM kills, Pending and Failed pods |
| `runbooks/detectives/nodes.clj` | readiness, pressure, cordoned nodes |
| `runbooks/detectives/platform.clj` | control plane, nodes, calico, CSI storage |
| `runbooks/detectives/storage.clj`, `csi.clj` | pending and unused claims, attachments, orphaned volumes |
| `runbooks/detectives/events.clj` | Warning events of the last hour |
| `runbooks/detectives/upgrade.clj` | removed APIs, webhooks, version skew, drain blockers |
| `runbooks/detectives/apiversions.clj` | CRD discovery drift |
| `runbooks/detectives/security.clj`, `rbac.clj` | RBAC, pod security, network segmentation |
| `runbooks/detectives/posture.clj` | probes, image tags, replicas, PDBs, deprecated APIs, template drift |
| `runbooks/detectives/databases.clj` | CloudNativePG and db-operator |
| `runbooks/detectives/observability.clj` | Prometheus, Alertmanager, ThanosRuler |
| `runbooks/detectives/dns.clj` | nameservers, SOA replication, answer consistency |
| `runbooks/detectives/mail.clj` | MX, SPF, DKIM, DMARC, rDNS, live SMTP/POP3/IMAP with TLS |
| `runbooks/detectives/fabric.clj` | EVPN-VXLAN MAC ownership, over SSH |

Posture findings describe risks that may be deliberate choices, so they run
only when posture is asked for.

Each tool package under `tools/<name>/` brings a `scan.clj` for its system,
registers its detectives into the scopes above, and adds runbooks for its
common actions. In the cluster they cover Argo CD, calico (including Whisker
flow logs), cert-manager, Cluster API, CloudNativePG, Crossplane, db-operator,
external-dns, Flagger, Flux, Gateway API, JaaS, Karpenter, Kueue, Loki,
Prometheus, StageSet, Strimzi, Thanos, Traefik, and Velero. Outside it they
cover ceph and OpenNebula over SSH, Matrix homeservers, restic repositories,
and the nginx/php-fpm upload limit chain of an app. A scope covers a tool only when
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
   what that step needs. A playbook passes its pauses under `--dry-run`; a
   real run without a terminal stops at the first pause with a `PAUSED`
   banner naming the `--from-segment` that continues after the human acted.
3. Show the human the plan and the command without `--dry-run`, written so
   they can run it in their own session, for example
   `! bb runbooks/kubernetes/nodes/cordon.clj --node worker-3`.
4. After they run it, check the outcome with the matching detective. The
   final banner says what the run did: `DONE - applied and verified` is the
   only one that promises its post-checks held, `DONE - applied; this book has
   no post-checks` verified nothing, and a `FAILED after N effects` banner
   names where it stopped while the effects before it stay applied.

A book whose change cannot be expressed as a plan says under `--dry-run` that
it does not support a dry run, and runs nothing. Interactive sessions
(`netshoot`, `psql` connections, `upterm` sharing) are for the human
entirely.

## Prerequisites and configuration

Every book checks its tools, their versions, connectivity, secrets, and
permissions before it acts, and prints a checklist naming what is missing.
With breto, `eval "$(breto env)"` puts every tool a book needs on `PATH`.

`env.edn` (optional, gitignored, built by `bb runbooks/setup/init.clj`)
holds per-host settings: environments selected by kubectl context, ceph mgr
hosts, VPN interfaces, the secrets provider (`:gopass`, `:pass`, `:passage`,
`:pasejo` or a `:command`), announcers, plugins, and the theme. A missing or
broken file means no configuration and never an error. Secrets come from the
password manager at runtime and never appear in process arguments.

## Extending gumshoe

A team keeps its own books in a **casebook**, a repository whose `bb.edn`
depends on a pinned gumshoe release and puts its own `runbooks`, `playbooks`
and `firebooks` on `:paths`; `casebook-example/` is a template. Books are
mostly data:

- a detective book is `(detective/book {:description … :detectives …})`;
- a uniform change is `(mutation/book {:select … :confirm … :effect …
  :verify …})`, whose effect plan (`[[:kubectl "cordon" node]]`) runs,
  dry-runs, or is collected in a test;
- a playbook is `(playbook/book {:select … :confirm … :steps […]})`, whose
  steps are mutation specs, checks and pauses; it derives its prerequisites
  and options from the steps and asks for one confirmation;
- a dispatcher is `(dispatch/book {:select … :classify (fn [ctx target] key)
  :routes {key {:playbook …}}})`, one front door that runs the playbook a
  subject's kind routes to;
- a step a runbook and a playbook share lives in a library, and the runbook
  names its description beside it, `(mutation/book (merge nodes/cordon
  {:description "…"}))`, so `./run --list` reads it without loading the book;
- a bespoke book calls `runbook/execute!` with its own `:action`;
- a cluster read no `gumshoe.kubectl` function wraps goes through
  `(kubectl/get-json context "namespaces" "--selector=…")`, nil when the read
  fails, or `get-json-result`, whose `:ok?` tells a failed read from an empty
  one before a gate trusts an absence.

A plugin calls `gumshoe.plugin/provide!` once with any of the seams:
announcers, detectives, capabilities, tools, pre- and post-hooks, secrets,
themes, prerequisites, drill-down probes, kinds and fetched edges.
`examples/example/plugin.clj` uses every one. In the gumshoe repository,
`bb test` runs the unit and conformance suites; the conformance suite
requires a license header, a namespace docstring, one harness entry point and
a description on every book and every option.
