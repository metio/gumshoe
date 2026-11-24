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
| `runbooks/detectives/cluster.clj` | every registered detective, except posture; takes `--hpa-grace-minutes` like `workloads.clj` |
| `runbooks/detectives/workloads.clj` | controllers, jobs, pods, Services, their storage; an Ingress backend Service that is missing is critical, and info when ingress-nginx redirects every request of that Ingress (`permanent-redirect`, `temporal-redirect`, or a top-level catch-all redirecting `rewrite` or `return 30x` in `configuration-snippet`); an HPA whose AbleToScale or ScalingActive condition is False is critical once it has held for `--hpa-grace-minutes` (default 10, read from the condition's `lastTransitionTime` at collection time) and a warning naming its age before that, while one whose age cannot be read stays critical |
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
| `runbooks/detectives/fabric.clj` | EVPN-VXLAN MAC ownership, over SSH; each `evpn-fdb-conflict` finding carries `local-uptime`, `bgp-route` (`present`/`absent`/`unread`) and `classification` (`live-misadvertisement`/`orphan`/`unknown`) |

Posture findings describe risks that may be deliberate choices, so they run
only when posture is asked for.

Each tool package under `tools/<name>/` brings a `scan.clj` for its system,
registers its detectives into the scopes above, and adds runbooks for its
common actions. In the cluster they cover Argo CD, calico (including Whisker
flow logs), cert-manager, Cluster API, CloudNativePG, Crossplane, db-operator,
external-dns, Flagger, Flux, Gateway API, JaaS, Karpenter, Kueue, Loki,
Prometheus, StageSet, Strimzi, Thanos, Traefik, and Velero. Outside it they
cover ceph and OpenNebula over SSH, Matrix homeservers, restic repositories,
the nginx/php-fpm upload limit chain of an app, and host facts from
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
   what that step needs. A `DRY RUN stopped by an unexpected error` banner
   means the rehearsal itself broke and the plans after the error were never
   shown; read the error before relying on the plan. A playbook passes its
   pauses under `--dry-run`; a
   real run without a terminal stops at the first pause with a `PAUSED`
   banner naming the `--from-segment` that continues after the human acted.
3. Show the human the plan and the command without `--dry-run`, written so
   they can run it in their own session, for example
   `! bb run runbooks/kubernetes/nodes/cordon.clj --node worker-3`.
4. After they run it, check the outcome with the matching detective. The
   final banner says what the run did: `DONE - applied and verified` is the
   only one that promises its post-checks held, `DONE - applied; this book has
   no post-checks` verified nothing, and a `FAILED after N effects` banner
   names where it stopped while the effects before it stay applied. Lines
   under a failed banner headed `Recovery:` are the stopped step's guidance:
   what the half-applied state means and how to bring the system back. A
   mutation book without its own `:recovery` that fails after applying an
   effect prints generic guidance there instead, starting `This change
   declares no recovery guidance`: which effects stayed applied, where it
   stopped or that its post-check did not hold, and where to look (those
   messages, the changelog entry, the object on its cluster). Read them to
   the human before anything else is run.

A book whose change cannot be expressed as a plan says under `--dry-run` that
it does not support a dry run, and runs nothing. Interactive sessions
(`netshoot`, `psql` connections, `upterm` sharing) are for the human
entirely.

## Prerequisites and configuration

Every book checks its tools, their versions, connectivity, secrets, the
directories its options name, and permissions before it acts, and prints a checklist naming what is missing.
With breto, `eval "$(breto env)"` puts every tool a book needs on `PATH`.

`env.edn` (optional, gitignored, built by `bb run runbooks/setup/init.clj`)
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

- a detective book is `(detective/book {:description … :detectives …})`.
  Evidence from outside the cluster's resources comes from `:collect (fn
  [opts] evidence)`, returning nil when it can not collect after printing
  why; `:around (fn [opts run] …)` wraps it in a port-forward or connection
  check and calls `(run opts)`, and `:extra (fn [evidence] map)` adds rows to
  the json/edn report. Checks whose `:requires` all came back nil are marked
  unread, so a collection that read nothing fails with "nothing was
  checked". `:requires` must name the evidence keys as the collection
  returns them, keywords or strings. `(detective/check-step {:title …
  :detectives … :blocking? false})` is the playbook step for the same;
- a uniform change is `(mutation/book {:select … :confirm … :effect …
  :verify …})`, whose effect plan (`[[:kubectl "cordon" node]]`) runs,
  dry-runs, or is collected in a test. An `:effect` returning one effect,
  such as a bare `(effect/note …)`, runs as a plan of one; anything else
  that is not a plan, a keyword, a map or a plan holding a non-effect, is
  refused with an error naming the step and the fix, `effect/plan`, before
  the step asks for confirmation or runs any of it. In a playbook the steps
  before it have already run. Its `:derive` reads the cluster
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
  check still turns that timeout into a warning. Reads over SSH, among them
  every `gumshoe.ceph` read, ignore the binding, so a check built on them
  still has to tell a failed read from an empty one itself;
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
  refusal runs, so they also run for a pick that does not parse, with
  `:ref` and `:object` nil: make the plan a note in that case instead of
  reading through the nil ref (`runbooks/kubernetes/workloads/restart.clj`).
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
  and options from the steps and asks for one confirmation;
- any step whose failure leaves a system parked or half-owned - a controller
  suspended, a finalizer stripped, an owner removed before its replacement
  exists - carries `:recovery`, a string or `(fn [ctx] …)` returning one,
  which the run prints under the `FAILED` banner when it stops at that step.
  A function receives the step's context with `:applied`, runs at the
  failure, and can read the cluster to say what is still suspended and give
  the commands that undo it; one that throws leaves the banner intact and
  says the guidance could not be rendered. The playbook's own `:recovery`
  covers its steps without one once the run changed something, which reaches
  a caller's steps added at positions; a mutation book carries `:recovery`
  the same way. A mutation without `:recovery` that fails after applying an
  effect still prints the engine's generic guidance (`mutation/fallback-guidance`:
  the effects that stayed applied and where to look), which a step's or the
  playbook's own `:recovery` replaces, and a dry run prints none. `gumshoe.tools.flux-storage/recovery` is the example: it
  names which of the HelmRelease and its owner stay suspended. A rollback
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
  namespace; after the extinguish the out check waits until none does,
  and evidence a detective read nothing from never passes either check.
  Any other drill declares `:derive`, `:ignite`, `:extinguish` (with its
  `:recovery`) and an `:expect :collect` that throws `(firebook/unread
  msg)` when it could not read; `firebooks/fabric/stale_fdb_entry.clj` is
  the example;
- a dispatcher is `(dispatch/book {:select … :classify (fn [ctx target] key)
  :routes {key {:playbook …}}})`, one front door that runs the playbook a
  subject's kind routes to;
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
  `:effect`, `:verify` - which a step merges with its own title,
  confirmation, `:derive`, `:precondition` and `:recovery`:
  `(gumshoe.steps.objects/delete {:objects (fn [ctx] …) :none "…" :timeout
  …})` deletes `{:type :namespace :name}` objects without waiting and proves
  each gone with a read that answered; `gumshoe.steps.flux/suspend`,
  `resume` and `reconcile` take `{:object (fn [ctx] {:kind :type :namespace
  :name}) …}` for any kind in `flux/suspendable` or a ResourceSet, and
  `reconcile` waits for `status.lastHandledReconcileAt` to equal the token
  it stamped;
- a bespoke book calls `runbook/execute!` with its own `:action`;
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
`examples/example/plugin.clj` uses every one. In the gumshoe repository,
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
(`conformance/advisories` returns them as data).
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
