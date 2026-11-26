;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.runbook
  "Shared entry point for every runbook.

   All runbooks behave the same way:
   - uniform CLI flags with -h/--help, unknown flags are rejected
   - prerequisites (tooling, connectivity, permissions) are checked first
   - flags are always optional where interactive selection can fill the gap
   - the process exits 0 on success and 1 on failure or abort; under the
     opt-in detailed contract a read-only run that only reported critical
     findings exits 2 (gumshoe.outcome/detailed-exit-code)"
  (:require [babashka.cli :as cli]
            [clojure.string :as str]
            [gumshoe.command :as command]
            [gumshoe.config :as config]
            [gumshoe.flags :as flags]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.flow :as flow]
            [gumshoe.hooks :as hooks]
            [gumshoe.outcome :as outcome]
            [gumshoe.summary :as summary]
            [gumshoe.ping :as ping]
            [gumshoe.plugins :as plugins]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.progress :as progress]
            [gumshoe.recording :as recording]
            [gumshoe.reproducer :as reproducer]
            [gumshoe.secrets :as secrets]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.theme :as theme]
            [gumshoe.ui :as ui]
            [gumshoe.upterm :as upterm]
            [gumshoe.utils :as utils]))

(def standard-options
  {:dry-run {:desc "Show what would change without changing anything"
             :coerce :boolean}
   :no-color {:desc "Disable colored output (also honors the NO_COLOR env var)"
              :coerce :boolean}
   :exit-codes {:desc "Exit codes: default (0/1), or detailed (2 = only critical findings); env GUMSHOE_EXIT_CODES"
                :coerce :string
                :validate {:pred #{"default" "detailed"}
                           :ex-msg (fn [{:keys [value]}]
                                     (format "invalid exit-code contract %s - use default or detailed" (pr-str value)))}}
   :help {:desc "Show this help message"
          :alias :h
          :coerce :boolean}})

(defn cli-spec
  "The babashka.cli spec a book with these options parses its arguments
   with: the options and the standard flags, unknown flags refused, and the
   deprecated aliases the options still take (gumshoe.flags), each named in
   its option's description. The spec lists the options in their order and
   the standard flags after them, so --help lists them so too."
  [options]
  {:spec (flags/merge-ordered (flags/documented options) standard-options)
   :alias (flags/deprecated-aliases options)
   :restrict true})

(defn arguments
  "What a book with these options makes of its arguments, as {:opts ..
   :notes [..]}, the notes naming each deprecated alias the arguments use.
   A refused argument goes to `error-fn` as babashka.cli hands it over, or
   throws without one. A book takes no positional argument, so the first one
   is refused with cause :unexpected-argument (flags/parse-without-positionals)."
  ([options args] (arguments options args nil))
  ([options args error-fn]
   {:opts (flags/parse-without-positionals
           args (cond-> (cli-spec options) error-fn (assoc :error-fn error-fn)) "a book")
    :notes (flags/notes options args)}))

(defn help-text
  ([description spec] (help-text description spec nil))
  ([description spec details]
   (let [file (System/getProperty "babashka.file")]
     (str (stdout/bold description)
          "\n\n"
          (when (seq details)
            (str (str/join "\n" (map #(if (or (str/blank? %) (str/starts-with? % " ")) % (stdout/yellow %))
                                     details))
                 "\n\n"))
          (stdout/yellow "Usage:")
          ;; an EDN book runs through the run door, not as a bb script
          "\n  bb " (cond (nil? file) "<runbook>"
                          (str/ends-with? file ".edn") (str "run " file)
                          :else file)
          " [options]"
          "\n\n"
          (stdout/yellow "Options:")
          "\n"
          (cli/format-opts (merge spec {:order (vec (keys (:spec spec)))}))))))

(defn help
  "The --help text of a book's config."
  [{:keys [description options details]}]
  (help-text description (cli-spec (or options {})) details))

;; Each prerequisite is one [label thunk] checklist item. The thunk does the
;; (often slow) check and returns {:ok? bool :label "..."} - so the item animates
;; while the check runs and settles into a final line naming exactly what passed
;; or failed (the tool version, the unreachable host, the missing permission).

(defn- tool-item
  [tool]
  [(str "tool: " tool)
   #(if (command/installed? tool)
      {:ok? true :label (str "tool: " (command/describe-installed tool))}
      {:ok? false :label (str "tool not installed: " tool)})])

(defn- min-version-item
  [[tool required]]
  [(format "%s meets version %s" tool required)
   #(let [v (command/version tool)]
      (if (command/version-at-least? v required)
        {:ok? true :label (format "%s %s meets minimum %s" tool (or v "") required)}
        {:ok? false :label (format "%s is %s, below minimum %s" tool (or v "unknown") required)}))])

(defn- ping-item
  [version host]
  [(format "reachable: %s (IPv%d)" host version)
   #(if (ping/reachable? version host)
      {:ok? true :label (format "reachable: %s (IPv%d)" host version)}
      {:ok? false :label (format "not reachable: %s (IPv%d)" host version)})])

(defn- secret-item
  [secret]
  [(str "secret: " secret)
   #(if (secrets/available? secret)
      {:ok? true :label (str "secret available: " secret)}
      {:ok? false :label (str "secret not available: " secret)})])

(defn- cluster-item
  "Checks the current cluster is one the operator configured and advertises the
   capabilities the book needs. Capabilities are opt-in labels in env.edn: a
   cluster that declares none is not capability-gated (any book runs on it, once
   it is a known cluster), so onboarding stays smooth while labelled clusters get
   the safety of a real match."
  [required-capabilities]
  ["connected to a suitable Kubernetes cluster"
   #(let [current (kubectl/current-cluster)
          known (config/known-clusters)
          declared (config/env-value {:kubernetes-cluster current} [:capabilities])
          missing (when declared (remove (set declared) required-capabilities))]
      (cond
        (str/blank? (str current))
        {:ok? false :label "not connected to any Kubernetes cluster"}

        (and (seq known) (not (some #{current} known)))
        {:ok? false :label (format "current Kubernetes cluster %s is none you configured: %s"
                                   current (str/join ", " known))}

        (seq missing)
        {:ok? false :label (format "Kubernetes cluster %s does not advertise: %s"
                                   current (str/join ", " (map name missing)))}

        :else
        {:ok? true :label (if (seq required-capabilities)
                            (format "Kubernetes cluster: %s (has %s)" current (str/join ", " (map name required-capabilities)))
                            (format "Kubernetes cluster: %s" current))}))])

(defn- resource-label
  "The label for a kubectl resource in a permission prerequisite: the cluster's
   Kind when discovery knows it (\"Pod resources\"), else the verbatim resource
   name, followed by the subresource it names (\"Pod resources (eviction
   subresource)\"). A fabricated Kind would read as certainty the check does
   not have, so an unknown or unreachable resource stays as the raw name the
   check actually uses."
  [resource]
  (let [[base sub] (kubectl/resource-and-subresource resource)
        label (if-let [kind (kubectl/resource-kind base)] (str kind " resources") base)]
    (if sub (format "%s (%s subresource)" label sub) label)))

(defn- can-i-item
  [verb resource]
  (let [verb-label (str/upper-case verb)
        target (resource-label resource)
        heading (format "Kubernetes: can %s %s" verb-label target)]
    [heading
     #(if (kubectl/can-i? verb resource)
        {:ok? true :label heading}
        {:ok? false :label (format "Kubernetes: cannot %s %s" verb-label target)})]))

(defn- can-exec-item
  [resource]
  (let [target (resource-label resource)
        heading (format "Kubernetes: can EXEC into %s" target)]
    [heading
     #(if (kubectl/can-exec? resource)
        {:ok? true :label heading}
        {:ok? false :label (format "Kubernetes: cannot EXEC into %s" target)})]))

(defn permission-checks
  "The `kubectl auth can-i` questions the permission prerequisites ask, as
   [verb resource] pairs in the order they are checked: one per resource of
   each key of gumshoe.prerequisites/kubectl-verbs, and create on the exec
   subresource for each of :kubectl-can-exec."
  [prerequisites]
  (concat
   (for [[key verb] prerequisites/kubectl-verbs
         resource (get prerequisites key)]
     [verb resource])
   (for [resource (:kubectl-can-exec prerequisites)]
     ["create" (str resource "/exec")])))

(def ^:private built-in-prerequisite-keys
  "The :prerequisites keys the harness handles directly; everything else must be
   a plugin-registered check. A declared key in neither set is a typo or a plugin
   that failed to load."
  #{:can-ping-using-ipv4 :can-ping-using-ipv6 :access-gopass-secrets
    :installed-tools :minimum-tool-versions :existing-paths :cluster-capabilities
    :kubectl-can-get :kubectl-can-create :kubectl-can-update :kubectl-can-patch
    :kubectl-can-delete :kubectl-can-exec})

(defn- unknown-prerequisite-items
  "Fail-closed items for declared prerequisite keys that no built-in handles and
   no plugin registered - so a change gate whose providing plugin is missing (or
   whose key is misspelled) blocks the book instead of being silently dropped."
  [prerequisites]
  (let [known (into built-in-prerequisite-keys (prerequisites/registered-checks))]
    (for [key (remove known (keys prerequisites))]
      [(format "prerequisite %s" key)
       (fn [] {:ok? false
               :label (format "unknown prerequisite %s - its providing plugin is not loaded, or the key is misspelled"
                              key)})])))

(defn- built-items
  "The checklist items (build) returns, or one failing item naming what
   threw when it throws: a prerequisite builder a plugin or a tool package
   registers runs while the list is built, before any item's own guard, and
   one that throws must show as an unmet prerequisite, not a stack trace."
  [label build]
  (try (doall (build))
       (catch Throwable e
         (let [message (or (ex-message e) (str e))]
           [[label (fn [] {:ok? false :label (format "%s could not be checked: %s" label message)})]]))))

(defn- prerequisite-items
  [prerequisites opts]
  (let [blank? #(str/blank? (str %))
        ipv4-hosts (remove blank? (map opts (:can-ping-using-ipv4 prerequisites)))
        ipv6-hosts (remove blank? (map opts (:can-ping-using-ipv6 prerequisites)))
        secrets (remove blank? (map opts (:access-gopass-secrets prerequisites)))
        tools (utils/conj-if-not-empty (:installed-tools prerequisites) secrets (secrets/command-name))
        ;; a tool brings its own version floor: any book that lists it inherits
        ;; the min-version its tool-support package registered, the book's own
        ;; :minimum-tool-versions winning on conflict
        tool-floors (into {} (keep (fn [t] (when-let [v (command/tool-min-version t)] [t v])) tools))
        min-versions (merge tool-floors (:minimum-tool-versions prerequisites))]
    (concat
     (map tool-item tools)
     (map min-version-item min-versions)
     ;; and any extra checks the tool declared it needs (a service to reach, a login)
     (mapcat (fn [t] (when-let [build (command/tool-prerequisites t)]
                       (built-items (format "the prerequisites of %s" t) #(build opts))))
             tools)
     (map (partial ping-item 4) ipv4-hosts)
     (map (partial ping-item 6) ipv6-hosts)
     (map secret-item secrets)
     (prerequisites/existing-path-items (:existing-paths prerequisites) opts)
     (when (contains? prerequisites :cluster-capabilities)
       [(cluster-item (:cluster-capabilities prerequisites))])
     (mapcat (fn [[key verb]] (map (partial can-i-item verb) (get prerequisites key)))
             prerequisites/kubectl-verbs)
     (map can-exec-item (:kubectl-can-exec prerequisites))
     ;; plugin-registered checks (change windows, tickets, on-call acks) run last
     ;; one registered check at a time, so a builder that throws fails its
     ;; own item and the others still run
     (mapcat (fn [key]
               (built-items (format "prerequisite %s" key)
                            #(prerequisites/items (select-keys prerequisites [key]) opts)))
             (prerequisites/registered-checks))
     ;; and a fail-closed item for any declared key nothing handles
     (unknown-prerequisite-items prerequisites))))

(defn- prerequisites?
  [prerequisites opts]
  (stdout/print-section "📋 Prerequisites")
  (let [items (prerequisite-items prerequisites opts)]
    (or (empty? items)
        (progress/checklist items {:stop-on-failure? false}))))

(defn- git-commit
  []
  (if (empty? (shell/stdout-of "git" "status" "--porcelain"))
    (shell/stdout-of "git" "log" "--max-count=1" "--format=%h")
    "DIRTY"))

(defn- hostname
  []
  (if (command/installed? "hostname")
    (shell/stdout-of "hostname")
    (shell/stdout-of "hostnamectl" "hostname")))

(defn- announcement-data
  "The context of a change, passed to every configured announcer: which cluster's
   environment it happened in (so the right announcers are selected), who did it,
   and the git provenance. Where to post and how is each announcer's own config
   in env.edn :announce - the announcers resolve their homeserver, room, webhook
   URL, and tokens themselves."
  []
  (let [username (shell/stdout-of "whoami")
        host (hostname)]
    {:git-branch (shell/stdout-of "git" "rev-parse" "--abbrev-ref" "HEAD")
     :git-commit (git-commit)
     :username username
     :hostname host
     :cluster (try (kubectl/current-cluster) (catch Exception _ nil))
     :actor (format "%s@%s" username host)}))

(defn- book-heading
  "The run header for a book: its kind and its description. execute! backs
   runbooks, playbooks, firebooks, and mutating books alike, so the kind is read
   from the book's path segment rather than assumed - a playbook or firebook must
   not announce itself as a Runbook."
  [book-file description]
  (let [path (str book-file)
        [emoji kind] (cond
                       (str/includes? path "/firebooks/") ["🔥" "Firebook"]
                       (str/includes? path "/playbooks/") ["📋" "Playbook"]
                       :else ["🚀" "Runbook"])]
    (if (str/blank? (str description))
      (str emoji " " kind)
      (format "%s %s · %s" emoji kind description))))

(def ^:dynamic *loading*
  "While true, execute! hands back its config and does nothing else: no
   plugin loads, no argument parse, no action. Every harness entry point goes
   through execute!, so a book loaded under this binding is analysed - every
   symbol it names must resolve - without running."
  false)

(defn- print-outcome!
  "Prints the final banner and, under a failed one, the recovery guidance of
   the step the run stopped at."
  [{:keys [tone text]} record]
  (case tone
    :ok (stdout/print-banner stdout/green (str (theme/token :ok) " " text))
    :neutral (stdout/print-banner stdout/yellow (str (theme/token :warn) " " text))
    (stdout/print-banner stdout/red (str (theme/token :error) " " text)))
  (doseq [line (outcome/recovery-lines tone record)]
    (stdout/err-println (stdout/yellow line))))

(defn read-only-run?
  "Pure: whether a finished run only read, so the CLI calls it recorded are safe
   to hand out as a reproducer. A change book never is, even one that posts
   nothing to the changelog (a debug pod), and neither is any run that reached
   an effect or a legacy thunk: a reproducer of it would replay the change."
  [change? record]
  (let [{:keys [applied stopped thunk]} record]
    (and (not change?)
         (zero? (or applied 0))
         (nil? stopped)
         (nil? thunk))))

(defn offers-share?
  "Pure: whether a finished read-only run asks to share its findings. The
   offer is an fzf prompt drawn on /dev/tty, which waits for a key whenever
   the process has a controlling terminal, even with stdout piped. So it is
   asked only of a text run (a book without --output is one) on a console:
   a json or edn run, a pipe, cron or a pty nobody watches ends without it,
   as the drill-down offer does."
  [{:keys [output console?]}]
  (boolean (and console? (contains? #{nil "text"} output))))

(defn change-book?
  "Pure: whether a run of the book with the parsed opts may change anything.
   :change? is a boolean or a (fn [opts]) for a book that changes something
   only under a flag; without it a book that announces is a change book."
  [{:keys [announce?] :or {announce? true} :as config} opts]
  (let [declared (:change? config)]
    (boolean (cond
               (fn? declared) (declared opts)
               (contains? config :change?) declared
               :else announce?))))

(defn exit-mode
  "Pure: the exit-code contract of a run, :default or :detailed, from the
   --exit-codes flag and else the GUMSHOE_EXIT_CODES value, as {:mode ..
   :note ..}. The flag wins. An env value that names no contract is ignored
   with a note: :default reports every failure and every finding as 1, so
   falling back to it never lets a failure pass."
  [flag env]
  (let [env (some-> env str str/trim str/lower-case not-empty)]
    (cond
      flag {:mode (keyword flag)}
      (nil? env) {:mode :default}
      (contains? outcome/exit-modes (keyword env)) {:mode (keyword env)}
      :else {:mode :default
             :note (format "GUMSHOE_EXIT_CODES=%s names no exit-code contract (default, detailed) - using default"
                           (pr-str env))})))

(defn verdict
  "Pure: the final banner and the exit code of a finished run of the book
   described by config, with the parsed opts, the outcome of its action, the
   record of what it did and whether it captured findings, as
   {:banner {:tone .. :text ..} :exit 0|1|2}. Whether the run may change
   things comes from change-book?, which honours :change?, so a change book
   that posts nothing is bannered as a change. Both halves are judged from
   the same outcome and record, so the banner never contradicts the exit
   code.

   The exit code follows the contract (:exit-codes opts), :default or
   :detailed, as outcome/detailed-exit-code judges it; a string names it as
   the flag does, and nil is :default."
  [config opts outcome record findings?]
  (let [run {:dry-run? (boolean (:dry-run opts))
             :changes? (change-book? config opts)
             :outcome outcome
             :findings? (boolean findings?)}]
    {:banner (outcome/banner run record)
     :exit (outcome/detailed-exit-code (some-> (:exit-codes opts) keyword) run record)}))

(defn execute!
  "Runs a runbook described by a config map:

   :description   one-line summary, shown by --help
   :options       babashka.cli option spec (standard flags are added)
   :prerequisites tooling/connection/permission checks to run first
   :announce?     whether the action posts to the changelog (default true)
   :change?       whether the book changes anything (default :announce?); a
                  change book that posts nothing, such as one starting a debug
                  pod, sets it true so pre-hooks (a change freeze) see a change
                  and no reproducer of its calls is offered. A book that
                  changes something only under a flag passes (fn [opts] ..),
                  judged on the parsed options
   :action        (fn [opts ctx]) -> truthy on success
   :details       optional lines --help shows under the description; an
                  unindented line is a heading

   ctx contains :announcement-data when :announce? is true. Under *loading*
   it returns the config untouched."
  [{:keys [description options prerequisites announce? action]
    :or {options {} prerequisites {} announce? true}
    :as config}]
  (if *loading*
    config
    (let [error-fn (fn [{:keys [type cause msg option]}]
                     (when (= :org.babashka/cli type)
                       (case cause
                         :require (stdout/error (format "missing required option: --%s" (name option)))
                         :validate (stdout/error (format "invalid value for option --%s: %s" (name option) msg))
                         (stdout/error msg))
                       (System/exit 1)))]
      (when (some #{"--no-color"} *command-line-args*) (stdout/disable-colors!))
      ;; load the configured plugins - their code is already on the classpath via
      ;; deps - before the parse, which validates --output against the formats
      ;; they register, and before anything in this run reaches for their
      ;; announcers, detectives and the like
      (plugins/load!)
      ;; help is answered before the strict parse, so required options never
      ;; stand between the user and the usage text
      (if (some #{"-h" "--help"} *command-line-args*)
        (println (help config))
        (let [{:keys [opts notes]} (arguments options *command-line-args* error-fn)
              change? (change-book? config opts)
              exit-contract (exit-mode (:exit-codes opts) (System/getenv "GUMSHOE_EXIT_CODES"))]
          ;; select the output theme and the interactive backend from env.edn (or a
          ;; plugin's, just loaded) before anything prints or prompts
          (theme/apply!)
          (ui/activate!)
          (doseq [note (cond-> (vec notes) (:note exit-contract) (conj (:note exit-contract)))] (stdout/warn note))
          (when-not (prerequisites? prerequisites opts)
            (stdout/print-banner stdout/red (str (theme/token :error) " PREREQUISITES NOT MET - nothing was attempted"))
            (stdout/err-println "Next: install the missing tools, connect to the right cluster/VPN, or fix the")
            (stdout/err-println (str "      access shown with a " (theme/token :check-error) " above, then run this book again."))
            (System/exit 1))
          ;; global pre-execution gates (change freeze, required ack) may veto the
          ;; run before it touches anything - they apply to every book, unlike a
          ;; book's own declared prerequisites
          (let [gate (hooks/run-pre-hooks! {:description description
                                            :book (System/getProperty "babashka.file")
                                            :opts opts
                                            :change? change?})]
            (when-not (:allowed? gate)
              (stdout/print-banner stdout/red (str (theme/token :error) " BLOCKED - " (:reason gate)))
              (System/exit 1)))
          (let [ctx (when announce? {:announcement-data (announcement-data)})]
            (stdout/print-section (book-heading (System/getProperty "babashka.file") description))
            ;; an unexpected exception must still land on the red banner with a
            ;; clean message, never a raw stack trace - the exit code stays 1 so
            ;; scripts and ./detect see the failure
            (let [record (outcome/fresh)
                  outcome (try
                            (binding [flow/*dry-run* (boolean (:dry-run opts))
                                      outcome/*record* record]
                              ;; a rehearsal that passed a step it could not
                              ;; show did not succeed, whatever the action returned
                              (if (and (action opts ctx) (empty? (:unrehearsed @record))) :ok :failed))
                            ;; Throwable, not Exception: an Error from the action
                            ;; must still land on the red banner and the recording,
                            ;; not skip them with a raw stack trace.
                            (catch Throwable e
                              (stdout/error "unexpected error:" (or (ex-message e) (str e)))
                              :error))
                  ;; every run leaves an audit trail: input vars, selections, commands
                  recording-path (recording/save! {:book-file (System/getProperty "babashka.file")
                                                   :description description
                                                   :opts opts
                                                   :outcome outcome
                                                   :skipped (:skipped @record)
                                                   :meta (:announcement-data ctx)})
                  final (verdict config (assoc opts :exit-codes (:mode exit-contract)) outcome @record (summary/captured?))]
              ;; the final banner leaves no doubt about the outcome, even at 3am
              (print-outcome! (:banner final) @record)
              ;; run summary: where the record is, how to reproduce, how to dig in together
              (when recording-path
                (stdout/err-println (format "📼 Recording: %s" (stdout/shorten-path recording-path))))
              (when (read-only-run? change? @record)
                ;; the exact CLI calls behind a read-only run's output are safe to replay
                (reproducer/offer! description)
                ;; the findings themselves, as Markdown, for a shared pad or a ticket
                (when (offers-share? {:output (:output opts) :console? (some? (System/console))})
                  (summary/offer! description))
                ;; something turned up - offer a shared terminal to investigate together
                (when (= :failed outcome)
                  (upterm/offer!)))
              ;; post-execution hooks observe the finished run (metrics, audit
              ;; forwarding, status pages) - bounded so none can block the exit
              (hooks/run-post-hooks! {:description description
                                      :book (System/getProperty "babashka.file")
                                      :opts opts
                                      :outcome outcome
                                      :recording-path recording-path
                                      :meta (:announcement-data ctx)})
              (System/exit (:exit final)))))))))
