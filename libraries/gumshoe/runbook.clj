;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.runbook
  "Shared entry point for every runbook.

   All runbooks behave the same way:
   - uniform CLI flags with -h/--help, unknown flags are rejected
   - prerequisites (tooling, connectivity, permissions) are checked first
   - flags are always optional where interactive selection can fill the gap
   - the process exits 0 on success and 1 on failure or abort"
  (:require [babashka.cli :as cli]
            [clojure.string :as str]
            [gumshoe.command :as command]
            [gumshoe.config :as config]
            [gumshoe.extensions :as extensions]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.flow :as flow]
            [gumshoe.summary :as summary]
            [gumshoe.ping :as ping]
            [gumshoe.plugins :as plugins]
            [gumshoe.progress :as progress]
            [gumshoe.recording :as recording]
            [gumshoe.reproducer :as reproducer]
            [gumshoe.secrets :as secrets]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]
            [gumshoe.theme :as theme]
            [gumshoe.upterm :as upterm]
            [gumshoe.utils :as utils]))

(def standard-options
  {:dry-run {:desc "Show what would change without changing anything"
             :coerce :boolean}
   :no-color {:desc "Disable colored output (also honors the NO_COLOR env var)"
              :coerce :boolean}
   :help {:desc "Show this help message"
          :alias :h
          :coerce :boolean}})

(defn help-text
  [description spec]
  (let [file (System/getProperty "babashka.file")]
    (str (stdout/bold description)
         "\n\n"
         (stdout/yellow "Usage:")
         "\n  bb " (or file "<runbook>") " [options]"
         "\n\n"
         (stdout/yellow "Options:")
         "\n"
         (cli/format-opts (merge spec {:order (vec (keys (:spec spec)))})))))

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
  ["connected to a suitable cluster"
   #(let [current (kubectl/current-cluster)
          known (config/known-clusters)
          declared (config/env-value {:kubernetes-cluster current} [:capabilities])
          missing (when declared (remove (set declared) required-capabilities))]
      (cond
        (str/blank? (str current))
        {:ok? false :label "not connected to any kubernetes cluster"}

        (and (seq known) (not (some #{current} known)))
        {:ok? false :label (format "current cluster %s is none you configured: %s"
                                   current (str/join ", " known))}

        (seq missing)
        {:ok? false :label (format "cluster %s does not advertise: %s"
                                   current (str/join ", " (map name missing)))}

        :else
        {:ok? true :label (if (seq required-capabilities)
                            (format "cluster: %s (has %s)" current (str/join ", " (map name required-capabilities)))
                            (format "cluster: %s" current))}))])

(defn- can-i-item
  [verb resource]
  [(format "can %s %s" verb resource)
   #(if (kubectl/can-i? verb resource)
      {:ok? true :label (format "can %s %s" verb resource)}
      {:ok? false :label (format "cannot %s %s" verb resource)})])

(defn- can-exec-item
  [resource]
  [(format "can exec into %s" resource)
   #(if (kubectl/can-exec? resource)
      {:ok? true :label (format "can exec into %s" resource)}
      {:ok? false :label (format "cannot exec into %s" resource)})])

(defn- prerequisite-items
  [prerequisites opts]
  (let [blank? #(str/blank? (str %))
        ipv4-hosts (remove blank? (map opts (:can-ping-using-ipv4 prerequisites)))
        ipv6-hosts (remove blank? (map opts (:can-ping-using-ipv6 prerequisites)))
        secrets (remove blank? (map opts (:access-gopass-secrets prerequisites)))
        tools (utils/conj-if-not-empty (:installed-tools prerequisites) secrets (secrets/command-name))]
    (concat
     (map tool-item tools)
     (map min-version-item (:minimum-tool-versions prerequisites))
     (map (partial ping-item 4) ipv4-hosts)
     (map (partial ping-item 6) ipv6-hosts)
     (map secret-item secrets)
     (when (contains? prerequisites :cluster-capabilities)
       [(cluster-item (:cluster-capabilities prerequisites))])
     (map (partial can-i-item "get") (:kubectl-can-get prerequisites))
     (map (partial can-i-item "create") (:kubectl-can-create prerequisites))
     (map (partial can-i-item "patch") (:kubectl-can-patch prerequisites))
     (map (partial can-i-item "delete") (:kubectl-can-delete prerequisites))
     (map can-exec-item (:kubectl-can-exec prerequisites)))))

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

(defn execute!
  "Runs a runbook described by a config map:

   :description   one-line summary, shown by --help
   :options       babashka.cli option spec (standard flags are added)
   :prerequisites tooling/connection/permission checks to run first
   :announce?     whether the action posts to the changelog (default true)
   :action        (fn [opts ctx]) -> truthy on success

   ctx contains :announcement-data when :announce? is true."
  [{:keys [description options prerequisites announce? action]
    :or {options {} prerequisites {} announce? true}}]
  (let [spec {:spec (merge options standard-options)
              :restrict true
              :error-fn (fn [{:keys [type cause msg option]}]
                          (when (= :org.babashka/cli type)
                            (case cause
                              :require (stdout/error (format "missing required option: --%s" (name option)))
                              :validate (stdout/error (format "invalid value for option --%s: %s" (name option) msg))
                              (stdout/error msg))
                            (System/exit 1)))}]
    ;; help is answered before the strict parse, so required options never
    ;; stand between the user and the usage text
    (if (some #{"-h" "--help"} *command-line-args*)
      (println (help-text description spec))
      (let [opts (cli/parse-opts *command-line-args* spec)]
        (when (:no-color opts) (stdout/disable-colors!))
        ;; activate the cloned extensions (their code onto the classpath) and load
        ;; every plugin - the flat :plugins plus the ones extensions declare - so
        ;; all seams are registered before anything in this run reaches for them
        (plugins/load! (concat (config/value [:plugins] []) (extensions/activate!)))
        ;; select the output theme from env.edn (or a plugin theme just loaded)
        ;; before anything prints
        (theme/apply!)
        (when-not (prerequisites? prerequisites opts)
          (stdout/print-banner stdout/red (str (theme/token :error) " PREREQUISITES NOT MET - nothing was attempted"))
          (stdout/err-println "Next: install the missing tools, connect to the right cluster/VPN, or fix the")
          (stdout/err-println (str "      access shown with a " (theme/token :check-error) " above, then run this book again."))
          (System/exit 1))
        (let [ctx (when announce? {:announcement-data (announcement-data)})]
          (stdout/print-section "🚀 Runbook")
          ;; an unexpected exception must still land on the red banner with a
          ;; clean message, never a raw stack trace - the exit code stays 1 so
          ;; scripts and ./detect see the failure
          (let [outcome (try
                          (binding [flow/*dry-run* (boolean (:dry-run opts))]
                            (if (action opts ctx) :ok :failed))
                          (catch Exception e
                            (stdout/error "unexpected error:" (or (ex-message e) (str e)))
                            :error))
                ;; every run leaves an audit trail: input vars, selections, commands
                recording-path (recording/save! {:book-file (System/getProperty "babashka.file")
                                                 :description description
                                                 :opts opts
                                                 :outcome outcome
                                                 :meta (:announcement-data ctx)})]
            ;; the final banner leaves no doubt about the outcome, even at 3am
            (if (= :ok outcome)
              (stdout/print-banner stdout/green (str (theme/token :ok) " DONE - everything verified"))
              (stdout/print-banner stdout/red (str (theme/token :error) " FAILED or ABORTED - read the messages above, nothing more was changed")))
            ;; run summary: where the record is, how to reproduce, how to dig in together
            (when recording-path
              (stdout/err-println (format "📼 Recording: %s" recording-path)))
            (when-not announce?
              ;; the exact CLI calls behind a read-only book's output are safe to replay
              (reproducer/offer! description)
              ;; the findings themselves, as Markdown, for a shared pad or a ticket
              (summary/offer! description)
              ;; something turned up - offer a shared terminal to investigate together
              (when (= :failed outcome)
                (upterm/offer!)))
            (System/exit (if (= :ok outcome) 0 1))))))))
