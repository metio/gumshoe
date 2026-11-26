;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.shell
  "Thin wrappers around babashka.process for the common execution styles.

   These never throw, except on a cancelled thread (see capture). babashka.process throws when the binary does not exist
   or can not be launched (a missing tool, a permission error) - even with
   :continue true, which only suppresses non-zero exits. A book must be able
   to survive that: a failed launch becomes a result map with a non-zero exit
   and the reason in :err, so every caller's exit-code and empty-output
   handling keeps working instead of crashing with a stack trace.

   A book must also always return. A captured command - a probe gathering data
   to report - is bounded by *timeout-ms*: if the tool or the network wedges it
   is killed and surfaced as a non-zero exit, never an endless hang. Streaming
   commands (run-with-output) are exempt: those are the deliberately
   long-running, interactive foreground operations - a port-forward, an ssh
   session, a shared upterm terminal - and must run until the operator ends
   them.

   Every child starts with child-env on top of our own environment, so
   kubectl never reads the operator's kuberc: a plan the operator confirmed is
   the command that runs. Spawning through babashka.process anywhere else would
   skip that; spawn through this namespace (start for a child the caller
   manages itself)."
  (:require [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def child-env
  "Environment every child process gets on top of ours, winning over the
   operator's own values and over a caller's extra environment.

   KUBERC=off makes kubectl skip both the file KUBERC names and
   ~/.kube/kuberc (kubectl's pkg/kuberc LoadKuberc, from kubectl 1.33). A
   kuberc can alias commands and default flags such as delete's
   --interactive, --force or --grace-period=0, so with it the command a book
   printed and the operator confirmed is not the command kubectl runs, and an
   interactive default makes an unattended delete hang on a prompt. kubectl
   plugins (kubectl-cnpg), and kubectl started by another tool or a shell,
   inherit it. An older kubectl has no kuberc and ignores the variable. There
   is no opt-out: a book's command has to mean the same on every machine."
  {"KUBERC" "off"})

(defn- child-opts
  "opts with child-env merged over any :extra-env the caller gave."
  [opts]
  (update opts :extra-env merge child-env))

(def ^:dynamic *timeout-ms*
  "Upper bound in milliseconds on any single captured command. The default is
   generous so ordinary queries never cancel early; it exists to guarantee a
   wedged tool or a dead network can not hang a book forever. Bind it tighter
   around a probe that should fail fast (a reachability ping), or to nil to run
   a genuinely long captured command unbounded."
  180000)

;; 127 is the shell's own convention for 'command not found'.
(defn- launch-failure
  [args exception]
  {:exit 127
   :out ""
   :err (format "could not run '%s': %s"
                (str/join " " (map str (flatten args)))
                (or (ex-message exception) (.getName (class exception))))})

;; 124 is coreutils timeout's convention for 'killed on deadline'.
(defn- timeout-failure
  [args ms]
  {:exit 124
   :out ""
   :err (format "'%s' did not finish within %dms and was stopped"
                (str/join " " (map str (flatten args))) ms)})

;; Every external command a book runs is recorded here, so a book can offer a
;; reproducer: the exact CLI calls that produced what the operator just saw.
;; Secrets never travel in arguments (they go via the environment), so the
;; recorded command lines are safe to keep and replay.
(def ^:private recorded (atom []))

(defn recording
  "The command lines run so far this process, in order."
  []
  @recorded)

(def ^:private shell-safe
  "Characters a POSIX shell leaves alone. Anything else means the token has to be
   quoted before a shell sees it."
  #"[^A-Za-z0-9_@%+=:,./-]")

(defn quote-token
  "Quotes one token for a POSIX shell, so the shell reads it back as exactly one
   argument. Tokens that need no quoting are passed through, so a recorded
   reproducer stays readable."
  [token]
  (let [s (str token)]
    (if (or (empty? s) (re-find shell-safe s))
      (str "'" (str/replace s "'" "'\\''") "'")
      s)))

(defn replay-preamble
  "Shell lines that give a replayed recording the same child-env the run had,
   so a reproducer an operator runs by hand is not changed by their kuberc
   either. One export per variable, ahead of the commands."
  []
  (apply str (for [[k v] (sort child-env)] (str "export " k "=" (quote-token v) "\n"))))

(defn- record!
  "Records the command as a line sh replays with the same argv: a jsonpath
   template or a message carries spaces, quotes and globs."
  [args]
  (swap! recorded conj (str/join " " (map quote-token (flatten args)))))

(defonce ^:private live-children (atom #{}))

(defn- destroy-quietly
  [proc]
  (try (process/destroy-tree proc) (catch Throwable _ nil)))

(defonce ^:private reaper
  ;; SIGTERM and SIGHUP run shutdown hooks and then halt the JVM without
  ;; unwinding any thread, so a finally that tears a child down never runs.
  ;; One hook destroys every child still tracked at that moment.
  (delay (.addShutdownHook (Runtime/getRuntime)
                           (Thread. ^Runnable (fn [] (run! destroy-quietly @live-children))))))

(defn track-child!
  "Registers a child process to be destroyed, with its descendants, when the
   JVM shuts down on a signal before the code that started it could tear it
   down. Returns proc. Pair it with untrack-child! where the child is torn
   down normally."
  [proc]
  @reaper
  (swap! live-children conj proc)
  proc)

(defn untrack-child!
  "Forgets a child registered with track-child!, once it is torn down."
  [proc]
  (swap! live-children disj proc)
  nil)

(defn live-children-count
  "How many children are tracked right now."
  []
  (count @live-children))

(defn- refuse-when-interrupted!
  "Throws when the current thread was interrupted, keeping its interrupt flag,
   so a cancelled collector or post-check starts no further command."
  [args]
  (when (.isInterrupted (Thread/currentThread))
    (throw (InterruptedException.
            (format "not running '%s': the thread was cancelled"
                    (str/join " " (map str (flatten args))))))))

(defn- capture
  "Runs a command capturing its output, bounded by *timeout-ms*. On expiry the
   whole process tree is destroyed and a timeout result is returned, so a
   caller waiting on data always gets an answer. A nil bound runs unbounded.

   A thread interrupted while it waits - a collector or a post-check whose
   deadline cancelled it - destroys the process tree and rethrows the
   InterruptedException with the interrupt flag set, so the cancelled work
   stops instead of reading the interrupt as a failed launch and running its
   next command.

   Only a command that started is recorded: one refused on a cancelled
   thread, or one that could not be launched, contributed nothing to what the
   operator saw, and the reproducer would replay it as if it had."
  [opts args]
  (refuse-when-interrupted! args)
  (let [proc (try (apply process/process (child-opts opts) args)
                  (catch Exception e e))]
    (if (instance? Exception proc)
      (launch-failure args proc)
      (do
        (record! args)
        (track-child! proc)
        (try
          (if-let [ms *timeout-ms*]
            (let [result (deref (future @proc) ms ::timeout)]
              (if (= ::timeout result)
                (do (process/destroy-tree proc)
                    (timeout-failure args ms))
                result))
            @proc)
          (catch InterruptedException e
            (destroy-quietly proc)
            (.interrupt (Thread/currentThread))
            (throw e))
          (catch Exception e
            (destroy-quietly proc)
            (launch-failure args e))
          (finally
            (untrack-child! proc)))))))

(defn start
  "Starts a child the caller manages itself - streams it reads, a lifetime it
   ends - with child-env applied. Takes babashka.process's options and
   command; throws as babashka.process does when the program can not be
   launched, and records nothing."
  [opts & args]
  (apply process/process (child-opts opts) args))

(defn execute
  "Runs a command capturing stdout/stderr. Returns the full process map."
  [& args]
  (capture {:out :string :err :string :continue true} args))

(defn execute-with-stdin
  "Runs a command with the given string as stdin, capturing stdout/stderr."
  [input & args]
  (capture {:in input :out :string :err :string :continue true} args))

(defn execute-env
  "Like execute, with extra environment variables."
  [env & args]
  (capture {:out :string :err :string :continue true :extra-env env} args))

(defn stdout-of
  [& args]
  (-> (apply execute args) :out str str/trim))

(defn exit-code-of
  [& args]
  (:exit (apply execute args)))

(defn- safe-stream
  "Runs a streaming command without capturing its output. Never bounded: this
   is for the interactive, deliberately long-running foreground operations."
  [opts args]
  (record! args)
  (try
    (apply process/shell (child-opts opts) args)
    (catch Exception e
      (launch-failure args e))))

(defn- stream
  "Streams a command's output. A launch failure has no output channel of its
   own, so its reason is surfaced on stderr. Returns the exit code."
  [opts args]
  (let [result (safe-stream opts args)]
    (when (and (= 127 (:exit result)) (seq (:err result)))
      (binding [*out* *err*] (println (:err result))))
    (:exit result)))

(defn run-with-output
  "Runs a command streaming its output to the terminal. Returns the exit code.
   Never times out - use this for long-running foreground work (port-forwards,
   ssh sessions, shared terminals) that must run until the operator ends it."
  [& args]
  (stream {:continue true} args))

(defn run-with-output-env
  "Like run-with-output, with extra environment variables - for secrets that
   must not appear in the process arguments."
  [env & args]
  (stream {:continue true :extra-env env} args))

(defn capture-line
  "Runs an interactive prompt tool - gum, which renders its widget to stderr and
   writes the entered value to stdout - capturing only stdout so the value comes
   back while the widget and the keyboard stay on the terminal. Never bounded: a
   person is answering. Returns the trimmed value, or nil on any trouble."
  [& args]
  (record! args)
  (try
    (not-empty (str/trim (str (:out (apply process/shell
                                           (child-opts {:out :string :err :inherit :in :inherit :continue true})
                                           args)))))
    (catch Exception _ nil)))

(def ^:private discard
  "The null device, for a child whose output is of no interest."
  (io/file (if (str/includes? (str/lower-case (str (System/getProperty "os.name"))) "windows")
             "NUL" "/dev/null")))

(defn pipe-to!
  "Feeds input to a command's stdin, bounded by a short deadline, and discards
   whatever it writes. Clipboard tools daemonize: wl-copy forks a process that
   serves the selection until it is replaced, long after the parent exits. A
   captured pipe would hold that output forever, and an inherited one hands the
   daemon our own stdout - which keeps the write end of `gumshoe | tee` open
   after we are done, so the reader never sees EOF and the pipeline hangs. The
   null device belongs to neither. Returns the exit code (non-zero on any
   trouble); this is best-effort plumbing and never throws."
  [input & args]
  (try
    (let [proc (apply process/process
                      (child-opts {:in input :out :write :out-file discard
                                   :err :write :err-file discard :continue true})
                      args)
          result (deref (future @proc) 3000 ::timeout)]
      (if (= ::timeout result)
        (do (process/destroy-tree proc) 1)
        (:exit result)))
    (catch InterruptedException _
      (.interrupt (Thread/currentThread))
      1)
    (catch Exception _ 1)))
