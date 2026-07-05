;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.shell
  "Thin wrappers around babashka.process for the common execution styles.

   These never throw. babashka.process throws when the binary does not exist
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
   them."
  (:require [babashka.process :as process]
            [clojure.string :as str]))

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

(defn- record!
  [args]
  (swap! recorded conj (str/join " " (map str (flatten args)))))

(defn- capture
  "Runs a command capturing its output, bounded by *timeout-ms*. On expiry the
   whole process tree is destroyed and a timeout result is returned, so a
   caller waiting on data always gets an answer. A nil bound runs unbounded."
  [opts args]
  (record! args)
  (try
    (let [proc (apply process/process opts args)]
      (if-let [ms *timeout-ms*]
        (let [result (deref (future @proc) ms ::timeout)]
          (if (= ::timeout result)
            (do (process/destroy-tree proc)
                (timeout-failure args ms))
            result))
        @proc))
    (catch Exception e
      (launch-failure args e))))

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
    (apply process/shell opts args)
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
                                           {:out :string :err :inherit :in :inherit :continue true}
                                           args)))))
    (catch Exception _ nil)))

(defn pipe-to!
  "Feeds input to a command's stdin without capturing its output, bounded by a
   short deadline. Clipboard tools like wl-copy daemonize and would hold a
   captured pipe open forever, so their output is inherited, never captured,
   and a hung tool is destroyed rather than freezing the caller. Returns the
   exit code (non-zero on any trouble); this is best-effort plumbing and never
   throws."
  [input & args]
  (try
    (let [proc (apply process/process
                      {:in input :out :inherit :err :inherit :continue true}
                      args)
          result (deref (future @proc) 3000 ::timeout)]
      (if (= ::timeout result)
        (do (process/destroy-tree proc) 1)
        (:exit result)))
    (catch Exception _ 1)))
