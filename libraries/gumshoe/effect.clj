;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.effect
  "Effects as data. A book that changes something describes the change as a
   plain plan - a vector of effect vectors - instead of doing it directly.
   That one move multiplies: the same plan can be run, or shown (--dry-run),
   or collected and asserted in a test without a cluster. Plans compose by
   concatenation, so a book builds a big change out of small, named pieces.

   An effect is [:op & args]. A plan is a seq of effects; a change handed one
   effect where a plan belongs runs it as a plan of one (see as-plan).
   Constructors below keep the data tidy; interpreters at the bottom give it
   meaning."
  (:require [clojure.string :as str]
            [gumshoe.child-output :as child-output]
            [gumshoe.offline :as offline]
            [gumshoe.outcome :as outcome]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]))

;; ---------------------------------------------------------------------------
;; constructors

(defn kubectl
  "A kubectl mutation against a context, e.g. (kubectl ctx \"cordon\" node)."
  [context & args]
  (into [:kubectl context] (vec args)))

(defn kubectl-plugin
  "A kubectl plugin command, e.g. (kubectl-plugin ctx \"cnpg\" \"promote\" ...).
   kubectl refuses any flag before a plugin's name and hands everything after
   it to the plugin, so the context goes last and the plugin must accept
   --context itself."
  [context plugin & args]
  (into [:kubectl-plugin context plugin] (vec args)))

(defn kubectl-plugin-args
  "The kubectl arguments a :kubectl-plugin effect runs."
  [context plugin args]
  (into [plugin] (concat args [(str "--context=" context)])))

(defn kubectl-stdin
  "A kubectl command fed a string on stdin (apply/replace from a manifest)."
  [context stdin & args]
  (into [:kubectl-stdin context stdin] (vec args)))

(defn ssh
  "A command run on a remote host over SSH (connection is an gumshoe.ssh map)."
  [connection & args]
  (into [:ssh connection] (vec args)))

(defn cmd
  "Any local command, e.g. (cmd \"flux\" \"reconcile\" ...) - for the tools that
   are not kubectl."
  [& args]
  (into [:cmd] (vec args)))

(defn note
  "A human message, carried in the plan so dry-run and reproductions read well."
  [text]
  [:note text])

(defn plan
  "Collects effects (and nils, which are dropped) into one plan, so a book can
   compose optional pieces: (plan a (when x b) c)."
  [& effects]
  (vec (remove nil? effects)))

(defn effect?
  "Whether x is one effect: a vector whose first element is a keyword, the
   shape every constructor above returns. A plan's elements are vectors, so a
   plan never starts with a keyword and the two cannot be mistaken."
  [x]
  (and (vector? x) (keyword? (first x))))

(defn as-plan
  "x as a plan: a plan as it is, one effect as a plan of one, and nil for
   anything else - a keyword, a map, a string, or a sequence holding anything
   that is not an effect. Every element is checked before any runs, so a
   malformed plan is refused whole instead of failing halfway through."
  [x]
  (cond
    (effect? x) [x]
    (and (sequential? x) (every? effect? x)) x
    :else nil))

(defn interactive
  "effect, marked as one that needs the terminal for itself: an interactive
   shell, a session the operator types into. It runs with stdin, stdout and
   stderr left on the terminal, so its prompt and its own rendering work, and
   its warnings are not shown under its line. Every other effect relays
   stderr and shows the warnings its child wrote under the effect's line."
  [effect]
  (vary-meta effect assoc ::interactive true))

(defn interactive?
  "Whether effect was marked with interactive."
  [effect]
  (boolean (::interactive (meta effect))))

;; ---------------------------------------------------------------------------
;; custom effect verbs - a plugin seam

(defonce ^:private effect-types (atom {}))

(def ^:private built-in-ops
  "The ops perform! interprets itself."
  #{:kubectl :kubectl-plugin :kubectl-stdin :ssh :cmd :note})

(defn- refuse-type!
  [op message]
  (throw (ex-info (format "the effect type %s %s" (pr-str op) message) {:op op})))

(defn register-effect-type!
  "Registers a custom effect verb, so a tool package can add an action to the DSL
   and have it flow through the same run!/dry-run/confirmation machinery as the
   built-ins. `op` is the keyword an effect starts with ([op & args]); handlers is

     {:describe    (fn [args] -> one-line string)   required
      :perform     (fn [args] -> true on success)   required
      :args        [\"dir\" \"message\" ..]          one name per argument
      :destructive? false                           whether it destroys
      :touches     (fn [args] -> refs or nil)        the cluster objects it acts on
      :doc         \"what it does\"}

   :describe and :perform are required: the :describe is what --dry-run and
   the change preview show, so a custom effect can not act without first
   being able to say what it will do. The :describe renders from its
   arguments alone; it runs inside gumshoe.offline/forbidding, so one that
   starts a command or sends a request throws instead of reading.

   The other keys are optional. :args names the positional arguments, and a
   gumshoe.dsl change can name only a type that declares them, so compile
   can refuse a wrong count. A type is destructive unless it says
   :destructive? false: only its own :perform knows what it does, so a
   type that does not say asks for the typed confirmation a delete asks
   for. :touches names the {:type :namespace :name} refs of the cluster
   objects an effect acts on, [] for one that acts on none (a file, a
   commit), so gumshoe.testing.plan can compare them with the
   confirmation. A built-in op cannot be registered: perform! interprets
   it itself and would never call the handlers."
  [op {:keys [describe perform args destructive? touches doc] :as handlers}]
  {:pre [(keyword? op) (fn? describe) (fn? perform)]}
  (when (contains? built-in-ops op)
    (refuse-type! op "is built in, and its handlers would never run"))
  (when (and (contains? handlers :args)
             (not (and (vector? args) (every? #(and (string? %) (not (str/blank? %))) args))))
    (refuse-type! op (format "names its :args as a vector of non-blank strings, not %s" (pr-str args))))
  (when (and (contains? handlers :destructive?) (not (boolean? destructive?)))
    (refuse-type! op (format "says :destructive? true or false, not %s" (pr-str destructive?))))
  (when (and (contains? handlers :touches) (not (fn? touches)))
    (refuse-type! op (format "names what it touches as a function of its arguments, not %s" (pr-str touches))))
  (when (and (contains? handlers :doc) (not (and (string? doc) (not (str/blank? doc)))))
    (refuse-type! op (format "has a non-blank :doc, not %s" (pr-str doc))))
  (swap! effect-types assoc op handlers))

(defn effect-type
  "What op was registered with, as {:describe :perform :args :destructive?
   :touches :doc}, :destructive? true where the registration did not say;
   nil for a built-in op or one nobody registered."
  [op]
  (when-let [handlers (get @effect-types op)]
    (update handlers :destructive? #(if (false? %) false true))))

(defn registered-types
  "Every registered effect type, by op, as effect-type returns it."
  []
  (into {} (map (fn [op] [op (effect-type op)])) (keys @effect-types)))

(defn built-in-op?
  "Whether op is one perform! interprets itself."
  [op]
  (contains? built-in-ops op))

;; ---------------------------------------------------------------------------
;; describing (for dry-run and logs) - pure

(defn known-op?
  "Whether an interpreter knows op: a built-in, or one a plugin or book
   registered with register-effect-type!."
  [op]
  (or (contains? built-in-ops op) (contains? @effect-types op)))

(defn unknown-ops
  "The distinct ops of plan no interpreter knows, in plan order. run! stops at
   the first such effect after applying the ones before it, so a change
   refuses a plan holding one before anything of it runs."
  [plan]
  (into [] (comp (map first) (remove known-op?) (distinct)) plan))

(defn describe
  "A one-line, human-readable form of an effect - never runs anything."
  [[op & args]]
  (case op
    :kubectl (str "kubectl " (str/join " " (rest args)))
    :kubectl-stdin (str "kubectl " (str/join " " (drop 2 args)) " (with stdin)")
    :kubectl-plugin (let [[context plugin & pargs] args]
                      (str "kubectl " (str/join " " (kubectl-plugin-args context plugin pargs))))
    ;; Render the same user@host target perform! connects to, so a --dry-run
    ;; preview an operator copies runs as the same identity, not their default.
    :ssh (let [[connection & command] args]
           (str "ssh " (ssh/target connection) " -- " (str/join " " command)))
    :cmd (str/join " " args)
    :note (str "# " (first args))
    (if-let [render (:describe (get @effect-types op))]
      (offline/forbidding (format "the :describe of the effect type %s renders what the effect will do from its arguments alone" (pr-str op))
                          #(render args))
      (pr-str (into [op] args)))))

;; ---------------------------------------------------------------------------
;; interpreters

(defn- stream-fn
  "The shell function an effect's child runs through: run-with-output for an
   interactive effect, which keeps the terminal, run-attributed otherwise."
  [effect]
  (if (interactive? effect) shell/run-with-output shell/run-attributed))

(defn- kubectl-stdin-failure!
  "Prints a failed kubectl-stdin effect's stderr in full: its warnings are
   collected for the effect's line like any other child's, so what is printed
   here is everything else, or the exit code when nothing else was written."
  [{:keys [exit err]}]
  (let [other (:other (child-output/split-stderr err))]
    (stdout/error (if (seq other)
                    (str/join "\n" other)
                    (format "kubectl exited %s" exit)))))

(defn- perform!
  "Runs one effect, streaming output. Returns true on a clean result."
  [[op & args :as effect]]
  (let [run (stream-fn effect)]
    (case op
      :kubectl (let [[context & kargs] args]
                 (zero? (apply run "kubectl" (str "--context=" context) kargs)))
      :kubectl-plugin (let [[context plugin & pargs] args]
                        (zero? (apply run "kubectl" (kubectl-plugin-args context plugin pargs))))
      :kubectl-stdin (let [[context stdin & kargs] args
                           result (apply shell/execute-with-stdin stdin
                                         "kubectl" (str "--context=" context) kargs)]
                       (when-not (str/blank? (:out result)) (println (:out result)))
                       (if (zero? (:exit result))
                         true
                         (do (kubectl-stdin-failure! result) false)))
      ;; assembled by ssh/ssh-args rather than spelled again here, so an option
      ;; added for the read-only path cannot go missing on the mutating one
      :ssh (let [[connection & command] args]
             (zero? (apply run (ssh/ssh-args connection command))))
      :cmd (zero? (apply run args))
      :note (do (stdout/err-println (str "  " (first args))) true)
      (if-let [act (:perform (get @effect-types op))]
        (boolean (act args))
        (do (stdout/error "unknown effect:" (pr-str op)) false)))))

(defn changes-state?
  "Whether running effect may change anything: every effect but a :note,
   which only prints its text. A registered effect type counts as a change,
   since only its own :perform knows what it does."
  [[op]]
  (not= :note op))

(defn- perform-attributed!
  "Runs one effect under its own line: the line names the effect before it
   runs, so its live output reads as its own, and the warnings its children
   wrote follow once it is done, a failed effect's included. A note is only
   its text, and an interactive effect, whose terminal stays its own, is
   named and left alone."
  [effect]
  (if-not (changes-state? effect)
    (perform! effect)
    (do (stdout/item :info (describe effect))
        (let [{:keys [value warnings]} (shell/collecting-warnings #(perform! effect))]
          (stdout/child-warnings warnings)
          value))))

(defn run!
  "Runs the whole plan in order, stopping at the first failure. Returns true
   only when every effect succeeded. Only effects that may change something
   count as applied, so a plan of notes reads as nothing changed."
  [plan]
  (reduce (fn [_ effect]
            (if (perform-attributed! effect)
              (do (when (changes-state? effect) (outcome/applied!)) true)
              (do (outcome/stopped! (describe effect)) (reduced false))))
          true
          plan))

(defn dry-run
  "Shows what the plan would do, changing nothing. Always returns true."
  [plan]
  (doseq [effect plan]
    (stdout/err-println (str "  " (stdout/blue "would run:") " " (describe effect))))
  true)

(defn collect
  "The test interpreter: the plan itself, so a test can assert on the exact
   effects a book would emit - no cluster, no mocking."
  [plan]
  (vec plan))
