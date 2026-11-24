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

;; ---------------------------------------------------------------------------
;; custom effect verbs - a plugin seam

(defonce ^:private effect-types (atom {}))

(defn register-effect-type!
  "Registers a custom effect verb, so a tool package can add an action to the DSL
   and have it flow through the same run!/dry-run/confirmation machinery as the
   built-ins. `op` is the keyword an effect starts with ([op & args]); handlers is
   {:describe (fn [args] -> one-line string) :perform (fn [args] -> true on
   success)}. Both are required: the :describe is what --dry-run and the change
   preview show, so a custom effect can not act without first being able to say
   what it will do."
  [op {:keys [describe perform] :as handlers}]
  {:pre [(keyword? op) (fn? describe) (fn? perform)]}
  (swap! effect-types assoc op handlers))

;; ---------------------------------------------------------------------------
;; describing (for dry-run and logs) - pure

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
      (render args)
      (pr-str (into [op] args)))))

;; ---------------------------------------------------------------------------
;; interpreters

(defn- perform!
  "Runs one effect, streaming output. Returns true on a clean result."
  [[op & args]]
  (case op
    :kubectl (let [[context & kargs] args]
               (zero? (apply shell/run-with-output "kubectl" (str "--context=" context) kargs)))
    :kubectl-plugin (let [[context plugin & pargs] args]
                      (zero? (apply shell/run-with-output "kubectl" (kubectl-plugin-args context plugin pargs))))
    :kubectl-stdin (let [[context stdin & kargs] args
                         result (apply shell/execute-with-stdin stdin
                                       "kubectl" (str "--context=" context) kargs)]
                     (when-not (str/blank? (:out result)) (println (:out result)))
                     (if (zero? (:exit result))
                       true
                       (do (stdout/error (:err result)) false)))
    ;; assembled by ssh/ssh-args rather than spelled again here, so an option
    ;; added for the read-only path cannot go missing on the mutating one
    :ssh (let [[connection & command] args]
           (zero? (apply shell/run-with-output (ssh/ssh-args connection command))))
    :cmd (zero? (apply shell/run-with-output args))
    :note (do (stdout/err-println (str "  " (first args))) true)
    (if-let [act (:perform (get @effect-types op))]
      (boolean (act args))
      (do (stdout/error "unknown effect:" (pr-str op)) false))))

(defn run!
  "Runs the whole plan in order, stopping at the first failure. Returns true
   only when every effect succeeded."
  [plan]
  (reduce (fn [_ effect]
            (if (perform! effect)
              (do (outcome/applied!) true)
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
