;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.hooks
  "Execution hooks - a plugin seam with two ends.

   POST hooks fire after every book finishes, with the run's outcome, so a plugin
   can push a metric, forward the recording to an audit store, update a status
   page, or trigger a follow-up. A post-hook is a pure observer: it never changes
   the outcome; a throwing one is warned about, and each is time-bounded so a slow
   one can never block the book's exit.

   PRE hooks fire before every book runs and can VETO it - a global gate for
   organisation policy that applies to every book without each declaring it (a
   change freeze that blocks changes, a required acknowledgement). A pre-hook
   returns false or {:allow? false :reason \"...\"} to block. Pre-hooks fail OPEN:
   a throwing or slow one warns and allows, so a broken gate can never block
   emergency response during an incident.

   Both differ from announcers, which fire on the operator's final confirmation
   ('this is starting now', for alert correlation), and from prerequisite checks,
   which a single book declares for itself - a pre-hook applies to every book."
  (:require [gumshoe.stdout :as stdout]))

(defonce ^:private post-hooks (atom []))
(defonce ^:private pre-hooks (atom []))

(def ^:private hook-timeout-ms
  "A post-hook runs after the book is already done, so a hang would only delay the
   exit; a pre-hook gates the start, so a hang delays it - bound both and move on."
  30000)

(defn register-post-hook!
  "Registers a function called after every book with the run context, a map:
     {:description  the book's one-line description
      :book         the book file that ran
      :opts         the parsed flags
      :outcome      :ok | :failed | :error
      :recording-path  where the audit trail was written (or nil)
      :meta         the announcement-data for a change (nil for a read-only book)}"
  [f]
  (swap! post-hooks conj f))

(defn registered-count
  "How many post-hooks are registered - for tests and diagnostics."
  []
  (count @post-hooks))

(defn run-post-hooks!
  "Runs every registered post-hook with the context. Best-effort and bounded: a
   hook that throws is warned about; one that exceeds the timeout is abandoned so
   it can never block the book's exit. Never throws."
  [context]
  (doseq [hook @post-hooks]
    ;; Catch Throwable, not just Exception: a hook that trips an assert throws
    ;; AssertionError (an Error), which would otherwise escape the future and
    ;; break the documented never-throw guarantee.
    (let [result (deref (future (try (hook context) nil (catch Throwable e (or (ex-message e) (str e)))))
                        hook-timeout-ms
                        ::timeout)]
      (cond
        (= ::timeout result) (stdout/warn "post-hook timed out - abandoned")
        (some? result) (stdout/warn "post-hook failed:" result)))))

(defn register-pre-hook!
  "Registers a function called before every book with the run context, a map:
     {:description  the book's one-line description
      :book         the book file about to run
      :opts         the parsed flags
      :change?      true for a book that makes a change, false for a read-only one}
   Return anything truthy to allow the run, or false / {:allow? false :reason
   \"...\"} to VETO it - the book stops before touching anything."
  [f]
  (swap! pre-hooks conj f))

(defn- vetoed?
  [result]
  (or (false? result) (and (map? result) (false? (:allow? result)))))

(defn run-pre-hooks!
  "Runs the pre-hooks in order and returns {:allowed? bool :reason ...}. The first
   veto stops the book. Bounded and fail-open: a throwing or timed-out hook warns
   and allows, so a broken gate can never block emergency response."
  [context]
  (loop [remaining @pre-hooks]
    (if-let [hook (first remaining)]
      ;; Catch Throwable so a hook that trips an assert (AssertionError, an Error)
      ;; fails open rather than crashing the run - the whole point of the gate.
      (let [result (deref (future (try {:value (hook context)}
                                       (catch Throwable e {:error (or (ex-message e) (str e))})))
                          hook-timeout-ms
                          ::timeout)]
        (cond
          (= ::timeout result) (do (stdout/warn "pre-hook timed out - allowing") (recur (rest remaining)))
          (:error result) (do (stdout/warn "pre-hook failed - allowing:" (:error result)) (recur (rest remaining)))
          (vetoed? (:value result)) {:allowed? false
                                     :reason (or (:reason (:value result))
                                                 "a pre-execution hook vetoed this run")}
          :else (recur (rest remaining))))
      {:allowed? true})))
