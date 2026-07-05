;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.hooks
  "Post-execution hooks - a plugin seam. After every book finishes, gumshoe calls
   each registered hook with the run's outcome, so a plugin can push a metric,
   forward the recording to an audit store, update a status page, or trigger a
   follow-up.

   This is deliberately distinct from announcers. An announcer fires BEFORE a
   change - on the operator's final confirmation, 'this is starting now' - so
   whoever sees the alerts that follow can correlate them to the change. A hook
   fires AFTER, with the result (did it succeed, where's the recording). A hook
   is a pure observer: it never changes the run's outcome. A throwing hook is
   warned about, and each hook is time-bounded, so a slow or wedged one can never
   block the book's exit."
  (:require [gumshoe.stdout :as stdout]))

(defonce ^:private post-hooks (atom []))

(def ^:private hook-timeout-ms
  "A hook runs after the book is already done, so a hang would only delay the
   exit - bound it and move on."
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
    (let [result (deref (future (try (hook context) nil (catch Exception e (or (ex-message e) (str e)))))
                        hook-timeout-ms
                        ::timeout)]
      (cond
        (= ::timeout result) (stdout/warn "post-hook timed out - abandoned")
        (some? result) (stdout/warn "post-hook failed:" result)))))
