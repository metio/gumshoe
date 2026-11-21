;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.outcome
  "What a run actually did, so the final banner asserts only that.

   The places where things happen record them as they happen: the confirmation
   records an abort, the effect interpreter records each applied effect and the
   one it stopped at, the post-check records whether verification ran and
   held, and a legacy :execute! thunk records that it ran. The harness binds a
   fresh record per run and derives the banner from it with `banner`, a pure
   function of the record, the book's kind and its result.

   Outside a run nothing is bound and recording does nothing, so a library
   function called from a test or a REPL behaves exactly as before."
  (:require [clojure.string :as str]))

(def ^:dynamic *record*
  "The atom collecting the facts of the current run, or nil outside one."
  nil)

(defn fresh
  "An empty record for one run."
  []
  (atom {:applied 0}))

(defn- record!
  [f & args]
  (when *record* (apply swap! *record* f args)))

(defn aborted!
  "The operator declined the confirmation, or a precondition no longer held
   after it, so nothing was attempted."
  [reason]
  (record! assoc :aborted reason))

(defn unmet!
  "A rehearsed step whose precondition does not hold against the current
   state; `step` names it."
  [step]
  (record! update :unmet (fnil conj []) step))

(defn paused!
  "A playbook stopped at a pause it can not wait at; `how` says how to continue."
  [how]
  (record! assoc :paused how))

(defn applied!
  "One effect of a plan ran cleanly."
  []
  (record! update :applied inc))

(defn stopped!
  "An effect failed; `where` describes it. The plan stops there."
  [where]
  (record! assoc :stopped where))

(defn thunk-ran!
  "A legacy :execute! thunk ran - it changes things we can not count."
  [ok?]
  (record! assoc :thunk (if ok? :ok :failed)))

(defn verified!
  "A post-check ran; every post-check of the run must hold for the run to
   count as verified."
  [held?]
  (record! update :verified #(if (false? %) false (boolean held?))))

(defn exit-code
  "The exit code of a run: 0 only for :ok, and a rehearsal that found an unmet
   precondition exits 1 so a script sees that the real run would stop."
  [outcome record]
  (if (and (= :ok outcome) (empty? (:unmet record))) 0 1))

(defn- changed?
  [{:keys [applied thunk]}]
  (or (pos? applied) (some? thunk)))

(defn- effects
  [n]
  (if (= 1 n) "1 effect" (str n " effects")))

(defn banner
  "The final banner for a run, as {:tone :ok|:neutral|:error :text}.

   :dry-run?  the run was a rehearsal of a change
   :changes?  the book changes things (it announces) rather than only reading
   :outcome   :ok, :failed, or :error (an unexpected exception)
   :findings? a read-only book captured findings for its report
   record     the facts recorded during the run (see the recording functions)

   Only a run whose post-checks all ran and held may claim verification."
  [{:keys [dry-run? changes? outcome findings?]} record]
  (let [{:keys [aborted applied paused stopped thunk verified unmet]} record]
    (cond
      (and dry-run? changes? (seq unmet))
      {:tone :error
       :text (str "DRY RUN - nothing was changed; the precondition of "
                  (str/join ", " unmet)
                  " does not hold against the current state")}

      ;; a read-only book reads and reports under --dry-run too
      (and dry-run? changes?)
      {:tone :neutral :text "DRY RUN - nothing was changed, this was a rehearsal"}

      (= :error outcome)
      {:tone :error
       :text (str "FAILED - an unexpected error stopped the run"
                  (when (changed? record)
                    (str "; " (effects applied) " had been applied before it"))
                  " - read the messages above")}

      aborted
      {:tone :neutral :text (str "ABORTED - " aborted " - nothing was changed")}

      paused
      {:tone :neutral
       :text (str "PAUSED"
                  (when (changed? record) (str " after " (effects applied)))
                  " - " paused)}

      (not changes?)
      (cond
        (= :ok outcome) {:tone :ok :text "DONE - no findings"}
        findings? {:tone :neutral :text "FINDINGS - read the report above"}
        :else {:tone :error :text "FAILED - read the messages above"})

      stopped
      {:tone :error
       :text (if (pos? applied)
               (str "FAILED after " (effects applied) " - stopped at: " stopped
                    " - the effects before it stay applied")
               (str "FAILED at the first effect: " stopped " - nothing was changed"))}

      (false? verified)
      {:tone :error
       :text (str "NOT VERIFIED - the change ran, but its post-check did not hold"
                  " - read the messages above")}

      (= :failed thunk)
      {:tone :error :text "FAILED - the change reported a failure - read the messages above"}

      (= :failed outcome)
      {:tone :error
       :text (str "FAILED - read the messages above"
                  (if (changed? record)
                    (str "; " (effects applied) " had been applied")
                    "; nothing was changed"))}

      (and (true? verified) (changed? record))
      {:tone :ok :text "DONE - applied and verified"}

      (changed? record)
      {:tone :ok :text "DONE - applied; this book has no post-checks"}

      (true? verified)
      {:tone :ok :text "DONE - verified, nothing needed changing"}

      :else
      {:tone :ok :text "DONE - nothing was changed"})))
