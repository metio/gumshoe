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

   A step may carry recovery guidance: what its failure leaves behind and what
   the operator does about it. The step that stops a run records it with
   `recovery!`, and `recovery-lines` prints it under a failed banner. An
   engine that can say something generic about a failure without guidance -
   which of its effects stayed applied - records that with `fallback!`; it is
   printed only when no guidance was recorded with `recovery!`, so a step's
   or a playbook's own guidance always wins.

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

(defn refused!
  "A change was refused: `step` names it (nil when the refusal came from
   outside a change) and `messages` are the lines its refusal printed. The
   first refusal of a run is kept, because it is the one that stopped the
   run; a rehearsal that goes on past a refusal records the later ones
   nowhere."
  [step messages]
  (record! update :refused #(or % {:step step :messages (vec messages)})))

(defn unmet!
  "A rehearsed step whose precondition does not hold against the current
   state; `step` names it."
  [step]
  (record! update :unmet (fnil conj []) step))

(defn unrehearsed!
  "A step a rehearsal could not show, because its work is a legacy
   :execute! thunk rather than a plan; `step` names it. Nothing of it ran,
   and the rehearsal cannot say what a real run would do there."
  [step]
  (record! update :unrehearsed (fnil conj []) step))

(defn paused!
  "A playbook stopped at a pause it can not wait at; `how` says how to continue."
  [how]
  (record! assoc :paused how))

(defn skipped!
  "A change found already done was skipped: nothing was asked, applied or
   checked for it. `step` names it. A skipped step is reached and judged,
   and is told apart from a step the run never reached, which records
   nothing."
  [step]
  (record! update :skipped (fnil conj []) step))

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

(defn unverified!
  "Post-checks that did not hold; `descriptions` name them, in the order they
   ran. The banner of a run that is not verified names them, so a change
   with several post-checks says which one failed."
  [descriptions]
  (when (seq descriptions)
    (record! update :unverified (fnil into []) descriptions)))

(def ^:private named-unverified
  "How many failed post-checks a banner names before it counts the rest."
  5)

(defn unverified-phrase
  "How a banner or guidance names the post-checks that did not hold: \"a
   post-check did not hold: <description>\" for one, \"<n> post-checks did
   not hold: <a>; <b>\" for several, the first few named and the rest
   counted, and \"its post-check did not hold\" when none was named."
  [unverified]
  (let [names (vec unverified)
        shown (take named-unverified names)
        more (- (count names) (count shown))]
    (case (count names)
      0 "its post-check did not hold"
      1 (str "a post-check did not hold: " (first names))
      (str (count names) " post-checks did not hold: " (str/join "; " shown)
           (when (pos? more) (str "; and " more " more"))))))

(defn unconfirmed!
  "A :soft? post-check ran out its time without holding; `description` names
   it. The run still counts as successful, since the book declared the check
   best effort, but its banner may not claim verification."
  [description]
  (record! update :unconfirmed (fnil conj []) description))

(defn findings!
  "A report rendered findings: `counts` maps a severity to how many it
   reported, as {:critical 2 :warning 1}. The counts of several reports in
   one run add up. Recording findings never fails or passes a run by itself;
   it lets the verdict name a run that failed only because it reported a
   critical finding as having findings rather than as broken."
  [counts]
  (record! update :findings #(merge-with + (or % {}) (select-keys counts [:critical :warning :info]))))

(defn incomplete!
  "The run could not do its job: a source did not answer, nothing was
   checked, a check crashed, or the report never reached stdout. `reason`
   says which. It never fails a run by itself, since the action reports the
   failure through its result; it keeps a failed run that also reported
   findings from being bannered as findings, because a failure must never
   be passed off as a mere finding."
  [reason]
  (record! update :incomplete (fnil conj []) reason))

(defn changed-so-far?
  "Whether the current run has changed anything yet; false outside a run."
  []
  (boolean (when *record* (let [{:keys [applied thunk]} @*record*]
                            (or (pos? applied) (some? thunk))))))

(defn applied-count
  "The number of effects the current run has applied; 0 outside a run."
  []
  (if *record* (:applied @*record*) 0))

(defn current
  "The facts recorded so far in the current run, or nil outside one."
  []
  (some-> *record* deref))

(defn render
  "The text of `guidance`, a string or a function of `ctx`, as {:text ..},
   {:error <why it could not be rendered>}, or nil when there is none. A
   function runs now, so a read in it observes the current state."
  [guidance ctx]
  (try
    (let [text (if (fn? guidance) (guidance ctx) guidance)]
      (when-not (str/blank? (some-> text str))
        {:text (str text)}))
    (catch Throwable e
      {:error (or (ex-message e) (str e))})))

(defn recovery!
  "Records the recovery guidance of the step the run stopped at: a string, or
   a function of the step's context, which also carries :applied, the number
   of effects applied so far. The function runs now, at the failure, so a read
   in it observes the state the failure left. The first guidance recorded in a
   run is the one printed: the step that stopped the run records before the
   playbook around it does."
  [guidance ctx]
  (when (and *record* guidance (not (contains? @*record* :recovery)))
    (when-let [rendered (render guidance (assoc ctx :applied (:applied @*record*)))]
      (record! assoc :recovery rendered))))

(defn fallback!
  "Records generic guidance for the run's failure, as `recovery!` renders it,
   printed only when no guidance is recorded with `recovery!`, whether before
   or after this call. The first fallback recorded in a run is the one kept."
  [guidance ctx]
  (when (and *record* guidance (not (contains? @*record* :fallback)))
    (when-let [rendered (render guidance (assoc ctx :applied (:applied @*record*)))]
      (record! assoc :fallback rendered))))

(defn recorded-recovery
  "The recovery guidance and the fallback recorded so far in the current run,
   as rendered: {:recovery .. :fallback ..}, each absent when not recorded."
  []
  (some-> *record* deref (select-keys [:recovery :fallback])))

(defn settle-recovery!
  "Replaces the run's recovery guidance with `rendered`, already rendered as
   `render` returns it, or leaves it as it is when `rendered` is nil. An
   engine that composes the guidance of a step with its own - a playbook
   around the change that stopped it - records the composition this way,
   after the step recorded its part."
  [rendered]
  (when rendered (record! assoc :recovery rendered)))

(defn recovery-lines
  "The lines printed under a banner of `tone` for `record`: the recorded
   recovery guidance, else the recorded fallback, or a note that it could not
   be rendered. Only a failed banner carries them, so a run that recorded
   neither prints the banner alone."
  [tone {:keys [recovery fallback]}]
  (when (= :error tone)
    (let [recovery (or recovery fallback)]
      (cond
        (:error recovery)
        [(str "Recovery: the guidance for the step the run stopped at could not be rendered - " (:error recovery))]

        (:text recovery)
        (into ["Recovery:"] (map #(str "  " %) (str/split-lines (:text recovery))))))))

;; ---------------------------------------------------------------------------
;; the verdict: the banner and the exit code come from one judgement

(defn- changed?
  [{:keys [applied thunk]}]
  (or (pos? (or applied 0)) (some? thunk)))

(defn failed?
  "Pure: whether the record and the outcome make the run a failure: the action
   failed or threw, a rehearsal found an unmet precondition or a step it
   could not rehearse, an effect stopped, a post-check did not hold, a legacy
   thunk reported a failure, or a run aborted after it had changed something.
   A failure recorded anywhere in the run fails it, whatever the action
   returned, so no banner can call a run done that exits 1, and none can
   call it failed while it exits 0."
  [outcome {:keys [unmet unrehearsed stopped verified thunk aborted] :as record}]
  (boolean (or (not= :ok outcome)
               (seq unmet)
               (seq unrehearsed)
               (some? stopped)
               (false? verified)
               (= :failed thunk)
               (and aborted (changed? record)))))

(defn exit-code
  "The exit code of a run: 0 unless `failed?` holds. A rehearsal that found an
   unmet precondition exits 1 so a script sees that the real run would stop,
   and one that passed a step it could not rehearse exits 1 so it never
   reads as a plan that was shown in full. A :soft? post-check that ran out
   its time unconfirmed does not fail the run: the book declared it best
   effort, and the banner says it was not confirmed."
  [outcome record]
  (if (failed? outcome record) 1 0))

(defn critical-findings
  "The number of critical findings the run's reports recorded, 0 when none."
  [record]
  (or (get-in record [:findings :critical]) 0))

(defn findings-only?
  "Pure: whether a run did its job and failed only because its report named
   critical findings. It holds for a book that changes nothing whose action
   reported failure, whose reports recorded at least one critical finding,
   and whose record holds no other failure, no abort, no pause and nothing
   incomplete. An unexpected error, a step that stopped, an unmet
   precondition, an abort, a pause or any `incomplete!` reason wins over the
   findings, so a run that both found something and did not finish is a
   failure."
  [{:keys [changes? outcome]} record]
  (boolean (and (not changes?)
                (= :failed outcome)
                (not (failed? :ok record))
                (not (:aborted record))
                (not (:paused record))
                (empty? (:incomplete record))
                (pos? (critical-findings record)))))

(def exit-modes
  "The exit-code contracts a run can follow. :default exits 0 or 1, as
   `exit-code` judges. :detailed keeps 0 and 1 and exits 2 for a run that
   `findings-only?` holds for, so a caller tells a book that reported critical
   findings from one that could not do its job by the exit code alone."
  #{:default :detailed})

(def findings-exit
  "The exit code of a run that did its job and reported critical findings,
   under the :detailed contract. Every failure, and anything that ends a
   process unexpectedly, exits 1, so a crash never reads as findings."
  2)

(defn detailed-exit-code
  "The exit code of a run under `mode`: `exit-code` under :default (or nil),
   and under :detailed `findings-exit` where `findings-only?` holds for the
   run, `exit-code` otherwise. `run` is {:changes? .. :outcome ..}."
  [mode run record]
  (let [code (exit-code (:outcome run) record)]
    (if (and (= :detailed mode) (findings-only? run record))
      findings-exit
      code)))

(defn- effects
  [n]
  (if (= 1 n) "1 effect" (str n " effects")))

(def ^:private unknown-step
  "a step whose effects are unknown")

(defn- made
  "What the run changed, as a phrase for a failed banner, or nil when
   nothing ran: a legacy thunk changes things the record cannot count, so
   it is named rather than counted."
  [{:keys [applied thunk]}]
  (let [applied (or applied 0)]
    (cond
      (and (pos? applied) thunk) (str (effects applied) " had been applied and " unknown-step " had run")
      (pos? applied) (str (effects applied) " had been applied")
      thunk (str unknown-step " had run"))))

(defn- refused-phrase
  "Which step a run's refusal stopped, as a phrase for a banner, or nil
   when no refusal named a step: \"refused at \\\"<step>\\\"\", with the
   number of messages when there were several."
  [{:keys [step messages]}]
  (when step
    (str "refused at \"" step "\""
         (when (< 1 (count messages)) (str " (" (count messages) " problems)")))))

(defn- with-refusal
  "text followed by the refused-phrase of the record, after a separator."
  [text refused]
  (if-let [phrase (refused-phrase refused)]
    (str text "; " phrase)
    text))

(defn- skipped-suffix
  [skipped]
  (when (seq skipped)
    (str "; skipped as already done: " (str/join ", " skipped))))

(defn- unconfirmed-suffix
  [unconfirmed]
  (str ", NOT confirmed (best effort): " (str/join ", " unconfirmed)))

(defn banner
  "The final banner for a run, as {:tone :ok|:neutral|:error :text}.

   :dry-run?  the run was a rehearsal of a change
   :changes?  the run may change things (runbook/change-book?) rather than
              only reading
   :outcome   :ok, :failed, or :error (an unexpected exception)
   :findings? a read-only book captured findings for its report
   record     the facts recorded during the run (see the recording functions)

   A successful run names the changes it skipped as already done.

   A read-only run that failed only because its reports recorded critical
   findings (`findings-only?`) is bannered FINDINGS with their count; one
   that recorded an `incomplete!` reason is bannered FAILED naming it, even
   when it also reported findings.

   A failed or aborted run that a refusal stopped names the step it
   refused at (see refused!), and how many problems it named when there
   were several, so a playbook's banner says which step blocked it.

   Only a run whose post-checks all ran and held may claim verification; a
   :soft? check that ran out its time is named as not confirmed, and a run
   whose post-checks did not hold names the ones that failed.

   The tone agrees with `exit-code`: :ok only when it is 0, :error only
   when it is 1."
  [{:keys [dry-run? changes? outcome findings?]} record]
  (let [{:keys [aborted applied paused stopped thunk verified unverified unmet unrehearsed skipped unconfirmed refused]} record
        applied (or applied 0)
        failed (failed? outcome record)]
    (cond
      ;; a rehearsal that threw never showed the plans after the throw
      (and dry-run? changes? (= :error outcome))
      {:tone :error
       :text "DRY RUN stopped by an unexpected error - nothing was changed - read the messages above"}

      ;; recorded only under --dry-run, whether or not the book changes things
      (or (seq unmet) (seq unrehearsed))
      {:tone :error
       :text (str (cond dry-run? "DRY RUN - nothing was changed"
                        (changed? record) (str "FAILED - " (made record))
                        :else "FAILED - nothing was changed")
                  (when (seq unmet)
                    (str "; the precondition of " (str/join ", " unmet)
                         " does not hold against the current state"))
                  (when (seq unrehearsed)
                    (str "; not rehearsed: " (str/join ", " unrehearsed)
                         " - a real run would do there what this rehearsal could not show")))}

      ;; a rehearsal that failed before it showed its plan rehearsed nothing
      (and dry-run? changes? failed)
      {:tone :error
       :text "DRY RUN failed - nothing was changed, and the plan was not shown in full - read the messages above"}

      ;; a read-only book reads and reports under --dry-run too
      (and dry-run? changes?)
      {:tone :neutral :text "DRY RUN - nothing was changed, this was a rehearsal"}

      (= :error outcome)
      {:tone :error
       :text (str "FAILED - an unexpected error stopped the run"
                  (when-let [m (made record)] (str "; " m " before it"))
                  " - read the messages above")}

      ;; a playbook can abort at a later step, after earlier ones applied
      aborted
      (cond
        (pos? applied)
        {:tone :error
         :text (str "ABORTED after " (effects applied) " - " (with-refusal aborted refused) " - the effects before it stay applied")}
        (changed? record)
        {:tone :error :text (str "ABORTED - " (with-refusal aborted refused) " - after a change had already run")}
        :else
        {:tone :neutral :text (str "ABORTED - " (with-refusal aborted refused) " - nothing was changed")})

      paused
      {:tone :neutral
       :text (str "PAUSED"
                  (cond (pos? applied) (str " after " (effects applied))
                        thunk (str " after " unknown-step " had run"))
                  " - " paused)}

      (not changes?)
      (cond
        ;; a healthy scan has nothing critical, and may still have reported
        ;; warnings and info findings
        (and (not failed) findings?) {:tone :ok :text "DONE - nothing critical; read the findings above"}
        (not failed) {:tone :ok :text "DONE - no findings"}
        (findings-only? {:changes? changes? :outcome outcome} record)
        {:tone :neutral
         :text (let [n (critical-findings record)]
                 (str "FINDINGS - the book ran and reported " n " critical finding" (when (not= 1 n) "s")
                      ", see above"))}
        (seq (:incomplete record))
        {:tone :error :text (str "FAILED - " (str/join "; " (distinct (:incomplete record)))
                                 " - read the messages above")}
        :else {:tone :error :text "FAILED - read the messages above"})

      stopped
      {:tone :error
       :text (cond
               (pos? applied)
               (str "FAILED after " (effects applied) " - stopped at: " stopped
                    " - the effects before it stay applied"
                    (when thunk (str ", and " unknown-step " had run")))
               thunk
               (str "FAILED at the first effect: " stopped " - but " unknown-step " had already run")
               :else
               (str "FAILED at the first effect: " stopped " - nothing was changed"))}

      (false? verified)
      {:tone :error
       :text (str "NOT VERIFIED - the change ran, but " (unverified-phrase unverified)
                  " - read the messages above")}

      (= :failed thunk)
      {:tone :error :text "FAILED - the change reported a failure - read the messages above"}

      failed
      {:tone :error
       :text (str "FAILED - "
                  (some-> (refused-phrase refused) (str " - "))
                  "read the messages above"
                  (if-let [m (made record)] (str "; " m) "; nothing was changed"))}

      (and (true? verified) (changed? record) (seq unconfirmed))
      {:tone :neutral :text (str "DONE - applied" (unconfirmed-suffix unconfirmed) (skipped-suffix skipped))}

      (and (true? verified) (changed? record))
      {:tone :ok :text (str "DONE - applied and verified" (skipped-suffix skipped))}

      (changed? record)
      {:tone :ok :text (str "DONE - applied; this book has no post-checks" (skipped-suffix skipped))}

      (and (true? verified) (seq unconfirmed))
      {:tone :neutral :text (str "DONE - nothing needed changing" (unconfirmed-suffix unconfirmed) (skipped-suffix skipped))}

      (true? verified)
      {:tone :ok :text (str "DONE - verified, nothing needed changing" (skipped-suffix skipped))}

      (seq skipped)
      {:tone :ok :text (str "DONE - already done, nothing was changed - skipped: " (str/join ", " skipped))}

      :else
      {:tone :ok :text "DONE - nothing was changed"})))
