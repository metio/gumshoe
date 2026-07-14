;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.flow
  "The unified flow every change in this repository follows:

   1. confirm    - the user sees exactly what is about to happen and approves
   2. announce   - the change is posted to the changelog room
   3. execute    - the actual work
   4. post-check - the intended state is verified before we call it done

   A change that executes but cannot be verified fails, because 'probably
   worked' is not a state an SRE should inherit.

   The work is expressed one of two ways. Preferred is :effect - a plan of
   effect data (see gumshoe.effect) - which composes, dry-runs, and tests. The
   legacy :execute! thunk still works, but a book using it cannot be dry-run,
   so under --dry-run the flow refuses rather than risk running it."
  (:require [clojure.string :as str]
            [gumshoe.effect :as effect]
            [gumshoe.interact :as interact]
            [gumshoe.stdout :as stdout]
            [gumshoe.verify :as verify]))

(def ^:dynamic *dry-run*
  "When true, a change describes its effect and touches nothing. The runbook
   harness binds this from --dry-run."
  false)

(defn valid-config?
  "A change may only run when its confirmation can tell the user exactly what
   is about to happen, and when it declares its work as either an :effect plan
   or an :execute! thunk."
  [{:keys [confirmation execute! effect]}]
  (boolean (and (map? confirmation)
                (string? (:action confirmation))
                (not (str/blank? (:action confirmation)))
                (some? (:target confirmation))
                (seq (:items confirmation))
                (or (fn? execute!) (sequential? effect)))))

(defn- verify-result
  [post-checks]
  (if (seq post-checks)
    (do (stdout/print-section "🔎 Post-check")
        (verify/all post-checks))
    true))

(defn- do-work!
  "Runs the declared work - the effect plan when present, otherwise the thunk."
  [effect execute!]
  (if effect
    (effect/run! effect)
    (execute!)))

(defn change!
  "Runs a change through confirm -> [precondition] -> announce -> execute -> verify.

   :confirmation  request map for interact/confirm! (:action, :target, :items)
   :precondition  optional thunk re-checked AFTER the confirm and before any
                  action, so a change confirmed while the world was safe aborts
                  cleanly if the world drifted while the operator was deciding
                  (e.g. the replacement went NotReady before a drain). Returns
                  truthy to proceed.
   :announce!     optional thunk posting to the changelog
   :effect        a plan of effect data (preferred - composes and dry-runs)
   :execute!      a thunk doing the work, truthy on success (legacy)
   :post-checks   optional seq of verify/eventually maps proving the result

   Returns true only when every step succeeded."
  [{:keys [confirmation precondition announce! execute! effect post-checks] :as config}]
  (cond
    (not (valid-config? config))
    (do (stdout/error "refusing a malformed change: :confirmation needs a non-blank :action, a :target, and :items, and either an :effect plan or an :execute! function")
        false)

    ;; a dry run shows what would happen and changes nothing - no prompt, no
    ;; announcement, no post-check. A legacy thunk cannot be shown safely, so we
    ;; refuse rather than run it.
    *dry-run*
    (do (stdout/print-banner stdout/blue "🔍 DRY RUN - nothing will be changed")
        (stdout/err-println (interact/confirmation-message confirmation))
        (if effect
          (effect/dry-run effect)
          (do (stdout/warn "this book does not support --dry-run yet - it declares an :execute! thunk, not an :effect plan")
              (stdout/warn "nothing was run")
              true)))

    (not (interact/confirm! confirmation))
    false

    (and precondition (not (precondition)))
    (do (stdout/error "the change was confirmed but its precondition no longer holds - aborting before any action")
        false)

    :else
    (do
      (when announce! (announce!))
      (stdout/print-section "⚡ Action")
      (if (do-work! effect execute!)
        (verify-result post-checks)
        false))))
