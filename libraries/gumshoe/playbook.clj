;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.playbook
  "A playbook as data: the steps it runs against one subject, in order, and the
   pauses where the operator acts outside gumshoe.

   A step is one of
     a mutation spec (see gumshoe.mutation)    a confirmed, announced, verified change
     {:check (fn [ctx] -> bool)}               reads and reports; false stops the playbook
     {:pause \"what the operator does now\"     waits for ENTER, then proves the result
      :verify (fn [ctx] -> post-checks)        optional
      :unattended :continue}                   optional, see below
   and may carry :title, :prerequisites, :options and :recovery. The playbook's
   prerequisites and options are derived from its steps, joined with its own,
   so the harness checks everything any step needs before the first one runs.
   Steps that declare the same option must agree on its :coerce, :alias and
   :default.

   Spec:
     {:description   \"...\"
      :options       {..}   ; joined with the steps' options, winning on conflict
      :prerequisites {..}   ; joined with the steps' prerequisites
      :select        {..}   ; picks the subject once, see gumshoe.select
      :subject       {..}   ; optional, reads a named target, see below
      :done          (fn [ctx] -> bool)   ; optional, see below
      :confirm       {:action \"...\" :destructive? false}
      :recovery      \"...\" or (fn [ctx] -> string)   ; optional, see below
      :kube-context  (fn [opts] -> a kube context name or nil)   ; optional, see below
      :steps         [..]}

   Each step receives {:context :cluster :target :opts :announcement-data}.

   :kube-context names the kube context the whole playbook reads and acts
   on, from the parsed flags; nil, or none, means the current one. The run
   resolves it once and pins it (gumshoe.kube-context): the pick, every
   step's context, every change step's reads and effects and the
   announcement data's :cluster use it. A change step that declares its own
   :kube-context runs only when it names the context the playbook pinned,
   and is refused otherwise, since the playbook picked its subject there; a
   step shared with a standalone book therefore takes the same
   :kube-context as the playbook that runs it.

   A target the flags name that the select does not offer is judged as
   gumshoe.mutation/pick! judges it, with pick-spec: :subject, the typed
   subject of a change spec (e.g. {:type \"namespaces\"}), reads it, so the
   run says whether it is absent or present; without one, the :subject of
   the first change step that picks through the playbook's select reads it.
   :done, a function of the context that read made ({:context :cluster
   :target :opts :ref :object ..}), judges whether the whole playbook
   already took effect on it; when it holds the run succeeds with nothing
   changed, otherwise the present target is not eligible. A playbook that
   declares neither fails closed with the generic message.

   :recovery is what the operator reads under the FAILED banner when the run
   stops at that step: what the steps before it left behind and how to bring
   the system back. A function receives the step's context with :applied, the
   effects applied so far, and runs at the failure, so it can read the state
   the failure left; one that throws leaves the banner intact and says the
   guidance could not be rendered. Once the run has changed something, the
   playbook's :recovery, its standing guidance, follows the step's own, so a
   step a caller added at a position still prints what the playbook leaves
   parked; a playbook without one names the effects its earlier steps
   applied instead (see compose-recovery). Every step whose failure leaves a
   system parked or half-owned - a controller suspended, a node cordoned, a
   finalizer stripped - carries one, and so does a playbook whose steps
   leave such a state for the steps after them to undo.

   The checks before the first change or pause run before the confirmation, so
   the operator decides with their report on screen. The confirmation lists
   every later step and covers all of them: a change shows what it does and runs
   without asking again, while its precondition and pin are still checked right
   before it. A change whose :confirm says :separately? true - a point of no
   return whose exact items the operator must see - asks again when it is
   reached. The confirmation is destructive when any change is.

   Before the confirmation, after the leading checks, the playbook judges the
   steps it covers as far as their evidence exists (prejudge!): the pick of
   every change, and the refusal of the first change unless a pause that
   waits comes before it (prejudgeable), so the operator is not asked to
   approve a playbook whose first change will be refused. A change that
   declares :done (see gumshoe.mutation) is first judged for whether it is
   already done; one that is alters nothing, so the change after it is
   judged as the first would be, and an interrupted run started again
   walks past every step that took effect. The confirmation marks those
   \"(already done)\"; it still covers them, and each is judged again when
   reached, over fresh reads, so it is skipped only if it is still done
   and runs, as confirmed, if it is not. When every change is done and no
   pause waits, nothing is asked: each change reports that it was
   skipped, the checks run, and nothing else does. A change after
   another change, or after a pause that waits, judges state the run is
   about to alter, so it is judged only when reached, after the
   confirmation, where judging it earlier would refuse it falsely. Every
   change is still judged right before it runs.

   The pauses split the steps into segments. `book` runs them all and waits at
   each pause; --from-segment N starts after the operator acted on the pause
   that opens segment N, checking its :verify without waiting. A pause with no
   terminal to wait at stops the playbook and names that flag, unless it is
   :unattended :continue, whose :verify then waits for the result instead.
   `run-segment!` runs one segment against a subject, for a driver that takes a
   batch through a segment, lets the operator act once for all of it, and
   continues with the next.

   A library playbook whose steps run in a fixed order offers positions where
   a caller's own steps go: it pairs each step with the position that follows
   it, [[position step] ..], and `with-extra` splices in a map of position ->
   steps, refusing an unknown position as the book loads."
  (:refer-clojure :exclude [run!])
  (:require [clojure.string :as str]
            [gumshoe.change :as change]
            [gumshoe.effect :as effect]
            [gumshoe.flags :as flags]
            [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kube-context :as kube-context]
            [gumshoe.mutation :as mutation]
            [gumshoe.outcome :as outcome]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.runbook :as runbook]
            [gumshoe.select :as select]
            [gumshoe.stdout :as stdout]
            [gumshoe.verify :as verify]))

(defn kind
  "Whether a step is a :change, a :check or a :pause."
  [step]
  (cond
    (contains? step :pause) :pause
    (contains? step :effect) :change
    (contains? step :check) :check
    :else (throw (ex-info "a playbook step needs an :effect, a :check or a :pause"
                          {:step (dissoc step :options :prerequisites)}))))

;; --- positions -------------------------------------------------------------------

(defn destructive?
  "Whether the step is a change whose confirmation calls it destructive or
   whose effect is marked destructive (gumshoe.change/destructive?)."
  [step]
  (change/destructive? step))

(defn positions
  "The positions `anchored` offers, in run order. `anchored` is a vector of
   [position step], each position naming where a caller's steps go: right
   after that step, or nowhere when the position is nil."
  [anchored]
  (vec (keep first anchored)))

(defn insert-at
  "The steps of `anchored`, with the steps `extra` holds for a position right
   after the step that position names. Only the position names are read,
   never a title."
  [anchored extra]
  (vec (mapcat (fn [[position step]] (cons step (get extra position))) anchored)))

(defn check-extra!
  "Throws when `extra` is not a map of position -> steps, names a position
   `anchored` does not offer, holds anything but a sequence of step maps for
   one, or holds a step `refuse` names a reason against. `refuse` takes the
   position and one step and returns why the step cannot go there, or nil;
   `of` names the steps in the error, as in \"the flux-storage steps\"."
  [anchored extra {:keys [of refuse] :or {of "the playbook's steps"}}]
  (let [offered (positions anchored)]
    (when-not (or (nil? extra) (map? extra))
      (throw (ex-info "the extra steps must be a map of position -> steps"
                      {:extra extra :positions offered})))
    (doseq [[position added] extra]
      (when-not (some #{position} offered)
        (throw (ex-info (format "%s is not a position of %s; the positions are %s"
                                (pr-str position) of (pr-str offered))
                        {:position position :positions offered})))
      (when-not (and (sequential? added) (every? map? added))
        (throw (ex-info (format "the steps for %s must be a vector of step maps" (pr-str position))
                        {:position position :steps added})))
      (when refuse
        (doseq [step added]
          (when-let [why (refuse position step)]
            (throw (ex-info why {:position position :step (:title step)}))))))))

(defn with-extra
  "The steps of `anchored` with a caller's `extra` steps at the positions
   they name, checked as check-extra! does with `options`. A playbook built
   from fixed steps offers positions this way, so a casebook adds its own
   steps by position name and an unknown name fails as its book loads:

     (with-extra [[nil read] [:after-delete delete] [nil resume]]
                 {:after-delete [my-step]}
                 {:of \"the reset steps\"})"
  ([anchored extra] (with-extra anchored extra {}))
  ([anchored extra options]
   (check-extra! anchored extra options)
   (insert-at anchored extra)))

(defn all-prerequisites
  "Everything any step needs, and the playbook's own."
  [{:keys [steps] :as playbook}]
  (apply prerequisites/union (concat (map :prerequisites steps) [(:prerequisites playbook)])))

(def ^:private option-identity [:coerce :alias :default])

(defn- join-option
  [key a b]
  (if (= (select-keys a option-identity) (select-keys b option-identity))
    a
    (throw (ex-info (format "the steps declare --%s differently: %s and %s"
                            (name key) (pr-str a) (pr-str b))
                    {:option key :declarations [a b]}))))

(defn all-options
  "Every option any step reads, in order of first appearance across the
   steps, then the playbook's own."
  [{:keys [steps options]}]
  (flags/merge-ordered
   (reduce (fn [acc [k v]]
             (flags/merge-ordered acc {k (if-let [seen (get acc k)] (join-option k seen v) v)}))
           {}
           (mapcat :options steps))
   options))

(defn- label
  [step]
  (case (kind step)
    :change (or (get-in step [:confirm :action]) (:description step))
    :check (or (:title step) (:description step) "check")
    :pause (:pause step)))

(defn numbered
  "The steps with what the run shows for them: ::number (\"2/5\", none for a
   pause), ::title and ::segment, the segment each step belongs to, from 1."
  [steps]
  (let [total (count (remove #(= :pause (kind %)) steps))]
    (first
     (reduce (fn [[acc i segment] step]
               (if (= :pause (kind step))
                 (let [segment (inc segment)]
                   [(conj acc (assoc step ::segment segment ::title (str "⏸  " (or (:title step) (:pause step)))))
                    i segment])
                 (let [number (format "%d/%d" (inc i) total)]
                   [(conj acc (assoc step ::segment segment ::number number
                                     ::title (str number " " (or (:title step) (label step)))))
                    (inc i) segment])))
             [[] 0 1]
             steps))))

(defn segments
  "The steps split at each pause, the pause opening the segment after it.
   Segment n is at index n-1; a playbook that opens with a pause has an
   empty segment 1, so the --from-segment 2 its pause names exists."
  [steps]
  (let [numbered (numbered steps)
        by-segment (group-by ::segment numbered)]
    (mapv #(vec (get by-segment % []))
          (range 1 (inc (reduce max 1 (map ::segment numbered)))))))

(defn- separately?
  [step]
  (and (= :change (kind step)) (true? (get-in step [:confirm :separately?]))))

(defn confirmation
  "The one confirmation covering `steps`."
  [{:keys [confirm select]} steps {:keys [cluster target]}]
  {:action (:action confirm)
   :target cluster
   :items (concat (select/items (:mode select) target)
                  (map (fn [step]
                         (case (kind step)
                           :pause (::title step)
                           (str (::number step) " " (label step)
                                (when (::done step) " (already done)")
                                (when (separately? step) " (asks again)"))))
                       steps))
   :destructive? (boolean (or (:destructive? confirm)
                              (some #(and (= :change (kind %)) (destructive? %))
                                    steps)))})

(defn- attended?
  []
  (some? (System/console)))

(defn- wait-for-operator!
  [what]
  (stdout/print-banner stdout/yellow (str "⏸  " what))
  (binding [*out* *err*]
    (print "Press ENTER once that is done: ")
    (flush))
  (some? (read-line)))

(defn- pause!
  [{:keys [pause verify unattended] :as step} ctx wait?]
  (let [resume (format "continue with --from-segment %d once this is done: %s" (::segment step) pause)
        attended? (attended?)]
    (cond
      flow/*dry-run*
      (do (stdout/err-println (str "  " (stdout/blue "would pause:") " " pause)) true)

      (and wait? (not attended?) (not= :continue unattended))
      (do (outcome/paused! resume)
          (stdout/warn (str "no terminal to wait at - " resume))
          false)

      (and wait? attended? (not (wait-for-operator! pause)))
      (do (outcome/paused! resume) false)

      verify
      (verify/all (verify ctx))

      :else true)))

(defn- run-change!
  "Runs a change step through gumshoe.mutation, keeping the plan its :effect
   built in `plan`, so the run can name what the step applied, and setting
   `skipped` when it was skipped as already done. A step judged before the
   confirmation carries the read cache and the context of that judgement,
   so its :derive phase is the one judged, reads nothing again and computes
   its :values once."
  [step {:keys [target opts announcement-data]} plan skipped]
  (mutation/run-on! (-> (dissoc step ::derive-cache ::derived ::done ::skip)
                        (assoc :confirmed? (not (separately? step)))
                        (update :effect (fn [effect] (fn [ctx] (vreset! plan (effect ctx))))))
                    target opts announcement-data
                    {:derive-cache (::derive-cache step) :derived (::derived step)
                     :on-skip #(vreset! skipped true)}))

;; --- judging before the confirmation ----------------------------------------------

(defn prejudgeable
  "The indices of the change steps among `steps` - the steps one
   confirmation covers - judged before that confirmation: every change
   `done` holds the index of, as the walk meets it, and the first change
   that is not done, unless a pause that waits comes before it. `done` is
   the set of indices already judged done, empty when absent.

   A change after another change judges state the earlier one is about to
   alter (a drain after the cordon it needs), and a change after a waiting
   pause judges state the operator is about to alter, so the evidence of
   either exists only once the run reaches it; such a step is judged when
   reached, after the confirmation, and never before it, where it would be
   refused for a state the run itself is about to change. A change judged
   done alters nothing, so the change after it is judged as the first one
   would be. Checks read and change nothing, so they do not end the
   judgeable prefix. `waits-first?` says whether a pause opening `steps`
   waits; one that does not - the pause opening a resumed segment, whose
   manual step is done - does not end it either. Pure."
  ([steps waits-first?] (prejudgeable steps waits-first? #{}))
  ([steps waits-first? done]
   (loop [[step & more] steps i 0 judged []]
     (if-not step
       (not-empty judged)
       (case (kind step)
         :check (recur more (inc i) judged)
         :pause (if (or (pos? i) waits-first?) (not-empty judged) (recur more (inc i) judged))
         :change (if (contains? done i)
                   (recur more (inc i) (conj judged i))
                   (not-empty (conj judged i))))))))

(defn- waiting-pause?
  [step i waits-first?]
  (and (= :pause (kind step)) (or (pos? i) waits-first?)))

(defn nothing-to-do?
  "Whether `steps`, as prejudge! returned them, need no confirmation: they
   hold a change, every change was judged done before the confirmation
   (::done), and no pause waits among them. Such a run reports each change
   skipped, runs its checks and asks nothing. Pure."
  [steps waits-first?]
  (let [changes (filter #(= :change (kind %)) steps)]
    (boolean (and (seq changes)
                  (every? ::done changes)
                  (not-any? true? (map-indexed (fn [i step] (waiting-pause? step i waits-first?)) steps))))))

(defn prejudge!
  "Judges `steps`, before the one confirmation covering them, as far as
   their evidence exists: the pick of every change, which parse-pick judges
   alone, and each prejudgeable change, over what its :derive phase reads
   now (gumshoe.mutation/prejudge) - whether it is already done, and when
   it is not, its refusal. Prints the first refusal under its step's title
   and returns nil; otherwise returns the steps, each change judged done
   carrying its verdict as ::done, and the judged change that is not done
   carrying the read cache its judgement filled and the context it derived
   (unless it asks :separately? or its :done was undecided).
   A rehearsal asks for no confirmation and judges every step as it
   reaches it, so it returns the steps as they are. `ctx` is the run's
   context and `waits-first?` as prejudgeable takes it. A driver that
   confirms a segment itself calls this before its confirmation."
  [steps {:keys [target opts announcement-data]} waits-first?]
  (if flow/*dry-run*
    steps
    (let [refused! (fn [step verdict]
                     (stdout/print-section (::title step))
                     (mutation/prejudge-printed (update verdict :step #(or % (mutation/step-name step))))
                     nil)
          pick-refusal (fn [steps from to]
                         (some (fn [i]
                                 (let [step (nth steps i)]
                                   (when (= :change (kind step))
                                     (when-let [refusal (:refusal (mutation/parse-pick step target))]
                                       [step refusal]))))
                               (range from to)))]
      (loop [steps (vec steps) done #{} from 0]
        (let [next (first (remove done (prejudgeable steps waits-first? done)))
              to (if next (inc next) (count steps))]
          (if-let [[step refusal] (pick-refusal steps from to)]
            (refused! step {:refusal refusal})
            (if-not next
              steps
              (let [step (nth steps next)
                    {:keys [cache derived undecided] :as verdict} (mutation/prejudge step target opts announcement-data)]
                (cond
                  (:done verdict)
                  (recur (update steps next assoc ::done (:done verdict)) (conj done next) to)

                  cache
                  (let [steps (cond-> steps
                                (not (or (separately? step) undecided))
                                (update next assoc ::derive-cache cache ::derived derived))]
                    (if-let [[step refusal] (pick-refusal steps to (count steps))]
                      (refused! step {:refusal refusal})
                      steps))

                  :else (refused! step verdict))))))))))

(defn- run-step!
  "Runs one step. A change marked ::skip was judged done before a
   confirmation that was never asked, because nothing was left to do: it
   reports that verdict and runs nothing, since nothing covers running it."
  [step ctx wait? plan skipped]
  (stdout/print-section (::title step))
  (case (kind step)
    :change (if (::skip step)
              (do (vreset! skipped true) (mutation/skipped! step (::done step)))
              (run-change! step ctx plan skipped))
    :check (boolean ((:check step) ctx))
    :pause (pause! step ctx wait?)))

;; --- recovery --------------------------------------------------------------------

(defn applied-effects
  "What a step applied, as gumshoe.effect/describe renders it: the first
   `applied` effects of `plan` that may change something, which effect/run!
   applies in order and counts; the notes change nothing and are not
   counted."
  [plan applied]
  (->> (effect/as-plan plan)
       (filter effect/changes-state?)
       (take applied)
       (mapv effect/describe)))

(defn- left-behind-text
  [left-behind]
  (str/join "\n"
            (concat ["This playbook declares no recovery guidance. What the steps before this one left behind - nothing was rolled back:"]
                    (mapcat (fn [{:keys [title effects]}]
                              (cons (str "  " title ":") (map #(str "    " %) effects)))
                            left-behind))))

(defn- part-text
  [role {:keys [text error]}]
  (or text (format "The guidance for %s could not be rendered - %s" role error)))

(defn compose-recovery
  "The guidance under the FAILED banner of a run that stopped at a step, from
   the state of the run, as gumshoe.outcome/render returns guidance: {:text
   ..}, {:error ..} or nil.

     :step        the step's own guidance as rendered, or for a change
                  without any the engine's fallback naming what it applied
     :changed?    whether the run changed anything before it stopped
     :standing    the playbook's :recovery as rendered at the failure
     :left-behind [{:title .. :effects [..]}], the steps before the stopped
                  one that applied effects, in run order

   The step's guidance comes first. Once the run changed something the
   playbook's standing guidance follows it, so a step added at a position
   still prints what the playbook leaves parked; a standing text equal to
   the step's prints once. A playbook without standing guidance names the
   effects its earlier steps applied instead, so every change still in
   place reaches the operator. A run that changed nothing prints the step's
   guidance alone. A single part keeps its rendered form; several join into
   one text, a part that could not be rendered saying so in its place."
  [{:keys [step changed? standing left-behind]}]
  (let [listed (when (and changed? (nil? standing) (seq left-behind))
                 {:text (left-behind-text left-behind)})
        parts (keep identity [(when step ["the step the run stopped at" step])
                              (when (and changed? standing (not= standing step)) ["the playbook" standing])
                              (when listed ["the playbook" listed])])]
    (case (count parts)
      0 nil
      1 (second (first parts))
      {:text (str/join "\n" (map #(apply part-text %) parts))})))

(defn- stopped-at!
  "Records the recovery guidance of the run stopped at `step`, composed by
   compose-recovery: the step's part is what the step recorded while it ran
   (a change records its own :recovery or the engine's fallback), else its
   :recovery rendered here. A rehearsal changed nothing and records none,
   and a run stopped at a pause it could not wait at did not fail."
  [step ctx {:keys [recovery left-behind]}]
  (when-not (or flow/*dry-run* (:paused (outcome/current)))
    (let [ctx (assoc ctx :applied (outcome/applied-count))
          recorded (outcome/recorded-recovery)
          changed? (outcome/changed-so-far?)]
      (outcome/settle-recovery!
       (compose-recovery {:step (or (:recovery recorded)
                                    (some-> (:recovery step) (outcome/render ctx))
                                    (:fallback recorded))
                          :changed? changed?
                          :standing (when (and changed? recovery) (outcome/render recovery ctx))
                          :left-behind left-behind})))))

(defn- run-steps!
  "Runs the steps in order, stopping at the first that fails. `wait?` says
   whether a pause that opens the run waits for the operator; `playbook`
   gives the standing :recovery composed with a step's own. The steps that
   applied effects are kept as the run goes, so a playbook without :recovery
   still names them when a later step fails.

   A rehearsal applies no change and waits at no pause, so once it has
   rehearsed a change it did not skip, or a pause that would wait, the
   state the later changes find is not the one a real run would give them:
   it judges none of them done, and rehearses each, as a real run reaching
   them after that change or pause would judge them over what it left."
  [steps ctx wait? playbook]
  (let [left-behind (volatile! [])
        rehearsed-ahead? (volatile! false)]
    (reduce (fn [_ [i step]]
              (let [plan (volatile! nil)
                    skipped (volatile! false)
                    before (outcome/applied-count)
                    wait? (or wait? (pos? i))
                    stop! #(stopped-at! step ctx {:recovery (:recovery playbook) :left-behind @left-behind})
                    step (if @rehearsed-ahead? (dissoc step :done :satisfied?) step)
                    ok? (try (run-step! step ctx wait? plan skipped)
                             (catch Throwable e (stop!) (throw e)))]
                (when (and flow/*dry-run*
                           (case (kind step) :change (not @skipped) :pause wait? :check false))
                  (vreset! rehearsed-ahead? true))
                (if ok?
                  (let [effects (applied-effects @plan (- (outcome/applied-count) before))]
                    (when (seq effects)
                      (vswap! left-behind conj {:title (::title step) :effects effects}))
                    true)
                  (do (stop!) (reduced false)))))
            true
            (map-indexed vector steps))))

(defn- context-for
  [{:keys [context cluster]} target opts announcement-data]
  {:context context
   :cluster cluster
   :target target
   :opts opts
   :announcement-data (some-> announcement-data (assoc :cluster cluster))})

(defn- refused
  [why]
  (stdout/err-println (str "refusing: " why))
  false)

(defn pinned
  "(f connection) with the kube context of the playbook or dispatcher
   pinned for the run (see gumshoe.kube-context/pinning): the one an
   enclosing run pinned, or its :kube-context resolved for opts. False,
   having said why, when that is refused."
  [{:keys [kube-context]} opts f]
  (kube-context/pinning kube-context opts refused f))

(defn- confirmed!
  [playbook steps ctx]
  (or flow/*dry-run*
      (empty? steps)
      (interact/confirm! (confirmation playbook steps ctx))))

(defn run-segment!
  "Runs segment `n` (from 1) of the playbook against an already-chosen target.
   The caller confirmed it, having judged it with prejudge! first, so its
   changes run without asking again, except those that ask :separately?; a
   pause
   opening it checks its :verify without waiting. Returns true when every step
   succeeded."
  [playbook n target opts announcement-data]
  (let [segs (segments (:steps playbook))]
    (if-let [steps (get segs (dec n))]
      (pinned playbook opts
              #(run-steps! steps (context-for % target opts announcement-data) false playbook))
      (do (stdout/error (format "the playbook has %d segments, there is no segment %d" (count segs) n))
          false))))

(defn- run-pinned!
  [playbook connection target opts announcement-data]
  (let [segs (segments (:steps playbook))
        from (or (:from-segment opts) 1)
        ctx (context-for connection target opts announcement-data)]
    (if-not (<= 1 from (count segs))
      (do (stdout/error (format "the playbook has %d segments, there is no segment %d" (count segs) from))
          false)
      (let [steps (apply concat (drop (dec from) segs))
            [leading later] (split-with #(= :check (kind %)) steps)
            ;; only the pause that opens a resumed segment skips the wait:
            ;; the operator did its manual step before running --from-segment.
            ;; A pause after the leading checks, or one opening a fresh run,
            ;; still has its manual step ahead of it
            waits-first? (or (= 1 from) (boolean (seq leading)))]
        (and (run-steps! leading ctx false playbook)
             (if-let [later (prejudge! later ctx waits-first?)]
               (if (nothing-to-do? later waits-first?)
                 (do (stdout/ok "every change of this playbook is already done - nothing to confirm")
                     (run-steps! (mapv #(cond-> % (= :change (kind %)) (assoc ::skip true)) later)
                                 ctx waits-first? playbook))
                 (and (confirmed! playbook later ctx)
                      (run-steps! later ctx waits-first? playbook)))
               false))))))

(defn run-on!
  "Runs the playbook against an already-chosen target, from --from-segment (1
   when absent) to its end: the leading checks, the judgement before the
   confirmation (prejudge!), the one confirmation, then every later step.
   Returns true when every step succeeded."
  [playbook target opts announcement-data]
  (pinned playbook opts #(run-pinned! playbook % target opts announcement-data)))

(defn- step-subject
  "The :pick entry of the first change step among steps that picks through
   the enclosing select (it has no :select of its own) and declares one."
  [steps]
  (some (fn [step]
          (when (and (map? step) (= :change (kind step)) (nil? (:select step)))
            (change/pick-subject (change/normalize step))))
        steps))

(defn pick-spec
  "The spec gumshoe.mutation/pick! picks the subject of a playbook or a
   dispatcher by: its :select; its :subject, else the :subject of the
   first change step that picks through that select, so a named target
   the candidates leave out is read and reported absent or present; its
   :done, which judges such a target already done; and its :title, else
   the action it confirms, else its description, which a skip names. A
   value with neither :subject nor :done picks by its :select alone."
  [{:keys [select subject done title confirm description steps]}]
  (let [from-step (when-not subject (step-subject steps))]
    (cond-> {:select select :title (or title (:action confirm) description)}
      subject (assoc :subject subject)
      from-step (assoc :subjects [from-step])
      done (assoc :done done))))

(defn run!
  "Selects the subject, honouring the flags (see gumshoe.mutation/pick!,
   which judges a named target by pick-spec), and runs the playbook
   against it. Returns true when there is nothing to select or the named
   target is not offered and the playbook's :done judges it already done,
   false when nothing was picked or the named target is not offered and
   not done (see gumshoe.mutation/pick!)."
  [playbook opts announcement-data]
  (pinned playbook opts
          (fn [{:keys [context]}]
            (let [{:keys [target result] :as picked} (mutation/pick! (pick-spec playbook) opts context)]
              (if (contains? picked :result)
                result
                (run-on! playbook target opts announcement-data))))))

(defn- step-line
  [step]
  (case (kind step)
    :check (str "  " (::title step) " - reads and reports")
    :pause (str "  " (::title step)
                (if (= :continue (:unattended step))
                  " - waits for ENTER, or goes on without a terminal"
                  " - waits for ENTER")
                (when (:verify step) ", then checks the result")
                "; --from-segment " (::segment step) " continues here")
    :change (str "  " (::title step)
                 (when (:title step) (str " - " (label step)))
                 (when (destructive? step) " (destructive)")
                 (when (:verify step) ", verified")
                 (when (or (:done step) (:satisfied? step)) ", skipped when already done")
                 (when (separately? step) ", asks again"))))

(defn details
  "What --help says a playbook does, generated from its steps."
  [{:keys [select steps] :as playbook}]
  (let [steps (numbered steps)
        [leading later] (split-with #(= :check (kind %)) steps)
        destructive? (:destructive? (confirmation playbook later {}))
        needs (prerequisites/summary (all-prerequisites playbook))]
    (concat
     ["What it does:"
      (str "  picks " (mutation/picks select) ", then:")]
     (map step-line leading)
     (when (seq later)
       [(str "  - asks once to confirm every step below"
             (when destructive? " (destructive - type yes)"))])
     (map step-line later)
     (when (seq needs)
       (concat ["" "Needs:"] (map #(str "  " %) needs))))))

(def ^:private segment-option
  {:from-segment {:desc "Start at this segment: the steps after the pause that opens it, once the operator acted on it"
                  :coerce :long}})

(defn book
  "Builds and runs a playbook from its spec, with the options and prerequisites
   derived from its steps. A :subject or :done change/normalize refuses
   fails the book as it loads."
  [{:keys [description steps] :as playbook}]
  (change/normalize (pick-spec playbook))
  (runbook/execute!
   {:description description
    :options (cond-> (all-options playbook)
               (some #(= :pause (kind %)) steps) (flags/merge-ordered segment-option))
    :prerequisites (all-prerequisites playbook)
    :kube-context (or (:kube-context playbook) (constantly nil))
    :details (details playbook)
    :action (fn [opts {:keys [announcement-data]}] (run! playbook opts announcement-data))}))
