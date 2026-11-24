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
      :confirm       {:action \"...\" :destructive? false}
      :recovery      \"...\" or (fn [ctx] -> string)   ; optional, see below
      :steps         [..]}

   Each step receives {:context :cluster :target :opts :announcement-data}.

   :recovery is what the operator reads under the FAILED banner when the run
   stops at that step: what the steps before it left behind and how to bring
   the system back. A function receives the step's context with :applied, the
   effects applied so far, and runs at the failure, so it can read the state
   the failure left; one that throws leaves the banner intact and says the
   guidance could not be rendered. The playbook's :recovery covers a step
   without its own once the run has changed something. Every step whose
   failure leaves a system parked or half-owned - a controller suspended, a
   finalizer stripped - carries one.

   The checks before the first change or pause run before the confirmation, so
   the operator decides with their report on screen. The confirmation lists
   every later step and covers all of them: a change shows what it does and runs
   without asking again, while its precondition and pin are still checked right
   before it. A change whose :confirm says :separately? true - a point of no
   return whose exact items the operator must see - asks again when it is
   reached. The confirmation is destructive when any change is.

   The pauses split the steps into segments. `book` runs them all and waits at
   each pause; --from-segment N starts after the operator acted on the pause
   that opens segment N, checking its :verify without waiting. A pause with no
   terminal to wait at stops the playbook and names that flag, unless it is
   :unattended :continue, whose :verify then waits for the result instead.
   `run-segment!` runs one segment against a subject, for a driver that takes a
   batch through a segment, lets the operator act once for all of it, and
   continues with the next."
  (:refer-clojure :exclude [run!])
  (:require [gumshoe.flow :as flow]
            [gumshoe.interact :as interact]
            [gumshoe.kubectl :as kubectl]
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
  "Every option any step reads, and the playbook's own."
  [{:keys [steps options]}]
  (merge (reduce (fn [acc step]
                   (reduce-kv (fn [acc k v]
                                (assoc acc k (if-let [seen (get acc k)] (join-option k seen v) v)))
                              acc
                              (:options step)))
                 {}
                 steps)
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
                                (when (separately? step) " (asks again)"))))
                       steps))
   :destructive? (boolean (or (:destructive? confirm)
                              (some #(and (= :change (kind %)) (get-in % [:confirm :destructive?]))
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

(defn- run-step!
  [step {:keys [target opts announcement-data] :as ctx} wait?]
  (stdout/print-section (::title step))
  (case (kind step)
    :change (mutation/run-on! (assoc step :confirmed? (not (separately? step))) target opts announcement-data)
    :check (boolean ((:check step) ctx))
    :pause (pause! step ctx wait?)))

(defn- stopped-at!
  "Records the recovery guidance for `step`, the one the run stopped at: its
   own, or once the run changed something the playbook's `fallback`. A
   rehearsal changed nothing and records none."
  [step ctx fallback]
  (when-not flow/*dry-run*
    (outcome/recovery! (or (:recovery step) (when (outcome/changed-so-far?) fallback)) ctx)))

(defn- run-steps!
  "Runs the steps in order, stopping at the first that fails. `wait?` says
   whether a pause that opens the run waits for the operator; `fallback` is
   the playbook's recovery guidance for a step without its own."
  [steps ctx wait? fallback]
  (reduce (fn [_ [i step]]
            (if (try (run-step! step ctx (or wait? (pos? i)))
                     (catch Throwable e
                       (stopped-at! step ctx fallback)
                       (throw e)))
              true
              (do (stopped-at! step ctx fallback)
                  (reduced false))))
          true
          (map-indexed vector steps)))

(defn- context-for
  [target opts announcement-data]
  {:context (kubectl/current-context)
   :cluster (kubectl/current-cluster)
   :target target
   :opts opts
   :announcement-data announcement-data})

(defn- confirmed!
  [playbook steps ctx]
  (or flow/*dry-run*
      (empty? steps)
      (interact/confirm! (confirmation playbook steps ctx))))

(defn run-segment!
  "Runs segment `n` (from 1) of the playbook against an already-chosen target.
   The caller confirmed it, so its changes run without asking again, except
   those that ask :separately?; a pause
   opening it checks its :verify without waiting. Returns true when every step
   succeeded."
  [playbook n target opts announcement-data]
  (let [segs (segments (:steps playbook))]
    (if-let [steps (get segs (dec n))]
      (run-steps! steps (context-for target opts announcement-data) false (:recovery playbook))
      (do (stdout/error (format "the playbook has %d segments, there is no segment %d" (count segs) n))
          false))))

(defn run-on!
  "Runs the playbook against an already-chosen target, from --from-segment (1
   when absent) to its end: the leading checks, the one confirmation, then
   every later step. Returns true when every step succeeded."
  [playbook target opts announcement-data]
  (let [segs (segments (:steps playbook))
        from (or (:from-segment opts) 1)
        ctx (context-for target opts announcement-data)]
    (if-not (<= 1 from (count segs))
      (do (stdout/error (format "the playbook has %d segments, there is no segment %d" (count segs) from))
          false)
      (let [steps (apply concat (drop (dec from) segs))
            [leading later] (split-with #(= :check (kind %)) steps)]
        ;; only the pause that opens a resumed segment skips the wait:
        ;; the operator did its manual step before running --from-segment.
        ;; A pause after the leading checks, or one opening a fresh run,
        ;; still has its manual step ahead of it
        (and (run-steps! leading ctx false (:recovery playbook))
             (confirmed! playbook later ctx)
             (run-steps! later ctx (or (= 1 from) (boolean (seq leading))) (:recovery playbook)))))))

(defn run!
  "Selects the subject, honouring the flags, and runs the playbook against it.
   Returns true when there is nothing to select, false when nothing was picked."
  [{:keys [select] :as playbook} opts announcement-data]
  (let [{:keys [candidates target unread]} (select/resolve-target select opts (kubectl/current-context))]
    (cond
      unread (select/unread! select unread)

      (empty? candidates)
      (do (stdout/ok (or (:empty-message select) "nothing to do")) true)

      (select/nothing-selected? (:mode select) target)
      (do (stdout/error (format "no %s selected" (:label select))) false)

      :else
      (run-on! playbook target opts announcement-data))))

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
                 (when (get-in step [:confirm :destructive?]) " (destructive)")
                 (when (:verify step) ", verified")
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
   derived from its steps."
  [{:keys [description steps] :as playbook}]
  (runbook/execute!
   {:description description
    :options (cond-> (all-options playbook)
               (some #(= :pause (kind %)) steps) (merge segment-option))
    :prerequisites (all-prerequisites playbook)
    :details (details playbook)
    :action (fn [opts {:keys [announcement-data]}] (run! playbook opts announcement-data))}))
