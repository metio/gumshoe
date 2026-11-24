;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mutation
  "A declarative shape for the many books that are the same underneath: pick a
   subject, then run a confirmed, announced, verified change against it. Such a
   book is a map - what to select, what to confirm, the effect plan, the
   post-checks - and this engine composes it out of gumshoe.select, gumshoe.flow,
   and gumshoe.effect. The bespoke pieces (the candidate query, the effect, the
   checks) stay small, named, and testable; the flow around them is written and
   tested once, here.

   Spec:
     {:description   \"...\"
      :options       {..cli options for the target flags..}
      :prerequisites {..}
      :select {:mode :one | :many | :namespaced
               :label \"Node\"
               :candidates (fn [context] -> seq of names)
               :flag :node                      ; :one / :many
               :namespace-flag :namespace       ; :namespaced
               :name-flag :name                 ; :namespaced
               :preview \"kubectl describe node {}\"}  ; :one, optional fzf side pane
      :empty-message \"every node is already cordoned\"   ; when nothing to pick
      :confirm  {:action \"...\" :destructive? false
                 :separately? false}   ; true: asks again inside a playbook
      :precondition (fn [ctx] -> bool)            ; optional, re-checked after confirm;
                                                  ; reads only, a dry run calls it too
      :announce (fn [ctx] -> changelog message)   ; optional
      :effect   (fn [ctx] -> effect plan)         ; one effect runs as a plan of one;
                                                  ; anything else is refused naming
                                                  ; the step (:title, else the action)
      :verify   (fn [ctx] -> seq of post-check maps)   ; optional
      :derive   (fn [ctx] -> map)   ; optional, values merged into ctx once so a
                                    ; per-run value is shared by :effect and :verify;
                                    ; its reads are strict, see below
      :items    (fn [ctx] -> seq)   ; optional, overrides the confirmation items
      :pin      (fn [ctx] -> {:type :name :namespace? :facts?})  ; optional, see below
      :recovery \"...\" or (fn [ctx] -> string)   ; optional, printed under the
                                    ; FAILED banner when this change fails,
                                    ; see gumshoe.playbook
      :confirmed? true}  ; set by a playbook whose confirmation covered this step

   The :precondition, :announce, :effect, :verify, :derive and :items callbacks
   each receive one context map, so a book destructures exactly what it needs:
     {:context .. :cluster .. :target <the pick(s)> :opts <parsed flags>}

   :derive and :precondition run under kubectl/*strict-reads*, as the
   candidate query of :select and every post-check do. A read in :derive that
   did not answer stops the change before the confirmation, so neither the
   precondition nor the effect is computed from a read that stood in for an
   object nobody could see. A read in :precondition that did not answer
   refuses the change with the reason. A read that answered that its object
   or resource type does not exist still returns nil, which the precondition
   judges. Only the gumshoe.kubectl readers that honour *strict-reads* are
   strict here (see its docstring).

   Three entry points, same spec: `book` is a standalone runbook; `run!` selects
   the subject and runs it, returning a boolean; `run-on!` runs against a subject
   already chosen, so a playbook picks once and drives many steps without
   re-prompting.

   :pin names the object the change acts on. Its fingerprint is taken when the
   confirmation is shown and taken again right before the effect; when the
   object was replaced (another uid) or its spec changed (another generation),
   the change is refused, because the operator confirmed an object that no
   longer exists in that form. Status and metadata updates leave the
   generation alone, so a controller reporting progress does not trip it. An
   object without a generation is compared by resourceVersion. A change that
   rests on something outside the spec - an annotation, a status field - names
   it with :facts (fn [object] -> value), which must be equal too. An object
   that cannot be read when the confirmation is shown is refused as well."
  (:require [clojure.walk :as walk]
            [gumshoe.flow :as flow]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.select :as select]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.announce :as announce]
            [gumshoe.outcome :as outcome]
            [gumshoe.runbook :as runbook]
            [gumshoe.stdout :as stdout]))

;; --- the confirmed object --------------------------------------------------------

(defn fingerprint
  "What identifies the object the operator confirmed: its uid, its generation
   or, for a type without one, its resourceVersion, and the value of facts
   when given."
  ([object] (fingerprint object nil))
  ([object facts]
   (when object
     (let [{:keys [uid generation resourceVersion]} (:metadata object)]
       (cond-> (if generation
                 {:uid uid :generation generation}
                 {:uid uid :resource-version resourceVersion})
         facts (assoc :facts (facts object)))))))

(defn drift
  "Why the object no longer is the one confirmed, or nil."
  [before after]
  (cond
    (nil? before) nil
    (nil? after) "it was deleted after the confirmation"
    (not= (:uid before) (:uid after)) "it was deleted and created again after the confirmation"
    (and (:generation before) (not= (:generation before) (:generation after)))
    (format "its spec changed after the confirmation (generation %s, now %s)" (:generation before) (:generation after))
    (and (:resource-version before) (not= (:resource-version before) (:resource-version after)))
    "it changed after the confirmation"
    (not= (:facts before) (:facts after))
    (format "what the change rests on moved after the confirmation (%s, now %s)" (pr-str (:facts before)) (pr-str (:facts after)))
    :else nil))

(defn fetch-pinned
  [context {:keys [type namespace name]}]
  (if namespace
    (kubectl/get-namespaced-resource context namespace type name)
    (kubectl/get-cluster-resource context type name)))

(defn pinned-precondition
  "The book's precondition followed by the drift check. fetch is
   (fn [ref] -> object); before is the fingerprint taken at confirmation.
   With a ref but no before, the object was not read when the change was
   confirmed - a read that did not answer looks the same as a missing
   object - so nothing ties what the effect would act on to what the
   operator confirmed, and the change is refused."
  [precondition ref before fetch]
  (if-not ref
    precondition
    (fn []
      (and (if precondition (precondition) true)
           (let [why (if before
                       (drift before (fingerprint (fetch ref) (:facts ref)))
                       "it could not be read when the change was confirmed")]
             (when why (println (str "refusing: " why)))
             (nil? why))))))

(defn derived
  "{:values <what derive returned>} or {:unread <reason>}. derive runs under
   kubectl/*strict-reads* and its result is realized inside the binding, so a
   lazy seq cannot run its reads after it, and a read that did not answer
   surfaces as its message instead of as nil - the same contract as the
   candidate listing of a select. A read that answered that its object does
   not exist still derives nil."
  [derive base]
  (try
    {:values (when derive
               (binding [kubectl/*strict-reads* true]
                 (walk/postwalk identity (derive base))))}
    (catch clojure.lang.ExceptionInfo e
      (if (::kubectl/unread (ex-data e)) {:unread (ex-message e)} (throw e)))))

(defn strict-precondition
  "precondition as a thunk whose reads are strict: a read in it that did not
   answer refuses with the reason instead of being judged as an absent
   object."
  [precondition]
  (when precondition
    (fn []
      (try
        (binding [kubectl/*strict-reads* true]
          (boolean (precondition)))
        (catch clojure.lang.ExceptionInfo e
          (if (::kubectl/unread (ex-data e))
            (do (println (str "refusing: could not read what the precondition rests on - " (ex-message e)))
                false)
            (throw e)))))))

(defn run-on!
  "Runs a mutation spec against an ALREADY-CHOSEN `target`, skipping the spec's own
   selection, through derive -> confirm -> [precondition] -> announce -> execute
   -> verify. Returns true/false, false without asking for a confirmation when a
   read in :derive did not answer. This is what a playbook calls per step after picking its
   subject once (e.g. via gumshoe.select/pick), so the operator is not re-prompted
   for every step. `run!` is this plus selection.

   `opts` is the parsed CLI flags; `announcement-data` is the value the runbook
   action receives in its ctx (nil when the book does not announce)."
  [{:keys [select confirm derive precondition announce effect verify] :as spec}
   target opts announcement-data]
  (let [context (kubectl/current-context)
        cluster (kubectl/current-cluster)
        base {:context context :cluster cluster :target target
              :opts opts :announcement-data announcement-data}
        {:keys [values unread]} (derived derive base)]
    (if unread
      (do (stdout/error (format "could not read what the change rests on - %s" unread))
          false)
      (let [ctx (merge base values)
            failed! #(when-not flow/*dry-run* (outcome/recovery! (:recovery spec) ctx))
            ok? (try
                  (let [ref (when-let [pin (:pin spec)] (pin ctx))
                        before (when ref (fingerprint (fetch-pinned context ref) (:facts ref)))
                        items-fn (:items spec)]
                    (flow/change!
                     {:confirmation {:action (:action confirm)
                                     :target cluster
                                     :items (if items-fn (items-fn ctx) (select/items (:mode select) target))
                                     :destructive? (:destructive? confirm)}
                      :precondition (strict-precondition
                                     (pinned-precondition (when precondition #(precondition ctx))
                                                          ref before #(fetch-pinned context %)))
                      :confirmed? (:confirmed? spec)
                      :announce! (when announce
                                   #(announce/announce! cluster announcement-data (announce ctx)))
                      :effect (effect ctx)
                      :step (or (:title spec) (:action confirm))
                      :post-checks (when verify (verify ctx))}))
                  (catch Throwable e (failed!) (throw e)))]
        (when-not ok? (failed!))
        ok?))))

(defn run!
  "Selects the spec's subject (interactive, honouring the CLI flags) and runs it
   through run-on!. Returns true when there is nothing to select (its
   :empty-message is shown), false when the candidates could not be read or
   the operator picks nothing, otherwise the
   change's result. Use run! for a single confirmed step; use run-on! to compose
   several steps against one already-picked subject."
  [{:keys [select empty-message] :as spec} opts announcement-data]
  (let [context (kubectl/current-context)
        {:keys [candidates target unread]} (select/resolve-target select opts context)]
    (cond
      unread (select/unread! select unread)

      (empty? candidates)
      (do (stdout/ok (or empty-message "nothing to do")) true)

      (select/nothing-selected? (:mode select) target)
      (do (stdout/error (format "no %s selected" (:label select))) false)

      :else
      (run-on! spec target opts announcement-data))))

(defn picks
  "How a select spec chooses its subject, in the words --help shows."
  [{:keys [mode label flag namespace-flag name-flag]}]
  (case mode
    :many (str "one or more of " label (when flag (str " (--" (name flag) ", or interactively)")))
    :namespaced (str "one " label " by namespace and name"
                     (when (and namespace-flag name-flag)
                       (str " (--" (name namespace-flag) " and --" (name name-flag) ", or interactively)")))
    (str "one " label (when flag (str " (--" (name flag) ", or interactively)")))))

(defn- needs
  [prerequisites]
  (when-let [lines (seq (prerequisites/summary prerequisites))]
    (concat ["" "Needs:"] (map #(str "  " %) lines))))

(defn details
  "What --help says a mutation book does, generated from its spec."
  [{:keys [select confirm precondition announce verify pin prerequisites]}]
  (concat
   ["What it does:"
    (str "  picks " (picks select))
    (str "  asks to confirm: " (:action confirm)
         (when (:destructive? confirm) " (destructive - type yes)"))]
   (when (or precondition pin)
     ["  checks again right before acting that what was confirmed still holds"])
   (when announce ["  announces the change to the changelog"])
   [(if verify
      "  verifies the result before it reports success"
      "  has no post-check")]
   (needs prerequisites)))

(defn book
  "Builds and runs a standalone mutating runbook from a declarative spec (see the
   namespace doc). To compose several confirmed steps in one playbook, define each
   step's spec and call `run!` per step from a bespoke runbook/execute! :action."
  [{:keys [description options prerequisites] :as spec}]
  (runbook/execute!
   {:description description
    :options (or options {})
    :prerequisites prerequisites
    :details (details spec)
    :action (fn [opts {:keys [announcement-data]}] (run! spec opts announcement-data))}))
