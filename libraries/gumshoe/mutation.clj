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
      :subject  {:type \"cronjobs\"                ; the object the change acts on, see below
                 :parse pick/namespaced          ; optional, the pick as a ref
                 :invalid \"...\"                 ; optional, why a pick that does not parse is refused
                 :facts (fn [object] -> value)}  ; optional, joins the pin
      :refusal  (fn [ctx] -> reason or nil)       ; optional, re-checked after confirm;
                                                  ; a reason is printed as \"refusing: <reason>\"
      :precondition (fn [ctx] -> bool)            ; optional, re-checked after confirm
                                                  ; and after :refusal;
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
                                    ; see gumshoe.playbook and below
      :connect  (fn [opts] -> {:context .. :cluster .. :announce-as ..} or nil)
                                    ; optional, what the change acts on when that
                                    ; is not the kube context, see below
      :announce? false  ; optional, handed to the runbook harness; a book that
                        ; changes nothing anyone else needs to hear of (a debug
                        ; session) runs as a read-only book does
      :confirmed? true}  ; set by a playbook whose confirmation covered this step

   The :refusal, :precondition, :announce, :effect, :verify, :derive and :items
   callbacks each receive one context map, so a book destructures exactly what
   it needs:
     {:context .. :cluster .. :target <the pick(s)> :opts <parsed flags>
      :ref .. :object ..}   ; with a :subject

   :subject declares the one object a change acts on. :parse turns the pick
   into a ref {:type :namespace :name}, plus whatever else the pick carries
   (a kind, a revision); it defaults to gumshoe.pick/namespaced for a
   :namespaced select and gumshoe.pick/cluster-scoped for a :one select, and
   :type fills in a ref whose parser names no type. A :many select picks
   several objects and takes no :subject. The engine then hands every
   callback the ref as :ref and the object as :object, read strictly before
   :derive runs; pins the ref, with :facts, unless the spec names its own
   :pin; and reads the object again right before :refusal and :precondition,
   so both judge the object as it is when the change is about to run. A pick
   that does not parse is refused with :invalid, read nothing, and leaves
   :ref and :object nil for the callbacks that run before the refusal.
   read-subject reads the object again, for a post-check.

   :refusal returns why the change must not run, or nil. The engine prints
   the reason as \"refusing: <reason>\" and stops, and only a change no
   refusal stopped reaches :precondition.

   The subject read, :derive, :refusal and :precondition run under
   kubectl/*strict-reads*, as the
   candidate query of :select and every post-check do. A read in :derive that
   did not answer stops the change before the confirmation, so neither the
   precondition nor the effect is computed from a read that stood in for an
   object nobody could see. A read in :precondition that did not answer
   refuses the change with the reason. A read that answered that its object
   or resource type does not exist still returns nil, which the precondition
   judges. Only the gumshoe.kubectl readers that honour *strict-reads* are
   strict here (see its docstring).

   :connect names what the change acts on when that is not the cluster of
   the kube context: an SSH host, a storage cluster's API. It receives the
   parsed flags and returns :context, which the candidate query, every
   callback and the effect receive in place of the kube context; :cluster,
   the name the confirmation shows the change acting on; and :announce-as,
   the system the changelog names, defaulting to :cluster. It returns nil
   when what it names cannot be reached, having said why, and the change
   stops before anything is listed. Without :connect the context is the
   current kube context and :cluster its cluster. :subject and :pin read
   their object with kubectl, so a spec with :connect takes neither.

   A change without :recovery that fails after applying an effect - a later
   effect failed, its post-check did not hold, or an error stopped it - is
   still followed by guidance: `fallback-guidance` names the effects that
   stayed applied, where it stopped, and where to look (the messages of the
   failure, the changelog entry it posted, the object on its cluster). It is
   recorded with gumshoe.outcome/fallback!, so the guidance of a playbook
   running the change as a step wins over it, and a rehearsal records none.

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
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.prerequisites :as prerequisites]
            [gumshoe.select :as select]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.announce :as announce]
            [gumshoe.outcome :as outcome]
            [gumshoe.pick :as pick]
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

(defn- refuse!
  "Prints why the change is refused and returns false, the verdict of a
   precondition that refused."
  [why]
  (println (str "refusing: " why))
  false)

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
             (if why (refuse! why) true))))))

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
            (refuse! (str "could not read what the precondition rests on - " (ex-message e)))
            (throw e)))))))

(defn subject-ref
  "The ref {:type :namespace :name ..} a :subject makes of target, or nil
   when target does not parse into an object with a type and a name."
  [{:keys [type parse]} select target]
  (when (= :many (:mode select))
    (throw (ex-info "a :subject needs a select that picks one object, not a :many select"
                    {:select select})))
  (let [parse (or parse (if (= :namespaced (:mode select)) pick/namespaced pick/cluster-scoped))
        ref (some->> (parse target) (merge (when type {:type type})))]
    (when (and (:type ref) (:name ref)) ref)))

(defn read-subject
  "The object the ref of ctx names, read now, or nil without a ref. A
   post-check calls it to see the subject after the change."
  [{:keys [context ref]}]
  (when ref (fetch-pinned context ref)))

(defn- invalid-pick
  [{:keys [invalid]} select target]
  (or invalid (format "the pick %s does not name a %s" (pr-str target) (:label select))))

(defn- subject-derive
  "derive preceded by the subject read, as one function of the base context:
   {:ref :object} merged with what derive returns for the context holding
   them. nil when the spec has neither."
  [{:keys [subject select derive]}]
  (cond
    subject (fn [{:keys [target] :as base}]
              (let [ref (subject-ref subject select target)
                    values {:ref ref :object (read-subject (assoc base :ref ref))}]
                (merge values (when derive (derive (merge base values))))))
    derive derive
    :else nil))

(defn- judged-precondition
  "The :refusal and :precondition of a spec as one thunk over ctx, or nil
   when the spec has neither and no :subject. With a :subject, a pick that
   does not parse is refused, and both judge the object read again."
  [{:keys [subject select refusal precondition]} ctx]
  (when (or subject refusal precondition)
    (fn []
      (let [{:keys [ref target]} ctx
            judge? (or refusal precondition)
            ctx (if (and subject ref judge?) (assoc ctx :object (read-subject ctx)) ctx)
            why (cond
                  (and subject (nil? ref)) (invalid-pick subject select target)
                  refusal (refusal ctx))]
        (cond
          why (refuse! why)
          precondition (boolean (precondition ctx))
          :else true)))))

(defn- pin-of
  "The ref the change pins and its fingerprint at the confirmation: the
   spec's :pin when it names one, else the subject's ref, whose object ctx
   already holds from the strict read before the confirmation."
  [{:keys [pin subject]} {:keys [context ref object] :as ctx}]
  (cond
    pin (when-let [pinned (pin ctx)]
          {:ref pinned :before (fingerprint (fetch-pinned context pinned) (:facts pinned))})
    (and subject ref) (let [pinned (cond-> ref (:facts subject) (assoc :facts (:facts subject)))]
                        {:ref pinned :before (fingerprint object (:facts pinned))})
    :else nil))

(defn- connect
  "{:context :cluster :announce-as} for the spec: what its :connect returns
   for opts, or nil when that could not be reached; the kube context and its
   cluster without :connect."
  [{:keys [connect subject pin]} opts]
  (if connect
    (do (when (or subject pin)
          (throw (ex-info "a :subject or :pin reads its object with kubectl, so a spec with :connect takes neither"
                          {:subject subject})))
        (when-let [connection (connect opts)]
          (update connection :announce-as #(or % (:cluster connection)))))
    (let [context (kubectl/current-context)
          cluster (kubectl/current-cluster)]
      {:context context :cluster cluster :announce-as cluster})))

(defn fallback-guidance
  "The guidance under the FAILED banner of a change that declares no
   :recovery and applied an effect before it failed. `applied` are the
   effects that stayed applied, as gumshoe.effect/describe renders them;
   `stopped` the effect it stopped at, when one failed; `post-check-failed?`
   whether its post-check did not hold; `announced?` whether it posted to the
   changelog; `subject` what it acted on and `cluster` where."
  [{:keys [applied stopped post-check-failed? announced? subject cluster]}]
  (str/join "\n"
            (concat ["This change declares no recovery guidance. What it left behind:"
                     (format "  %d %s of this change stayed applied - nothing was rolled back:"
                             (count applied) (if (= 1 (count applied)) "effect" "effects"))]
                    (map #(str "    " %) applied)
                    (cond
                      stopped [(str "  it stopped at: " stopped)]
                      post-check-failed? ["  its post-check did not hold"]
                      :else ["  an unexpected error stopped it"])
                    ["Look at:"]
                    (cond
                      stopped ["  the messages of the effect that failed, above"]
                      post-check-failed? ["  the post-check messages above"]
                      :else ["  the error above"])
                    (when announced? ["  the changelog entry this change posted"])
                    [(str "  the current state of " subject " on " cluster)])))

(defn- left-behind
  "fallback-guidance for a change that failed after running the first
   effects of `plan`, counting from `applied-before`, the effects the run had
   applied when the change began; nil when none of its effects that ran
   changes anything, and outside a run, which records nothing. effect/run!
   applies a plan in order and stops at the first failure, so the applied
   effects are the plan's first ones."
  [{:keys [select announce] :as spec} {:keys [cluster target]} plan applied-before]
  (let [{:keys [applied stopped verified]} (outcome/current)
        changed (->> (when applied plan)
                     (take (- (or applied 0) applied-before))
                     (remove #(= :note (first %)))
                     (mapv effect/describe))]
    (when (seq changed)
      (fallback-guidance {:applied changed
                          :stopped stopped
                          :post-check-failed? (false? verified)
                          :announced? (boolean (and announce (not (false? (:announce? spec)))))
                          :subject (str/join ", " (select/items (:mode select) target))
                          :cluster cluster}))))

(defn- run-connected!
  [{:keys [select confirm announce effect verify] :as spec}
   {:keys [context cluster announce-as]} target opts announcement-data]
  (let [base {:context context :cluster cluster :target target
              :opts opts :announcement-data announcement-data}
        {:keys [values unread]} (derived (subject-derive spec) base)]
    (if unread
      (do (stdout/error (format "could not read what the change rests on - %s" unread))
          false)
      (let [ctx (merge base values)
            applied-before (outcome/applied-count)
            plan (volatile! nil)
            failed! #(when-not flow/*dry-run*
                       (if (:recovery spec)
                         (outcome/recovery! (:recovery spec) ctx)
                         (when-let [guidance (left-behind spec ctx (effect/as-plan @plan) applied-before)]
                           (outcome/fallback! guidance ctx))))
            ok? (try
                  (let [{:keys [ref before]} (pin-of spec ctx)
                        items-fn (:items spec)]
                    (flow/change!
                     {:confirmation {:action (:action confirm)
                                     :target cluster
                                     :items (if items-fn (items-fn ctx) (select/items (:mode select) target))
                                     :destructive? (:destructive? confirm)}
                      :precondition (strict-precondition
                                     (pinned-precondition (judged-precondition spec ctx)
                                                          ref before #(fetch-pinned context %)))
                      :confirmed? (:confirmed? spec)
                      :announce! (when announce
                                   #(announce/announce! announce-as announcement-data (announce ctx)))
                      :effect (vreset! plan (effect ctx))
                      :step (or (:title spec) (:action confirm))
                      :post-checks (when verify (verify ctx))}))
                  (catch Throwable e (failed!) (throw e)))]
        (when-not ok? (failed!))
        ok?))))

(defn run-on!
  "Runs a mutation spec against an ALREADY-CHOSEN `target`, skipping the spec's own
   selection, through derive -> confirm -> [precondition] -> announce -> execute
   -> verify. Returns true/false, false without asking for a confirmation when a
   read in :derive did not answer. This is what a playbook calls per step after picking its
   subject once (e.g. via gumshoe.select/pick), so the operator is not re-prompted
   for every step. `run!` is this plus selection.

   `opts` is the parsed CLI flags; `announcement-data` is the value the runbook
   action receives in its ctx (nil when the book does not announce)."
  [spec target opts announcement-data]
  (if-let [connection (connect spec opts)]
    (run-connected! spec connection target opts announcement-data)
    false))

(defn run!
  "Selects the spec's subject (interactive, honouring the CLI flags) and runs it
   through run-on!. Returns true when there is nothing to select (its
   :empty-message is shown), false when what :connect names could not be
   reached, the candidates could not be read or the operator picks nothing,
   otherwise the
   change's result. Use run! for a single confirmed step; use run-on! to compose
   several steps against one already-picked subject."
  [{:keys [select empty-message] :as spec} opts announcement-data]
  (if-let [connection (connect spec opts)]
    (let [{:keys [candidates target unread]} (select/resolve-target select opts (:context connection))]
      (cond
        unread (select/unread! select unread)

        (empty? candidates)
        (do (stdout/ok (or empty-message "nothing to do")) true)

        (select/nothing-selected? (:mode select) target)
        (do (stdout/error (format "no %s selected" (:label select))) false)

        :else
        (run-connected! spec connection target opts announcement-data)))
    false))

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
  [{:keys [select confirm precondition refusal subject announce verify pin prerequisites]}]
  (concat
   ["What it does:"
    (str "  picks " (picks select))
    (str "  asks to confirm: " (:action confirm)
         (when (:destructive? confirm) " (destructive - type yes)"))]
   (when (or precondition refusal subject pin)
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
   (cond-> {:description description
            :options (or options {})
            :prerequisites prerequisites
            :details (details spec)
            :action (fn [opts {:keys [announcement-data]}] (run! spec opts announcement-data))}
     (contains? spec :announce?) (assoc :announce? (:announce? spec)))))
