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
      :confirm  {:action \"...\" :destructive? false}
      :precondition (fn [ctx] -> bool)            ; optional, re-checked after confirm;
                                                  ; reads only, a dry run calls it too
      :announce (fn [ctx] -> changelog message)   ; optional
      :effect   (fn [ctx] -> effect plan)
      :verify   (fn [ctx] -> seq of post-check maps)   ; optional
      :derive   (fn [ctx] -> map)   ; optional, values merged into ctx once so a
                                    ; per-run value is shared by :effect and :verify
      :items    (fn [ctx] -> seq)   ; optional, overrides the confirmation items
      :pin      (fn [ctx] -> {:type :name :namespace? :facts?})  ; optional, see below
      :confirmed? true}  ; set by a playbook whose confirmation covered this step

   The :precondition, :announce, :effect, :verify, :derive and :items callbacks
   each receive one context map, so a book destructures exactly what it needs:
     {:context .. :cluster .. :target <the pick(s)> :opts <parsed flags>}

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
   it with :facts (fn [object] -> value), which must be equal too."
  (:require [gumshoe.flow :as flow]
            [gumshoe.select :as select]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.announce :as announce]
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
   (fn [ref] -> object); before is the fingerprint taken at confirmation."
  [precondition ref before fetch]
  (if-not ref
    precondition
    (fn []
      (and (if precondition (precondition) true)
           (let [why (drift before (fingerprint (fetch ref) (:facts ref)))]
             (when why (println (str "refusing: " why)))
             (nil? why))))))

(defn run-on!
  "Runs a mutation spec against an ALREADY-CHOSEN `target`, skipping the spec's own
   selection, through confirm -> [precondition] -> announce -> execute -> verify.
   Returns true/false. This is what a playbook calls per step after picking its
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
        ctx (merge base (when derive (derive base)))
        ref (when-let [pin (:pin spec)] (pin ctx))
        before (when ref (fingerprint (fetch-pinned context ref) (:facts ref)))
        items-fn (:items spec)]
    (flow/change!
     {:confirmation {:action (:action confirm)
                     :target cluster
                     :items (if items-fn (items-fn ctx) (select/items (:mode select) target))
                     :destructive? (:destructive? confirm)}
      :precondition (pinned-precondition (when precondition #(precondition ctx))
                                         ref before #(fetch-pinned context %))
      :confirmed? (:confirmed? spec)
      :announce! (when announce
                   #(announce/announce! cluster announcement-data (announce ctx)))
      :effect (effect ctx)
      :post-checks (when verify (verify ctx))})))

(defn run!
  "Selects the spec's subject (interactive, honouring the CLI flags) and runs it
   through run-on!. Returns true when there is nothing to select (its
   :empty-message is shown), false when the operator picks nothing, otherwise the
   change's result. Use run! for a single confirmed step; use run-on! to compose
   several steps against one already-picked subject."
  [{:keys [select empty-message] :as spec} opts announcement-data]
  (let [context (kubectl/current-context)
        {:keys [candidates target]} (select/resolve-target select opts context)]
    (cond
      (empty? candidates)
      (do (stdout/ok (or empty-message "nothing to do")) true)

      (select/nothing-selected? (:mode select) target)
      (do (stdout/error (format "no %s selected" (:label select))) false)

      :else
      (run-on! spec target opts announcement-data))))

(defn book
  "Builds and runs a standalone mutating runbook from a declarative spec (see the
   namespace doc). To compose several confirmed steps in one playbook, define each
   step's spec and call `run!` per step from a bespoke runbook/execute! :action."
  [{:keys [description options prerequisites] :as spec}]
  (runbook/execute!
   {:description description
    :options (or options {})
    :prerequisites prerequisites
    :action (fn [opts {:keys [announcement-data]}] (run! spec opts announcement-data))}))
