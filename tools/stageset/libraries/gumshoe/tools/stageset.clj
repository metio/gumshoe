;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.stageset
  "The StageSet tool package. StageSet is a Flux controller for ordered, gated,
   multi-stage delivery - it rolls a release out one stage at a time, proving each
   healthy before the next and gating on schedules, approvals, and error budgets.
   This plugin fills the :delivery scan scope (and so the cluster-wide scan) with
   StageSets that are not Ready or whose rollout is held, teaches the setup wizard
   the :stageset capability, and makes the StageSet CRDs drill-down subjects with a
   per-stage status probe - all through one plugin/provide!.

   A StageSet's edges follow its stages to the sources they build from, so a
   rollout that stalled leads to the artifact behind the stage that stalled it,
   and on to whatever produced that artifact."
  (:require [clojure.string :as str]
            [gumshoe.conditions :as conditions]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]
            [gumshoe.steps.flux :as flux-steps]
            [gumshoe.subject :as subject]))

(def stageset-type "stagesets.stages.metio.wtf")
(def stageinventory-type "stageinventories.stages.metio.wtf")

;; The labels the controller stamps on every StageInventory: its owning StageSet
;; and the stage it records. They are the only handle on a shard, whose own name
;; carries a hash of the (StageSet, stage, shard) tuple.
(def stage-set-label "stages.metio.wtf/stage-set")
(def stage-label "stages.metio.wtf/stage")

;; stagesetctl promote stamps "<stage>@<token>" here, and the controller copies
;; the token into status.stages[].lastHandledPromotion once it acted on it.
(def promote-annotation :stages.metio.wtf/promote)

(defn declared-stages
  "The stage names a StageSet declares, as its status reports them, else as
   its spec lists them."
  [stageset]
  (mapv :name (or (seq (-> stageset :status :stages))
                  (-> stageset :spec :stages))))

(def held-phases
  "The promotion phases in which the controller holds a stage at its gate:
   soaking, analysing, blocked by an analysis, restart or event check, or
   awaiting a manual promotion. A promote token addressed to a stage in one
   of them is consumed on the next reconcile."
  #{"Soaking" "Analyzing" "Blocked" "AwaitingManual"})

(def hold-reasons
  "The Ready reasons the controller writes while a promotion gate holds the
   rollout: Soaking for a soak or an analysis in progress, PromotionBlocked
   for a failed check, AwaitingPromotion for a manual gate."
  #{"Soaking" "PromotionBlocked" "AwaitingPromotion"})

(def ^:private direct-source-kinds
  "The sourceRef kinds the controller reads as the artifact itself, so the
   artifact's key in status.lastAttemptedRevisions is the reference's own
   namespace/name. Any other kind names a producer, whose ExternalArtifact
   carries a name of its own."
  #{nil "" "ExternalArtifact" "GitRepository" "OCIRepository" "Bucket"})

(defn- stage-spec
  [stageset stage]
  (first (filter #(= stage (:name %)) (-> stageset :spec :stages))))

(defn- stage-status
  [stageset stage]
  (first (filter #(= stage (:name %)) (-> stageset :status :stages))))

(defn current-revisions
  "The revisions the stage's source pins in the run the controller is on now
   (status.lastAttemptedRevisions): the one keyed by the source's
   namespace/name for a direct source, every pinned revision for a producer,
   whose artifact name the spec does not say. Empty when nothing is pinned."
  [stageset stage]
  (let [pinned (-> stageset :status :lastAttemptedRevisions)
        ref (:sourceRef (stage-spec stageset stage))
        key (keyword (str (or (not-empty (:namespace ref)) (kubectl/namespace-of stageset)) "/" (:name ref)))]
    (set (if (contains? direct-source-kinds (:kind ref))
           (keep identity [(get pinned key)])
           (vals pinned)))))

(defn hold
  "Why stage of stageset is not held at its promotion gate at the revision
   the controller is rolling out now, or nil when it is. Held means: the
   controller has observed the StageSet's current generation, the StageSet is
   not suspended, its Ready condition reports a promotion hold, the stage
   declares a promotion gate, its promotionState.phase is one of held-phases,
   and the revision it holds - its appliedRevision, or the abortedRevision a
   rollback set aside - is one the current run pins. A token addressed to a
   stage outside that state is not consumed now: the controller ignores it
   for a stage Promoted at this revision and keeps it armed, so it would
   promote the stage, skipping every gate, when the next revision reaches it."
  [stageset stage]
  (let [status (stage-status stageset stage)
        state (:promotionState status)
        phase (:phase state)
        ready (conditions/condition stageset "Ready")
        revisions (current-revisions stageset stage)
        held-revision (some revisions [(:appliedRevision status) (not-empty (:abortedRevision state))])]
    (cond
      (true? (-> stageset :spec :suspend))
      "the StageSet is suspended, so the controller would not act on the promotion"

      (not= (-> stageset :metadata :generation) (-> stageset :status :observedGeneration))
      "the controller has not observed the StageSet's current spec yet"

      (nil? (:promotion (stage-spec stageset stage)))
      (format "stage %s has no promotion gate" stage)

      (not (contains? held-phases phase))
      (format "stage %s is not held at its promotion gate (phase %s)" stage (or phase "none"))

      (nil? held-revision)
      (format "stage %s is held at a revision the rollout is no longer on (applied %s)"
              stage (or (:appliedRevision status) "none"))

      (not (and (= "False" (:status ready)) (contains? hold-reasons (:reason ready))))
      (format "the StageSet's Ready condition reports no promotion hold (%s)" (or (:reason ready) "none"))

      :else nil)))

(defn held-stages
  "The stages of stageset held at their promotion gate now (see hold), in
   declared order."
  [stageset]
  (filterv #(nil? (hold stageset %)) (declared-stages stageset)))

(defn promotion-refusal
  "Why `stage` of the StageSet named `target` must not be promoted, judged on
   the StageSet as read now; `requested` is the stage the flags named, when
   they named one: the StageSet no longer exists, declares no stages, no
   stage was chosen (the flags named one it does not hold at a gate), it no
   longer declares the stage chosen, or the stage is not held at its gate at
   the current revision (hold)."
  [target stageset stage requested]
  (let [stages (declared-stages stageset)]
    (cond
      (nil? stageset) (format "StageSet %s no longer exists" target)
      (empty? stages) (format "StageSet %s declares no stages" target)
      (and (nil? stage) requested (not (some #{requested} stages)))
      (format "StageSet %s declares no stage %s, only %s" target requested (str/join ", " stages))
      (and (nil? stage) requested) (format "StageSet %s: %s" target (hold stageset requested))
      (nil? stage) (format "no stage of StageSet %s is held at its promotion gate" target)
      (not (some #{stage} stages)) (format "StageSet %s no longer declares stage %s" target stage)
      (hold stageset stage) (format "StageSet %s: %s" target (hold stageset stage))
      :else nil)))

(defn armed-promotion
  "The stage the StageSet's promote annotation addresses, when it carries
   one, else nil."
  [stageset]
  (some->> (get-in stageset [:metadata :annotations promote-annotation])
           (re-matches #"([^@]+)@(.+)")
           second))

(defn disarm-command
  "The kubectl command that removes the promote annotation of the StageSet
   namespace/name, as an operator types it."
  [context namespace name]
  (format "kubectl --context=%s --namespace=%s annotate %s %s %s-"
          context namespace stageset-type name (subs (str promote-annotation) 1)))

(defn promotion-recovery
  "The guidance printed when a promotion failed: whether the promote
   annotation is still armed, as read now, and the command that removes it.
   An armed token the stage never consumed promotes it, skipping every gate,
   whenever the controller next holds it there, so it is named rather than
   left behind silently. stageset is nil when the read did not answer."
  [context target stageset stage]
  (let [{:keys [namespace name]} (kubectl/namespace-name-map target)
        command (disarm-command context namespace name)
        armed (armed-promotion stageset)]
    (cond
      (nil? stageset)
      (str (format "StageSet %s could not be read, so whether its promote annotation is still armed is unknown." target)
           "\nAn armed annotation promotes the stage it names, skipping every gate, when the controller next holds it at its gate."
           "\nCheck it, and remove it unless the promotion should still happen:\n  " command)

      (= stage armed)
      (str (format "The promote annotation of StageSet %s is STILL ARMED for stage %s: the controller has not consumed it." target stage)
           "\nWhile it stays, the controller promotes the stage, skipping soak, analysis and manual gates, the next time it holds it there - also for a later revision nobody reviewed."
           "\nRemove it unless the promotion should still happen:\n  " command)

      armed
      (format "The promote annotation of StageSet %s names stage %s, not %s. Remove it unless that promotion should happen:\n  %s"
              target armed stage command)

      :else
      (format "StageSet %s carries no promote annotation, so nothing is left armed." target))))

(defn promotion-handled?
  "Whether the stage acted on the promotion the StageSet's promote annotation
   addresses to it: the annotation names the stage and a token, and the
   stage's lastHandledPromotion is that token."
  [stageset stage]
  (let [[_ addressed token] (some->> (get-in stageset [:metadata :annotations promote-annotation])
                                     (re-matches #"([^@]+)@(.+)"))]
    (boolean (and (= stage addressed)
                  (some #(and (= stage (:name %)) (= token (:lastHandledPromotion %)))
                        (get-in stageset [:status :stages]))))))

;; The window override stagesetctl reconcile --update-now stamps; the
;; controller copies the value it honoured into status.lastHandledUpdateOverride
;; and clears status.pendingUpdate.
(def update-now-annotation "stages.metio.wtf/update-now")

(defn reconcile-annotations
  "The annotations that ask the StageSet to reconcile at `token`, as
   key=value arguments of kubectl annotate: requestedAt and, with
   `update-now?`, the window override, both carrying the same token."
  [token update-now?]
  (cond-> [(str flux-steps/requested-at "=" token)]
    update-now? (conj (str update-now-annotation "=" token))))

(defn reconcile-refusal
  "Why the StageSet named `target` must not be asked to reconcile, judged on
   the StageSet as read now: it no longer exists, or it is suspended and so
   will not act on the request."
  [target stageset]
  (cond
    (nil? stageset) (format "StageSet %s no longer exists" target)
    (true? (-> stageset :spec :suspend)) (format "StageSet %s is suspended; it will not act on a reconcile request" target)
    :else nil))

(defn reconcile-handled?
  "Whether the controller handled the reconcile request stamped at `token`:
   status.lastHandledReconcileAt is the token, and with `update-now?` the
   override with that token was honoured and no update is held any more.
   stagesetctl reconcile --wait waits for the same state. Readiness is not
   the test, since a StageSet Ready before the request stays Ready whether
   or not it was handled."
  [stageset token update-now?]
  (boolean
   (and (some? stageset)
        (flux-steps/handled? stageset token)
        (or (not update-now?)
            (and (= token (-> stageset :status :lastHandledUpdateOverride))
                 (nil? (-> stageset :status :pendingUpdate)))))))

;; A Ready=False reason says how loud the finding should be: hard failures are
;; critical, gated or budget-frozen states are warnings, and states that just mean
;; "waiting on a human or a soak window" are info - a rollout paused for approval
;; is not an outage.
(def reason-severity
  {"InvalidVersion" :critical
   "PreviousRevisionUnavailable" :critical
   "RollbackStoreFailed" :critical
   "DowngradeRequiresMigration" :critical
   "BudgetExhausted" :warning
   "BudgetSourceUnavailable" :warning
   "PromotionBlocked" :warning
   "RolledBack" :warning
   "AwaitingPromotion" :info
   "Soaking" :info})

(defn- stageset-subject
  [stageset]
  (subject/subject "StageSet" (kubectl/namespace-of stageset) (kubectl/name-of stageset)))

(defn detect-stageset-problems
  [evidence]
  (for [stageset (kubectl/items-of (get evidence stageset-type))
        :let [ready (conditions/condition stageset "Ready")]
        :when (= "False" (:status ready))]
    {:severity (get reason-severity (:reason ready) :warning)
     :component (kubectl/namespace-name-of stageset)
     ;; Naming the subject explicitly lets the operator drill from the finding
     ;; straight into the StageSet - and from there along its stages' sources to
     ;; whatever produced them - instead of the scan ending at a line of text.
     :subject (stageset-subject stageset)
     :summary (format "StageSet is not Ready (%s)" (or (:reason ready) "unknown"))
     ;; The controller appends "(runbook: <url>)" to the condition message, so the
     ;; hint already carries a direct route to the remediation page.
     :hint (:message ready)}))

(defn detect-held-updates
  "A revision held by an update window is not a failure - it is the controller
   doing its job - but it is worth surfacing so an operator is not surprised a new
   version has not rolled yet."
  [evidence]
  (for [stageset (kubectl/items-of (get evidence stageset-type))
        :let [pending (-> stageset :status :pendingUpdate)]
        :when pending]
    {:severity :info
     :component (kubectl/namespace-name-of stageset)
     :subject (stageset-subject stageset)
     :summary "a new revision is held by the update window"
     ;; nextWindowOpens is a field of the pendingUpdate record, not of status -
     ;; read one level too high it is always absent and the finding never says
     ;; when the wait ends, which is the whole point of surfacing it.
     :hint (when-let [opens (:nextWindowOpens pending)]
             (str "next window opens: " opens))}))

(def detectives
  [{:name "stagesets"
    :description "StageSets that are not Ready or whose rollout is held"
    :requires [stageset-type]
    :detect (fn [evidence]
              (concat (detect-stageset-problems evidence)
                      (detect-held-updates evidence)))}])

(defn- source-edge
  "One stage's (or the migration ladder's) source, as an edge. A SourceReference
   with no kind means an ExternalArtifact; any other kind names the producer
   behind one, which the controller resolves through the artifact's back-pointer -
   so following the edge lands on the object the author actually wrote."
  [relation namespace ref]
  (when (:name ref)
    {:relation relation
     :subject (subject/subject (or (:kind ref) "ExternalArtifact")
                               (or (:namespace ref) namespace)
                               (:name ref))}))

(defn stageset-edges
  "Every source a StageSet builds from, one edge per stage. This is the first hop
   of the delivery chain: on to the ExternalArtifact, then - via the flux
   package's back-pointer edge - to whatever produced it, so 'stage two never
   went healthy' walks all the way to the object that rendered its manifests."
  [stageset]
  (let [namespace (kubectl/namespace-of stageset)]
    (vec
     (keep identity
           (concat
            (for [stage (-> stageset :spec :stages)]
              (source-edge (format "stage '%s' builds from" (:name stage))
                           namespace (:sourceRef stage)))
            [(source-edge "migrations from" namespace
                          (-> stageset :spec :migrationsSourceRef :sourceRef))])))))

(defn inventory-edges
  "The StageInventories a StageSet applied through, labelled with the stage each
   one records. Nothing about them can be derived from the StageSet - a shard's
   name is hashed - so this asks the cluster by label, and answers 'what did this
   actually apply' by making every shard a subject you can walk into."
  [context {:keys [namespace name]} _stageset]
  (->> (kubectl/get-selected context stageinventory-type (str stage-set-label "=" name))
       (kubectl/items-of)
       (filter #(= namespace (kubectl/namespace-of %)))
       (sort-by kubectl/name-of)
       (map (fn [inventory]
              (let [stage (get-in inventory [:metadata :labels (keyword stage-label)])]
                {:relation (if stage (format "stage '%s' applied via" stage) "applied via")
                 :subject (subject/subject "StageInventory" namespace (kubectl/name-of inventory))})))))

(defn stageset-facts
  [stageset]
  (let [status (:status stageset)
        stages (:stages status)]
    [["stages" (when (seq stages)
                 (str/join ", " (map #(format "%s: %s" (:name %) (:phase %)) stages)))]
     ["version" (:version status)]
     ["pending version" (:pendingVersion status)]
     ["held until" (-> status :pendingUpdate :nextWindowOpens)]]))

(plugin/provide!
 {:detectives {:delivery detectives}
  :capabilities {:stageset #(kubectl/serves-crd? stageset-type)}
  :kinds {"StageSet" {:type stageset-type :edges stageset-edges}
          "StageInventory" {:type stageinventory-type}}
  :facts {"StageSet" stageset-facts}
  :fetched-edges {"StageSet" inventory-edges}
  :probes [{:key :stageset-status :label "🎬 StageSet per-stage progress"
            :kinds #{"StageSet"} :tools ["stagesetctl"]
            :args (fn [_context {:keys [namespace name]}]
                    ["stagesetctl" "get" name (str "--namespace=" namespace)])}]})
