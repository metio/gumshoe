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
            [gumshoe.investigation :as investigation]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]
            [gumshoe.subject :as subject]))

(def stageset-type "stagesets.stages.metio.wtf")
(def stageinventory-type "stageinventories.stages.metio.wtf")

;; The label the controller stamps on every StageInventory with its owning
;; StageSet's name - the only handle on a shard, whose own name carries a hash.
(def stage-set-label "stages.metio.wtf/stage-set")

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

(defn- ready-condition
  [resource]
  (first (filter #(= "Ready" (:type %)) (-> resource :status :conditions))))

(defn- stageset-subject
  [stageset]
  (subject/subject "StageSet" (kubectl/namespace-of stageset) (kubectl/name-of stageset)))

(defn detect-stageset-problems
  [evidence]
  (for [stageset (kubectl/items-of (get evidence stageset-type))
        :let [ready (ready-condition stageset)]
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
  :probes [{:key :stageset-status :label "🎬 StageSet per-stage progress"
            :kinds #{"StageSet"} :tools ["stagesetctl"]
            :args (fn [_context {:keys [namespace name]}]
                    ["stagesetctl" "get" name (str "--namespace=" namespace)])}
           ;; A StageSet's inventories are found by label, which an edge (pure
           ;; over the object) can not do - the shard names are hashed, so there
           ;; is nothing to derive. A probe asks the cluster instead, and answers
           ;; "what did this actually apply" without leaving the drill-down.
           {:key :stageset-inventories :label "📒 StageInventories of this StageSet"
            :kinds #{"StageSet"} :tools ["kubectl"]
            :args (fn [context {:keys [namespace name]}]
                    (investigation/with-context
                     context namespace "get" stageinventory-type
                     (str "--selector=" stage-set-label "=" name)))}]})
