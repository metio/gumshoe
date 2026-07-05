;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.tools.stageset
  "The StageSet tool package. StageSet is a Flux controller for ordered, gated,
   multi-stage delivery - it rolls a release out one stage at a time, proving each
   healthy before the next and gating on schedules, approvals, and error budgets.
   This plugin fills the :delivery scan scope (and so the cluster-wide scan) with
   StageSets that are not Ready or whose rollout is held, teaches the setup wizard
   the :stageset capability, and makes the StageSet CRDs drill-down subjects with a
   per-stage status probe - all through one plugin/provide!."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.plugin :as plugin]))

(def stageset-type "stagesets.stages.metio.wtf")
(def stageinventory-type "stageinventories.stages.metio.wtf")

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

(defn detect-stageset-problems
  [evidence]
  (for [stageset (kubectl/items-of (get evidence stageset-type))
        :let [ready (ready-condition stageset)]
        :when (= "False" (:status ready))]
    {:severity (get reason-severity (:reason ready) :warning)
     :component (kubectl/namespace-name-of stageset)
     :summary (format "StageSet is not Ready (%s)" (or (:reason ready) "unknown"))
     :hint (:message ready)}))

(defn detect-held-updates
  "A revision held by an update window is not a failure - it is the controller
   doing its job - but it is worth surfacing so an operator is not surprised a new
   version has not rolled yet."
  [evidence]
  (for [stageset (kubectl/items-of (get evidence stageset-type))
        :when (-> stageset :status :pendingUpdate)]
    {:severity :info
     :component (kubectl/namespace-name-of stageset)
     :summary "a new revision is held by the update window"
     :hint (when-let [opens (-> stageset :status :nextWindowOpens)]
             (str "next window opens: " opens))}))

(def detectives
  [{:name "stagesets"
    :description "StageSets that are not Ready or whose rollout is held"
    :requires [stageset-type]
    :detect (fn [evidence]
              (concat (detect-stageset-problems evidence)
                      (detect-held-updates evidence)))}])

(plugin/provide!
 {:detectives {:delivery detectives}
  :capabilities {:stageset #(kubectl/serves-crd? stageset-type)}
  :kinds {"StageSet" {:type stageset-type}
          "StageInventory" {:type stageinventory-type}}
  :probes [{:key :stageset-status :label "🎬 StageSet per-stage progress"
            :kinds #{"StageSet"} :tools ["stagesetctl"]
            :args (fn [_context {:keys [namespace name]}]
                    ["stagesetctl" "get" name (str "--namespace=" namespace)])}]})
