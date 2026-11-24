;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.argocd
  "Detectives for Argo CD: Applications whose conditions report an error,
   whose health is Degraded or Missing, whose last sync failed or never
   finishes, and whose automated sync does not converge, plus ApplicationSets
   whose generators fail."
  (:require [gumshoe.conditions :as conditions]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def application-type "applications.argoproj.io")
(def applicationset-type "applicationsets.argoproj.io")

(def ^:private stuck-minutes
  "How long a sync may run before it counts as stuck - a sync waits on hooks
   and on health, so this leaves room for a slow rollout."
  30)

(def ^:private settle-minutes
  "How long after its last sync an automated Application may stay OutOfSync
   before it counts as not converging."
  10)

(def condition-severity
  "The severity of each Application condition type. The *Error types stop
   Argo CD from comparing, syncing or deleting the Application; the warnings
   describe resources it cannot cleanly own."
  {"InvalidSpecError" :critical
   "ComparisonError" :critical
   "SyncError" :critical
   "DeletionError" :critical
   "UnknownError" :critical
   "SharedResourceWarning" :warning
   "RepeatedResourceWarning" :warning
   "OrphanedResourceWarning" :info
   "ExcludedResourceWarning" :info})

(defn automated?
  "Whether automated sync is on: an automated block whose enabled flag is not
   false."
  [application]
  (let [automated (-> application :spec :syncPolicy :automated)]
    (boolean (and (map? automated) (not (false? (:enabled automated)))))))

(defn condition-findings
  [application]
  (for [{:keys [type message lastTransitionTime]} (-> application :status :conditions)
        :let [severity (condition-severity type)]
        :when severity]
    (onset/since
     {:severity severity
      :component (kubectl/namespace-name-of application)
      :summary (format "Application reports %s" type)
      :hint message}
     lastTransitionTime)))

(defn health-finding
  [application]
  (let [{:keys [status message]} (-> application :status :health)
        base {:component (kubectl/namespace-name-of application)}]
    (case status
      "Degraded" (assoc base :severity :critical :summary "Application is Degraded"
                        :hint (or (not-empty message) "a resource it manages is unhealthy - argocd app get shows which"))
      "Missing" (assoc base :severity :critical :summary "Application is Missing"
                       :hint "its resources do not exist in the cluster - it was never synced, or they were deleted")
      nil)))

(defn sync-findings
  [application now]
  (let [{:keys [phase message startedAt finishedAt retryCount]} (-> application :status :operationState)
        sync-status (-> application :status :sync :status)
        component (kubectl/namespace-name-of application)
        running-minutes (conditions/minutes-since startedAt now)
        settled-minutes (conditions/minutes-since finishedAt now)]
    (remove nil?
            [(when (contains? #{"Failed" "Error"} phase)
               (onset/since
                {:severity :critical
                 :component component
                 :summary (str "Application's last sync "
                               (if (= "Error" phase) "errored" "failed")
                               (when (and retryCount (pos? retryCount)) (format " after %d retries" retryCount)))
                 :hint message}
                (or finishedAt startedAt)))
             (when (and (contains? #{"Running" "Terminating"} phase) running-minutes (>= running-minutes stuck-minutes))
               (onset/since
                {:severity :warning
                 :component component
                 :summary (format "Application's sync has been %s for %d minutes" phase running-minutes)
                 :hint (str (some-> message not-empty (str " - "))
                            "a hook or a resource's health check holds it - argocd app get shows which; argocd app terminate-op ends it")}
                startedAt))
             (when (and (= "OutOfSync" sync-status) (automated? application)
                        (not (contains? #{"Running" "Terminating" "Failed" "Error"} phase))
                        (or (nil? phase) (nil? settled-minutes) (>= settled-minutes settle-minutes)))
               {:severity :warning
                :component component
                :summary "Application stays OutOfSync although automated sync is on"
                :hint (if (true? (-> application :spec :syncPolicy :automated :selfHeal))
                        "automated sync should have applied it - check the application controller's logs"
                        "selfHeal is off, so changes made in the cluster are not reverted - sync it, or turn selfHeal on")})])))

(defn detect-applications
  [evidence]
  (let [now (:now evidence)]
    (mapcat (fn [application]
              (concat (condition-findings application)
                      (keep identity [(health-finding application)])
                      (sync-findings application now)))
            (kubectl/items-of (get evidence application-type)))))

(defn detect-applicationsets
  [evidence]
  (for [applicationset (kubectl/items-of (get evidence applicationset-type))
        {:keys [type status message reason lastTransitionTime]} (-> applicationset :status :conditions)
        :when (and (= "ErrorOccurred" type) (= "True" status))]
    (onset/since
     {:severity :critical
      :component (kubectl/namespace-name-of applicationset)
      :summary (format "ApplicationSet cannot generate its Applications (%s)" reason)
      :hint (str message " - the Applications it already generated stay as they are")}
     lastTransitionTime)))

(def detectives
  [{:name "argocd-applications"
    :description "Argo CD Applications with error conditions, Degraded or Missing health, failed or stuck syncs, or automated syncs that do not converge"
    :requires [application-type]
    :detect detect-applications}
   {:name "argocd-applicationsets"
    :description "Argo CD ApplicationSets whose generators fail"
    :requires [applicationset-type]
    :detect detect-applicationsets}])
