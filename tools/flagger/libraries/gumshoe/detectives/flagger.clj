;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.flagger
  "Detectives for Flagger (flagger.app): Canaries whose analysis failed and
   rolled back, analyses that are failing checks right now, Canaries that
   never finish initializing, and rollouts held at a confirmation gate."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.onset :as onset]))

(def canary-type "canaries.flagger.app")

(def ^:private initializing-minutes 15)
(def ^:private gate-minutes 60)

(defn- minutes-since
  [value now]
  (when-let [at (try (some-> value java.time.Instant/parse) (catch Exception _ nil))]
    (when now (.toMinutes (java.time.Duration/between at ^java.time.Instant now)))))

(defn- promoted
  [canary]
  (first (filter #(= "Promoted" (:type %)) (-> canary :status :conditions))))

(defn- target
  [canary]
  (let [{:keys [kind name]} (-> canary :spec :targetRef)]
    (str (or kind "Deployment") " " name)))

(defn canary-findings
  [canary now]
  (let [{:keys [phase failedChecks canaryWeight lastTransitionTime]} (:status canary)
        threshold (-> canary :spec :analysis :threshold)
        condition (promoted canary)
        component (kubectl/namespace-name-of canary)
        since (minutes-since lastTransitionTime now)
        message (not-empty (:message condition))]
    (cond
      (-> canary :spec :suspend) nil

      (= "Failed" phase)
      [(onset/since
        {:severity :critical
         :component component
         :summary (format "Canary analysis failed - %s was rolled back" (target canary))
         :hint (str (some-> message (str " - "))
                    "the primary still serves the previous version; Flagger retries when the target's spec changes again - its events and the metric checks say which one failed")}
        lastTransitionTime)]

      (and (= "Progressing" phase) failedChecks (pos? failedChecks))
      [{:severity :warning
        :component component
        :summary (if threshold
                   (format "Canary analysis has failed %d of %d allowed checks (canary at %d%% traffic)" failedChecks threshold (or canaryWeight 0))
                   (format "Canary analysis has failed %d checks (canary at %d%% traffic)" failedChecks (or canaryWeight 0)))
        :hint "it rolls back when the failures reach the threshold - the metric checks and webhooks of spec.analysis say which one fails"}]

      (and (= "Initializing" phase) since (>= since initializing-minutes))
      [(onset/since
        {:severity :warning
         :component component
         :summary (format "Canary has been Initializing for %d minutes" since)
         :hint (str (some-> message (str " - "))
                    "Flagger waits for " (target canary) " and its primary to become ready")}
        lastTransitionTime)]

      (and (contains? #{"Waiting" "WaitingPromotion"} phase) since (>= since gate-minutes))
      [(onset/since
        {:severity :info
         :component component
         :summary (format "Canary has waited %d minutes at its %s gate" since
                          (if (= "Waiting" phase) "confirm-rollout" "confirm-promotion"))
         :hint "a webhook of that type in spec.analysis keeps answering no - approve it, or check the gate service"}
        lastTransitionTime)]

      :else nil)))

(defn detect-canaries
  [evidence]
  (mapcat #(canary-findings % (:now evidence)) (kubectl/items-of (get evidence canary-type))))

(def detectives
  [{:name "flagger-canaries"
    :description "Flagger Canaries that rolled back, are failing checks, never initialize, or wait at a gate"
    :requires [canary-type]
    :detect detect-canaries}])
