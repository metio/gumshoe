;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.events
  "Detective for cluster signals: Warning events from the last hour, grouped
   so a flapping component shows up once with a count instead of a hundred
   times. Each finding names the object the events are about, so a symptom
   folds under the finding that explains it."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.subject :as subject]))

(def ^:private recent-window (java.time.Duration/ofHours 1))

(defn- component-of
  [event]
  (let [object (:involvedObject event)]
    (if (:namespace object)
      (format "%s/%s" (:namespace object) (:name object))
      (:name object))))

(defn- last-seen
  "When the event last happened. An event written through events.k8s.io sets
   eventTime once and records its repeats in series, so series wins."
  [event]
  (or (-> event :series :lastObservedTime) (:lastTimestamp event) (:eventTime event)))

(defn- occurrences
  [event]
  (or (-> event :series :count) (:count event) 1))

(defn- recent?
  [now event]
  (when-let [timestamp (last-seen event)]
    (.isAfter (java.time.Instant/parse timestamp) (.minus now recent-window))))

(defn- truncate
  [text limit]
  (let [text (str/replace (str text) #"\s+" " ")]
    (if (> (count text) limit)
      (str (subs text 0 limit) "…")
      text)))

(defn detect-warning-events
  [evidence]
  (let [now (:now evidence)
        warnings (filter #(and (= "Warning" (:type %)) (recent? now %))
                         (kubectl/items-of (get evidence "events")))
        grouped (group-by (juxt #(-> % :involvedObject :kind) component-of :reason) warnings)]
    (for [[[kind component reason] events] grouped
          :let [total (reduce + (map occurrences events))
                object (:involvedObject (last events))]]
      (cond-> {:severity :warning
               :component component
               :summary (format "%s seen %dx in the last hour" reason total)
               :hint (truncate (:message (last events)) 120)}
        (and kind (:name object)) (assoc :subject (subject/subject kind (:namespace object) (:name object)))))))

(def detectives
  [{:name "events"
    :description "Warning events from the last hour, grouped by component and reason"
    :requires ["events"]
    :detect detect-warning-events}])
