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

(defn- instant-of
  [timestamp]
  (when timestamp
    (try (java.time.Instant/parse timestamp) (catch Exception _ nil))))

(defn- last-seen
  "When the event last happened. An event written through events.k8s.io sets
   eventTime once and records its repeats in series, so series wins."
  [event]
  (instant-of (or (-> event :series :lastObservedTime) (:lastTimestamp event) (:eventTime event))))

(defn- first-seen
  "When the event first happened: firstTimestamp for a core/v1 event, and
   eventTime for one written through events.k8s.io, whose series counts the
   repeats since then."
  [event]
  (instant-of (or (:firstTimestamp event) (:eventTime event))))

(defn- occurrences
  "How often the event happened since first-seen. Both count and series.count
   are lifetime totals, which can span days."
  [event]
  (or (-> event :series :count) (:count event) 1))

(defn- window-start
  [now]
  (.minus ^java.time.Instant now ^java.time.Duration recent-window))

(defn- recent?
  [now event]
  (when-let [at (last-seen event)]
    (.isAfter ^java.time.Instant at (window-start now))))

(defn- within-window?
  "Whether every occurrence of the event lies inside the window: it happened
   once, or it was first seen inside it."
  [now event]
  (or (= 1 (occurrences event))
      (when-let [at (first-seen event)]
        (not (.isBefore ^java.time.Instant at (window-start now))))))

(defn- earliest
  [instants]
  (when (seq instants) (apply min-key #(.toEpochMilli ^java.time.Instant %) instants)))

(defn- latest
  [instants]
  (when (seq instants) (apply max-key #(.toEpochMilli ^java.time.Instant %) instants)))

(defn occurrence-summary
  "How a group of events reads: the count in the last hour when every
   occurrence falls inside it, else the lifetime count since the group was
   first seen, so a week-long crash loop does not read as a week's count in
   one hour."
  [reason events now]
  (let [total (reduce + (map occurrences events))
        first-at (earliest (keep first-seen events))]
    (cond
      (every? #(within-window? now %) events)
      (format "%s seen %dx in the last hour" reason total)

      (and first-at (every? first-seen events))
      (format "%s seen %dx since %s, most recently in the last hour" reason total first-at)

      :else
      (format "%s seen %dx, most recently in the last hour (first occurrence not recorded)" reason total))))

(defn- truncate
  [text limit]
  (let [text (str/replace (str text) #"\s+" " ")]
    (if (> (count text) limit)
      (str (subs text 0 limit) "…")
      text)))

(defn detect-warning-events
  "Warning events last seen within the hour, one finding per object, kind and
   reason, newest last-seen first. Each finding carries :since, the group's
   first occurrence, and :last-seen."
  [evidence]
  (let [now (:now evidence)
        warnings (filter #(and (= "Warning" (:type %)) (recent? now %))
                         (kubectl/items-of (get evidence "events")))
        grouped (group-by (juxt #(-> % :involvedObject :kind) component-of :reason) warnings)]
    (for [[[kind component reason] events] (sort-by (fn [[k events]]
                                                      [(- (.toEpochMilli ^java.time.Instant (latest (map last-seen events))))
                                                       (str k)])
                                                    grouped)
          :let [newest (apply max-key #(.toEpochMilli ^java.time.Instant (last-seen %)) events)
                object (:involvedObject newest)]]
      (cond-> {:severity :warning
               :component component
               :summary (occurrence-summary reason events now)
               :hint (truncate (:message newest) 120)
               :last-seen (str (last-seen newest))}
        (earliest (keep first-seen events)) (assoc :since (str (earliest (keep first-seen events))))
        (and kind (:name object)) (assoc :subject (subject/subject kind (:namespace object) (:name object)))))))

(def detectives
  [{:name "events"
    :description "Warning events from the last hour, grouped by component and reason"
    :requires ["events"]
    :detect detect-warning-events}])
