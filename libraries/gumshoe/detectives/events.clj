;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.events
  "Detective for cluster signals: Warning events from the last hour, grouped
   so a flapping component shows up once with a count instead of a hundred
   times."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(def ^:private recent-window (java.time.Duration/ofHours 1))

(defn- component-of
  [event]
  (let [object (:involvedObject event)]
    (if (:namespace object)
      (format "%s/%s" (:namespace object) (:name object))
      (:name object))))

(defn- recent?
  [now event]
  (when-let [timestamp (or (:lastTimestamp event) (:eventTime event))]
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
        grouped (group-by (juxt component-of :reason) warnings)]
    (for [[[component reason] events] grouped
          :let [total (reduce + (map #(or (:count %) 1) events))]]
      {:severity :warning
       :component component
       :summary (format "%s seen %dx in the last hour" reason total)
       :hint (truncate (:message (last events)) 120)})))

(def detectives
  [{:name "events"
    :description "Warning events from the last hour, grouped by component and reason"
    :requires ["events"]
    :detect detect-warning-events}])
