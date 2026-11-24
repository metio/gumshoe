;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.onset
  "When a finding's condition began. A finding may carry :since, an ISO-8601
   instant string, so a report can tell a problem of five minutes from one of
   five days. Detectives set it from what the API records - a condition's
   lastTransitionTime, a deletionTimestamp, a creation time - and leave it out
   when nothing records the start.")

(defn instant
  "The Instant value names: an Instant, an ISO-8601 string, or a Date (as
   clj-yaml reads an unquoted timestamp). nil for anything else, including a
   string that does not parse."
  [value]
  (cond
    (instance? java.time.Instant value) value
    (instance? java.util.Date value) (.toInstant ^java.util.Date value)
    (string? value) (try (java.time.Instant/parse value) (catch Exception _ nil))
    :else nil))

(defn since
  "The finding with :since set to the instant value names, or unchanged when
   value is nil or not an instant."
  [finding value]
  (if-let [at (instant value)]
    (assoc finding :since (str at))
    finding))

(defn earliest
  "The earliest readable :since among findings, as a string, or nil."
  [findings]
  (some->> findings (keep (comp instant :since)) seq (apply min-key #(.toEpochMilli ^java.time.Instant %)) str))

(defn label
  "How long a finding has held, read at now: minutes under an hour, hours under
   two days, then the date it began. nil when since is unreadable or later
   than now."
  [since now]
  (let [at (instant since)]
    (when (and at now (not (.isAfter ^java.time.Instant at now)))
      (let [minutes (.toMinutes (java.time.Duration/between at now))]
        (cond
          (< minutes 60) (format "for %dm" minutes)
          (< minutes (* 48 60)) (format "for %dh" (quot minutes 60))
          :else (format "since %s" (subs (str at) 0 10)))))))
