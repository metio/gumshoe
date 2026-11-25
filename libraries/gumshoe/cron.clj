;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cron
  "How often a cron schedule fires, as an upper bound. A detective that asks
   whether a CronJob stopped running needs the gap between two runs; an
   estimate that errs long only makes it slower to complain, while one that
   errs short raises false alarms, so every rule here rounds up."
  (:require [clojure.string :as str]))

(def ^:private macros
  {"@yearly" (java.time.Duration/ofDays 366)
   "@annually" (java.time.Duration/ofDays 366)
   "@monthly" (java.time.Duration/ofDays 31)
   "@weekly" (java.time.Duration/ofDays 7)
   "@daily" (java.time.Duration/ofDays 1)
   "@midnight" (java.time.Duration/ofDays 1)
   "@hourly" (java.time.Duration/ofHours 1)})

;; the CronJob controller's parser (robfig/cron) reads ? as * in any field
(defn- every?-field [field] (contains? #{"*" "?"} field))

(defn- step
  "n for a */n field, else nil. n may exceed a long; the callers clamp it to
   the field's range."
  [field]
  (some-> (re-find #"^\*/(\d+)$" field) second biginteger))

(defn- zero-step?
  "Does any item of the field step by zero? robfig/cron refuses such a
   schedule, so it never fires and has no interval."
  [field]
  (boolean (some #(re-find #"/0+$" %) (str/split field #","))))

(defn- day-at-most-28?
  "Can the day-of-month field only name days every month has? Only plain days
   and ranges with an explicit end count; a step from a start (5/7) runs to the
   month's end, and anything else is assumed to reach day 31."
  [field]
  (every? (fn [item]
            (when-let [[_ start end] (re-matches #"(\d+)(?:-(\d+))?(?:/\d+)?" item)]
              (and (not (and (nil? end) (str/includes? item "/")))
                   (<= (parse-long (or end start)) 28))))
          (str/split field #",")))

(defn- may-be-february?
  "Can the month field name February? A numeric item answers exactly; a named
   one only when it is FEB alone, and a named range or a step is assumed to
   reach it."
  [field]
  (boolean
   (some (fn [item]
           (if-let [[_ start end] (re-matches #"(\d+)(?:-(\d+))?(?:/\d+)?" item)]
             (let [start (parse-long start)
                   end (cond end (parse-long end)
                             (str/includes? item "/") 12
                             :else start)]
               (<= start 2 end))
             (or (re-find #"(?i)feb" item)
                 (re-find #"[-/*?]" item))))
         (str/split field #","))))

(defn- clamped
  "A */n step as the longest gap it leaves in a field that wraps after
   `range` units: it fires at 0, n, 2n ... within the range and again at 0, so
   the gap is n, or the whole range once n reaches it."
  [n range]
  (long (min n range)))

(defn interval
  "The longest gap between two runs of a schedule, or nil for a schedule it
   cannot read. Any restriction on a coarser field than the one that repeats
   makes the gap that coarser unit: a restricted month means a year, a day of
   month a month, a weekday a week, an hour a day. A day of month that may be
   29, 30 or 31 skips the months without it, so its gap is two months, and
   with a restricted month that may be February (29 February) it is eight
   years, the gap across a century year that is no leap year."
  [schedule]
  (let [schedule (str/trim (str schedule))
        schedule (str/replace schedule #"^(?:CRON_TZ|TZ)=\S+\s+" "")]
    (or (get macros schedule)
        (let [fields (str/split schedule #"\s+")]
          (when (and (= 5 (count fields)) (not-any? zero-step? fields))
            (let [[minute hour day-of-month month day-of-week] fields
                  day-restricted? (not (every?-field day-of-month))
                  late-day? (and day-restricted? (not (day-at-most-28? day-of-month)))]
              (cond
                (and (not (every?-field month)) late-day? (may-be-february? month))
                (java.time.Duration/ofDays (* 8 366))
                (not (every?-field month)) (java.time.Duration/ofDays 366)
                late-day? (java.time.Duration/ofDays 62)
                day-restricted? (java.time.Duration/ofDays 31)
                (not (every?-field day-of-week)) (java.time.Duration/ofDays 7)
                (step hour) (java.time.Duration/ofHours (clamped (step hour) 24))
                (not (every?-field hour)) (java.time.Duration/ofDays 1)
                (step minute) (java.time.Duration/ofMinutes (clamped (step minute) 60))
                (not (every?-field minute)) (java.time.Duration/ofHours 1)
                :else (java.time.Duration/ofMinutes 1))))))))
