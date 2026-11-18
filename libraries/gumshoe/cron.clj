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

(defn- every?-field [field] (= "*" field))

(defn- step
  "n for a */n field, else nil."
  [field]
  (some-> (re-find #"^\*/(\d+)$" field) second parse-long))

(defn interval
  "The longest gap between two runs of a schedule, or nil for a schedule it
   cannot read. Any restriction on a coarser field than the one that repeats
   makes the gap that coarser unit: a restricted month means a year, a day of
   month a month, a weekday a week, an hour a day."
  [schedule]
  (let [schedule (str/trim (str schedule))
        schedule (str/replace schedule #"^(?:CRON_TZ|TZ)=\S+\s+" "")]
    (or (get macros schedule)
        (let [fields (str/split schedule #"\s+")]
          (when (= 5 (count fields))
            (let [[minute hour day-of-month month day-of-week] fields]
              (cond
                (not (every?-field month)) (java.time.Duration/ofDays 366)
                (not (every?-field day-of-month)) (java.time.Duration/ofDays 31)
                (not (every?-field day-of-week)) (java.time.Duration/ofDays 7)
                (step hour) (java.time.Duration/ofHours (step hour))
                (not (every?-field hour)) (java.time.Duration/ofDays 1)
                (step minute) (java.time.Duration/ofMinutes (step minute))
                (not (every?-field minute)) (java.time.Duration/ofHours 1)
                :else (java.time.Duration/ofMinutes 1))))))))
