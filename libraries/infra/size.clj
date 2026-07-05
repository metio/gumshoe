;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.size
  "Parsing the byte sizes nginx and PHP use in their configuration ('512M',
   '2G', '100k', '-1' for unlimited) into comparable numbers, and rendering
   bytes back into something an operator reads at a glance. Both nginx and PHP
   treat the k/m/g suffixes as powers of 1024."
  (:require [clojure.string :as str]))

(def ^:private suffix-factors
  {"" 1 "k" 1024 "m" 1048576 "g" 1073741824})

(defn parse
  "A size as a byte count, the keyword :unlimited for '-1', or nil when it can
   not be parsed. '0' is treated as :unlimited, matching PHP's post_max_size."
  [value]
  (let [text (str/lower-case (str/trim (str value)))]
    (cond
      (= "-1" text) :unlimited
      (= "0" text) :unlimited
      :else (when-let [[_ number suffix] (re-matches #"(\d+)\s*([kmg]?)b?" text)]
              (when-let [factor (get suffix-factors suffix)]
                (* (parse-long number) factor))))))

(defn human
  "Bytes as a compact human string; passes :unlimited and nil straight through
   as words."
  [value]
  (cond
    (= :unlimited value) "unlimited"
    (nil? value) "unknown"
    (< value 1024) (format "%dB" value)
    (< value 1048576) (format "%.0fK" (/ value 1024.0))
    (< value 1073741824) (format "%.0fM" (/ value 1048576.0))
    :else (format "%.1fG" (/ value 1073741824.0))))

(defn smaller?
  "Is a strictly smaller than b, treating :unlimited as larger than anything?
   nil (unknown) never compares as smaller, so unknowns never raise a warning."
  [a b]
  (cond
    (or (nil? a) (nil? b)) false
    (= :unlimited a) false
    (= :unlimited b) true
    :else (< a b)))

(defn minimum
  "The most restrictive real limit among the values, ignoring nil and treating
   :unlimited as no constraint. nil when nothing constrains."
  [values]
  (let [limits (remove #(or (nil? %) (= :unlimited %)) values)]
    (when (seq limits)
      (apply min limits))))
