;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.quantity
  "Parsing Kubernetes resource quantities into comparable numbers. CPU comes
   out in cores ('100m' -> 0.1), memory in bytes ('128Mi' -> 134217728), and
   plain counts stay themselves. Anything unparseable is nil, so a surprising
   value degrades to 'unknown' instead of a crash."
  (:require [clojure.string :as str]))

(def ^:private suffix-factors
  {""   1.0
   "m"  1e-3
   "k"  1e3   "M"  1e6   "G"  1e9   "T"  1e12  "P"  1e15  "E"  1e18
   "Ki" 1024.0
   "Mi" 1048576.0
   "Gi" 1073741824.0
   "Ti" 1.099511627776E12
   "Pi" 1.125899906842624E15
   "Ei" 1.152921504606846976E18})

(defn quantity->number
  "A Kubernetes quantity as a double, or nil when it can not be parsed."
  [value]
  (when-let [[_ number suffix] (re-matches #"(\d+(?:\.\d+)?)([a-zA-Z]*)" (str/trim (str value)))]
    (when-let [factor (get suffix-factors suffix)]
      (* (Double/parseDouble number) factor))))

(defn sum
  "Sums a seq of quantities, skipping any that do not parse."
  [values]
  (reduce + 0.0 (keep quantity->number values)))

(defn ratio
  "used/total as a double, or nil when either side is unparseable or total is
   zero."
  [used total]
  (let [u (quantity->number used)
        t (quantity->number total)]
    (when (and u t (pos? t))
      (/ u t))))
