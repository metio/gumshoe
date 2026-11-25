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

(def ^:private quantity-pattern
  ;; The grammar apimachinery's resource.ParseQuantity accepts: an optional
  ;; sign, a decimal that may start or end with its point ('.5', '1.'), then
  ;; either a decimal exponent ('129e6', '1E3', '5e-1') or an SI suffix. A bare
  ;; 'E' with no digits after it is the exa suffix; with digits it is an
  ;; exponent.
  #"([+-]?(?:\d+(?:\.\d*)?|\.\d+))(?:([eE][+-]?\d+)|([a-zA-Z]*))")

(defn quantity->number
  "A Kubernetes quantity as a double, or nil when it can not be parsed. A value
   too large for a double (an exponent of thousands) is nil as well, so a
   comparison never runs against infinity."
  [value]
  (when-let [[_ number exponent suffix] (re-matches quantity-pattern (str/trim (str value)))]
    (let [result (if exponent
                   (Double/parseDouble (str number exponent))
                   (when-let [factor (get suffix-factors suffix)]
                     (* (Double/parseDouble number) factor)))]
      (when (and result (Double/isFinite result))
        result))))

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
