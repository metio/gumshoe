;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.joins-support
  "Test support for checks that join several reads. A case names a check, the
   reads it must join and answered evidence on which it reports something;
   the helpers here derive the evidence with joined reads missing, judge it
   the way a scan does, and generate it for test.check. Adding a check to a
   case list is all a later fix needs to put it under the same table and
   property."
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [gumshoe.detective :as detective]))

(defn without
  "The evidence with each named read unanswered."
  [evidence reads]
  (reduce #(assoc %1 %2 nil) evidence reads))

(defn missing-read-cases
  "[label evidence] for each joined read missing alone, and for all of them
   missing at once, derived from one answered evidence map."
  [joins answered]
  (conj (mapv (fn [read] [(str "only " read " did not answer") (without answered [read])]) joins)
        ["no joined read answered" (without answered joins)]))

(defn judgement
  "What a scan makes of the check on this evidence: whether it is marked
   unread, and the findings run-detectives reports for it. A detect function
   that throws shows as a finding, as it does in a scan."
  [check evidence]
  (let [marked (detective/mark-unread [check] evidence)]
    {:unread? (boolean (:unread (first marked)))
     :findings (vec (detective/run-detectives marked evidence))}))

(defn judges-nothing?
  "Whether the check is unread and silent on this evidence, so it reports
   neither a pass nor a finding."
  [check evidence]
  (let [{:keys [unread? findings]} (judgement check evidence)]
    (and unread? (empty? findings))))

(defn- gen-sublist
  "A kubectl list document keeping a random subset of its items, so the
   property sees more shapes than the hand-written evidence. Other values -
   a vector of names, a map keyed by host - stay as they are."
  [value]
  (if (and (map? value) (sequential? (:items value)))
    (gen/fmap #(assoc value :items (vec %))
              (gen/let [kept (gen/vector gen/boolean (count (:items value)))]
                (keep-indexed (fn [i item] (when (nth kept i) item)) (:items value))))
    (gen/return value)))

(defn gen-varied
  "The answered evidence with every list document thinned at random."
  [answered]
  (gen/fmap #(into answered %)
            (apply gen/tuple (for [[k v] answered] (gen/fmap (fn [v'] [k v']) (gen-sublist v))))))

(defn gen-missing-joined-read
  "[check evidence] for one of the cases: its answered evidence, thinned, with
   a non-empty random subset of the case's joins unanswered."
  [cases]
  (gen/let [{:keys [check joins answered]} (gen/elements cases)
            evidence (gen-varied answered)
            missing (gen/such-that seq (gen/fmap #(vec (keep-indexed (fn [i read] (when (nth % i) read)) joins))
                                                 (gen/vector gen/boolean (count joins)))
                                   100)]
    [check (without evidence missing)]))

(defn no-missing-joined-read-judges
  "The property that no evidence with a missing joined read yields a finding
   or a pass for any of the cases' checks. Each case's :check may be swapped
   by the caller, which is how a positive control drops a check's :joins."
  [cases]
  (prop/for-all [[check evidence] (gen-missing-joined-read cases)]
    (judges-nothing? check evidence)))

(defn gen-answered
  "[check evidence] for one of the cases with every read answered and the
   list documents thinned, the evidence the property withholds reads from."
  [cases]
  (gen/let [{:keys [check answered]} (gen/elements cases)
            evidence (gen-varied answered)]
    [check evidence]))
