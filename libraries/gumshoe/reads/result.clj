;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.reads.result
  "Read results that keep an absence apart from a read that did not answer,
   whatever was read: a Kubernetes object, a DNS record, a mail header.

   A source - a classifier such as gumshoe.reads for kubectl or
   gumshoe.dns/classify for dig - turns one raw answer into a result map in
   exactly one of three states:

     {:state :present :ref ref ...}            the thing exists
     {:state :absent  :ref ref ...}            the source answered: none
     {:state :unread  :ref ref :reason \"why\"}  no answer, nothing is known

   An object result carries :object on :present. A list result carries
   :items on :present (one or more) and :absent (none, an empty vector).
   A source may add keys of its own (:status, :unserved).

   The ref names what was read and is the source's own data. describe turns
   it into the words an operator reads; a ref without a :source is described
   as a type, a namespace and a name, and a source with other words for its
   refs adds a method for its :source.

   Polarity. Which state a caller acts on decides whether an unanswered read
   is safe:

   | caller                               | proceeds on | test with       |
   |--------------------------------------|-------------|-----------------|
   | asserts a presence (precondition,    | :present    | present?        |
   |   post-check that X exists)          |             |                 |
   | confirms an absence (post-check that | :absent     | absent?         |
   |   X is gone, gate on an absence)     |             |                 |
   | skips a thing (acts when present,    | :present    | on-state, with  |
   |   does nothing when absent)          | / :absent   | :unread refused |

   (not (present? r)) is not an absence and (not (absent? r)) is not a
   presence: both include :unread.

   Unread is never a value. object!, items! and value! throw an ex-info for
   an :unread result whose ex-data carries both :gumshoe.reads/unread and
   :gumshoe.kubectl/unread, so an unanswered read cannot reach a string, a
   manifest or a comparison as if it were data. unread-exception? recognises
   either key; the strict-read machinery of gumshoe.verify and
   gumshoe.mutation dispatches on it, so an unanswered read of any source
   holds a change.

   A check that rests on several reads at once - an address and its PTR
   records, every selector of a DKIM setup - combines them with combined,
   which is only as good as its worst read, or keeps them apart as a map of
   results and asks first-unread which one did not answer.

   This namespace depends on nothing else in gumshoe, so any source and
   every detective can require it."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; the exception a read that did not answer throws

(def unread-key
  "The source-neutral ex-data key of a read that did not answer."
  :gumshoe.reads/unread)

(def kubectl-unread-key
  "The ex-data key gumshoe.kubectl's strict reads have always thrown with.
   Every unread exception built here carries it beside unread-key, so a
   catcher written against it holds for every source."
  :gumshoe.kubectl/unread)

(defn unread-exception?
  "Whether e is an ex-info marking a read that did not answer: its ex-data
   carries :gumshoe.reads/unread or :gumshoe.kubectl/unread."
  [e]
  (let [data (ex-data e)]
    (boolean (or (get data unread-key) (get data kubectl-unread-key)))))

(defn unread-exception
  "An ex-info for a read that did not answer, carrying both unread keys
   merged over data."
  ([message] (unread-exception message nil))
  ([message data]
   (ex-info message (merge data {unread-key true kubectl-unread-key true}))))

;; ---------------------------------------------------------------------------
;; states and predicates

(def states
  "The three states a result can be in."
  #{:present :absent :unread})

(defn result?
  "Whether r is a result map in one of the three states. A sorted map is
   never one: looking up :state in a map sorted by strings throws, and
   evidence holds such maps beside results."
  [r]
  (and (map? r) (not (sorted? r)) (contains? states (:state r))))

(defn state-of
  "The state of a result; anything that is not a result map in one of the
   three states counts as :unread, so a nil or malformed value never passes
   as an answer."
  [r]
  (if (result? r) (:state r) :unread))

(defn present?
  "True only when the read answered and the thing exists (a list: one or
   more matched). The test for a caller that asserts a presence. False for
   :absent and for :unread - so (not (present? r)) does NOT mean the thing
   is absent."
  [r]
  (= :present (state-of r)))

(defn absent?
  "True only when the read answered that no such thing exists (a list: it
   answered with none). The test for a caller that confirms a deletion or
   gates on an absence. False for :present and for :unread - so
   (not (absent? r)) does NOT mean the thing exists."
  [r]
  (= :absent (state-of r)))

(defn unread?
  "True when the read did not answer, and for anything that is not a result
   map in one of the three states. The branch a caller must refuse or report
   on: never read it as absent nor as present."
  [r]
  (= :unread (state-of r)))

(defn answered?
  "True for :present and :absent: the source answered and the result says
   what it holds."
  [r]
  (not (unread? r)))

(defn on-state
  "Calls the branch for the result's state with the result and returns what
   it returns. branches must hold :present, :absent and :unread, each a
   function of one argument; a missing branch throws, so a caller cannot
   forget the read that did not answer. A malformed result takes the
   :unread branch."
  [r branches]
  (let [missing (remove #(fn? (get branches %)) [:present :absent :unread])]
    (when (seq missing)
      (throw (IllegalArgumentException.
              (str "reads/on-state needs a function for every state; missing "
                   (str/join ", " missing)))))
    ((get branches (state-of r)) r)))

;; ---------------------------------------------------------------------------
;; building results

(defn present
  "A :present result for ref with extra merged in (:object, :items, ...)."
  ([ref] (present ref nil))
  ([ref extra] (merge {:state :present :ref ref} extra)))

(defn absent
  "An :absent result for ref with extra merged in (:items [], ...)."
  ([ref] (absent ref nil))
  ([ref extra] (merge {:state :absent :ref ref} extra)))

(defn unread
  "An :unread result for ref, carrying reason - the source's own word for
   why it did not answer - and extra merged in."
  ([ref reason] (unread ref reason nil))
  ([ref reason extra] (merge {:state :unread :ref ref :reason reason} extra)))

;; ---------------------------------------------------------------------------
;; describing what was read

(defn- describe-object
  [{:keys [type namespace name selector]}]
  (str type
       (cond
         (and name namespace) (str " " namespace "/" name)
         name (str " " name)
         (= :all namespace) " in every namespace"
         namespace (str " in " namespace)
         :else "")
       (when selector (str " matching " selector))))

(defmulti describe
  "The read a ref names, in words an operator knows. A ref without a
   :source reads as a type, a namespace and a name: \"deployments web/api\",
   \"nodes worker-1\", \"pods in web\", \"pods in every namespace\". A source
   whose refs read otherwise adds a method for its :source value."
  :source)

(defmethod describe :default
  [ref]
  (describe-object ref))

;; ---------------------------------------------------------------------------
;; extractors

(defn- did-not-answer
  [r]
  (let [reason (if (map? r) (:reason r) (str "not a read result: " (pr-str r)))]
    (unread-exception (format "could not read %s: %s"
                              (if (map? r) (describe (:ref r)) "an object")
                              (or reason "the read did not answer"))
                      {:gumshoe.reads/result r})))

(defn object!
  "The object of a :present result, nil for an :absent one. Throws an
   unread exception for an :unread result, so the object of a read that did
   not answer is never nil by accident."
  [r]
  (case (state-of r)
    :present (:object r)
    :absent nil
    :unread (throw (did-not-answer r))))

(defn items!
  "The items of an answered list result: a vector, empty for :absent.
   Throws an unread exception for an :unread result, so a list that did not
   answer never reads as an empty one."
  [r]
  (case (state-of r)
    (:present :absent) (vec (:items r))
    :unread (throw (did-not-answer r))))

(defn derived
  "The result with :value set to what f makes of its answer, state kept.
   For an object result f receives the object, and only a :present result
   gets a :value. For a list result f receives the items, and every answered
   result gets one (f [] for :absent). An :unread result is returned
   unchanged: f never sees a read that did not answer."
  [r f]
  (let [s (state-of r)]
    (cond
      (= :unread s) r
      (contains? r :items) (assoc r :value (f (vec (:items r))))
      (= :present s) (assoc r :value (f (:object r)))
      :else r)))

(defn value!
  "The :value of a derived result: nil for an :absent object result, and
   throws an unread exception for an :unread one."
  [r]
  (if (unread? r)
    (throw (did-not-answer r))
    (:value r)))

;; ---------------------------------------------------------------------------
;; several reads at once

(defn first-unread
  "The first result that did not answer, nil when every one answered. The
   result keeps its :ref and :reason, so unread-refusal names the read that
   failed rather than the check built on it."
  [results]
  (first (filter unread? results)))

(defn- answer-of
  [r]
  (if (contains? r :value) (:value r) (if (contains? r :items) (vec (:items r)) (:object r))))

(defn combined
  "One result for a check that rests on several reads: the first that did
   not answer, or a :present result for ref whose :value is f applied to
   each answered result's answer in order - its :value when derived, else
   its :items, else its :object. Read it with value!."
  [ref results f]
  (or (first-unread results)
      (present ref {:value (apply f (map answer-of results))})))

(defn unread-in
  "The entries of a map of results whose result did not answer, as
   [key result] pairs in the map's order. For evidence that keeps several
   reads apart (one per nameserver, one per selector), where some answering
   is worth judging and the rest is named as unread."
  [results]
  (filterv (comp unread? val) results))

;; ---------------------------------------------------------------------------
;; refusals, one per polarity: each returns a line for a :refusal or a
;; precondition, or nil when the caller may proceed

(defn- unread-line
  [r what because]
  (format "could not read %s%s - %s, so this refuses rather than act on a read that did not answer"
          what
          (if-let [reason (and (map? r) (:reason r))] (str " (" reason ")") "")
          (or because "acting on a wrong answer could do harm")))

(defn- what-of
  [r what]
  (or what (when (map? r) (describe (:ref r))) "the object"))

(defn unread-line-of
  "\"could not read <what> (<reason>)\" for a result, without a refusal's
   tail: the summary of a finding that reports a read as unread. nil for an
   answered result."
  ([r] (unread-line-of r nil))
  ([r what]
   (when (unread? r)
     (str "could not read " (what-of r what)
          (when-let [reason (and (map? r) (:reason r))] (str " (" reason ")"))))))

(defn unread-refusal
  "A refusal line when the read did not answer, nil when it answered either
   way. opts: :what names the thing as the operator knows it (default: the
   ref described), :because what acting on a wrong answer would do."
  ([r] (unread-refusal r nil))
  ([r {:keys [what because]}]
   (when (unread? r)
     (unread-line r (what-of r what) because))))

(defn refusal-unless-present
  "nil only when the thing exists; a refusal line when the read answered
   that it does not, and when it did not answer. opts as unread-refusal."
  ([r] (refusal-unless-present r nil))
  ([r {:keys [what] :as opts}]
   (case (state-of r)
     :present nil
     :absent (format "%s does not exist" (what-of r what))
     :unread (unread-refusal r opts))))

(defn refusal-unless-absent
  "nil only when the read answered that the thing does not exist; a refusal
   line when it exists, and when the read did not answer. opts as
   unread-refusal."
  ([r] (refusal-unless-absent r nil))
  ([r {:keys [what] :as opts}]
   (case (state-of r)
     :absent nil
     :present (format "%s still exists" (what-of r what))
     :unread (unread-refusal r opts))))
