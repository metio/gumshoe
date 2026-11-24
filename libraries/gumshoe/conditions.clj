;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.conditions
  "Reading the status conditions of a Kubernetes object. Every function is pure
   and takes the object as kubectl returns it.

   The semantics are the same everywhere:
   - an object holds at most one condition per type; when a list carries the
     same type twice, the first one counts, in `condition`, `by-type` and every
     predicate built on them;
   - a missing condition is neither True nor False nor Unknown: `holds?` and
     `status?` answer false for it whatever status is asked, so a check that
     wants \"anything but True\" writes `(not (holds? o t))`, and one that wants
     an explicit failure writes `(status? o t \"False\")`;
   - staleness is a separate question, asked with `stale?`: none of the
     predicates looks at observedGeneration on its own;
   - timestamps are read tolerantly: an absent or unparseable one yields nil,
     never an exception."
  (:require [clojure.string :as str]
            [gumshoe.onset :as onset]))

(defn all
  "The object's conditions, in the order the API returned them."
  [object]
  (-> object :status :conditions))

(defn condition
  "The object's condition of type, or nil when it has none."
  [object type]
  (first (filter #(= type (:type %)) (all object))))

(defn by-type
  "The object's conditions keyed by type, keeping the first of a repeated type."
  [object]
  (reduce (fn [acc c] (if (contains? acc (:type c)) acc (assoc acc (:type c) c)))
          {} (all object)))

(defn status?
  "Whether the object has a condition of type whose status is status. False
   when the condition is missing."
  [object type status]
  (= status (:status (condition object type))))

(defn holds?
  "Whether the object's condition of type has status True. False when the
   condition is missing, False or Unknown."
  [object type]
  (status? object type "True"))

(defn ready?
  "Whether the object's Ready condition is True."
  [object]
  (holds? object "Ready"))

(defn first-false
  "The first of types, in order, whose condition on the object has status
   False, or nil. Missing and Unknown conditions are skipped."
  [object types]
  (first (keep #(let [c (condition object %)] (when (= "False" (:status c)) c)) types)))

(defn- observed-generation
  [object condition]
  (or (:observedGeneration condition) (-> object :status :observedGeneration)))

(defn stale?
  "Whether the controller has not yet observed the object's current
   generation, so its conditions may describe an older spec. With a condition,
   the condition's own observedGeneration is read, falling back to the
   object's status.observedGeneration; without one, only the latter. False
   when either generation is not a number, since nothing then says the
   status is behind."
  ([object]
   (stale? object nil))
  ([object condition]
   (let [generation (-> object :metadata :generation)
         observed (if condition
                    (observed-generation object condition)
                    (-> object :status :observedGeneration))]
     (boolean (and (number? generation) (number? observed) (< observed generation))))))

(defn minutes-since
  "Whole minutes from the timestamp value (an ISO-8601 string, an Instant or a
   Date) to now, or nil when either is absent or value is unreadable."
  [value now]
  (when-let [at (onset/instant value)]
    (when now (.toMinutes (java.time.Duration/between at ^java.time.Instant now)))))

(defn first-line
  "The first line of a condition message, trimmed, or nil when it is blank."
  [text]
  (some-> text str str/split-lines first str/trim not-empty))

(defn ready-check
  "A post-check that passes once the object fetch returns has its condition
   of type (default Ready) True. fetch takes no arguments and reads the object
   afresh on every attempt."
  [{:keys [description fetch type timeout interval] :or {type "Ready"}}]
  (cond-> {:description description
           :check (fn [] (holds? (fetch) type))}
    timeout (assoc :timeout timeout)
    interval (assoc :interval interval)))
