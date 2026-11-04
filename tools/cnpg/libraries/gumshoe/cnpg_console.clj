;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-console
  "Reading a CloudNativePG cluster's instances directly, for the question no
   detective should answer: when a cluster has no working primary, which
   instance should become one, and what does promoting it throw away.

   Every standby has replayed up to some point in the WAL stream. Promoting one
   discards everything the others received beyond it, so the choice is a
   trade between availability now and transactions already acknowledged. That
   is an operator's call. These functions lay out the positions and name the
   cost; they never decide, and the proposal they produce says so."
  (:require [clojure.string :as str]))

(defn parse-lsn
  "A PostgreSQL LSN (`125/27000000`) as a byte offset. nil for anything that is
   not one, so an empty or error reading never sorts as a position."
  [lsn]
  (when-let [[_ hi lo] (re-matches #"\s*([0-9A-Fa-f]+)/([0-9A-Fa-f]+)\s*" (str lsn))]
    (try
      (+ (* (Long/parseLong hi 16) 4294967296) (Long/parseLong lo 16))
      (catch Exception _ nil))))

(defn describe-bytes
  [bytes]
  (cond
    (nil? bytes) "unknown"
    (zero? bytes) "nothing"
    (< bytes 1048576) (format "%d bytes" bytes)
    (< bytes 1073741824) (format "%.1fMiB" (/ (double bytes) 1048576))
    :else (format "%.2fGiB" (/ (double bytes) 1073741824))))

(defn- position-of [reading] (parse-lsn (:receive-lsn reading)))

(defn positioned
  "Readings that reported a usable WAL position, furthest ahead first."
  [readings]
  (->> readings
       (filter position-of)
       (sort-by position-of >)))

(defn proposal
  "What the positions suggest, with the cost of acting on it.

   :candidate is the instance furthest ahead - the one promotion discards least
   from, not the one that is safe to promote. Promoting it discards nothing by
   construction, so the number worth stating is the other direction: :behind
   says what choosing each alternative instead would throw away. :ties names
   instances sharing the candidate's position, where the positions alone cannot
   choose."
  [readings]
  (let [ranked (positioned readings)
        best (first ranked)
        unreadable (remove :readable? readings)]
    (if (nil? best)
      {:candidate nil
       :unreadable (mapv :instance unreadable)
       :reason (if (seq readings)
                 "no instance reported a WAL position, so nothing here can rank them"
                 "no instance was read")}
      (let [top (position-of best)]
        {:candidate (:instance best)
         :lsn (:receive-lsn best)
         :in-recovery? (:in-recovery? best)
         :ties (mapv :instance (filter #(= top (position-of %)) (rest ranked)))
         :behind (mapv (fn [reading]
                         {:instance (:instance reading)
                          :bytes (- top (position-of reading))})
                       (rest ranked))
         :unreadable (mapv :instance unreadable)}))))

(defn proposal-lines
  "The proposal as lines for an operator to read. It states what is unknown as
   plainly as what is known: an unread instance may hold the furthest position
   of all, which would make the candidate below the wrong one."
  [{:keys [candidate lsn ties behind unreadable reason in-recovery?]}]
  (if (nil? candidate)
    [(str "no promotion candidate: " reason)]
    (into
     (cond-> [(format "furthest ahead: %s at %s" candidate lsn)]
       (seq ties)
       (conj (format "tied at the same position: %s - the positions cannot choose between them"
                     (str/join ", " ties)))
       (not in-recovery?)
       (conj (str candidate " does not report itself in recovery, so it may not be a standby - "
                  "check what it is before treating it as one")))
     (concat
      (for [{:keys [instance bytes]} behind
            :when (pos? bytes)]
        (format "promoting %s instead would discard %s that %s already received"
                instance (describe-bytes bytes) candidate))
      (when (seq unreadable)
        [(format "%s could not be read, and an unread instance may sit further ahead than %s"
                 (str/join ", " unreadable) candidate)])
      ["this is a proposal from WAL positions alone - it weighs no application, "
       "no backup and no disk headroom, and promoting is a write that needs your approval"]))))

(defn retaining-slots
  "Replication slots holding WAL for a consumer that is not reading it. An
   inactive slot with WAL still reserved grows the volume for as long as it
   exists, and with no `max_slot_wal_keep_size` there is no bound on it."
  [slots]
  (filter #(and (false? (:active %)) (= "reserved" (:wal-status %))) slots))

(defn slot-lines
  [slots]
  (for [{:keys [slot-name]} (retaining-slots slots)]
    (format "slot %s is inactive and still reserving WAL - it grows the volume until its consumer returns or it is dropped"
            slot-name)))
