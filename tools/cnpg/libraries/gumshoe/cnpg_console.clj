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
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

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

(defn- rank
  "The positions alone, furthest ahead first, with the cost of each alternative."
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

(defn proposal
  "What the positions suggest, with the cost of acting on it.

   A cluster whose operator reports a healthy primary gets no candidate at all.
   Promoting a standby over a working primary is not a repair, and the positions
   invite exactly that: a primary is not in recovery, so it has no receive LSN
   and ranks below every standby that does.

   Otherwise :candidate is the instance furthest ahead - the one promotion
   discards least from, not the one that is safe to promote. Promoting it
   discards nothing by construction, so the number worth stating is the other
   direction: :behind says what choosing each alternative instead would throw
   away. :ties names instances sharing the candidate's position, where the
   positions alone cannot choose."
  ([readings] (proposal readings nil))
  ([readings primary]
   (if (and (:name primary) (:healthy? primary))
     {:candidate nil
      :primary (:name primary)
      :reason (format (str "the cluster reports %s as a healthy primary, so no promotion is "
                           "called for - promoting a standby over a working primary discards "
                           "everything the primary holds beyond it")
                      (:name primary))}
     (rank readings))))

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

(defn current-primary
  "The instance a cluster reports as its primary, looked up by \"namespace/name\"
   in an already-fetched list. A promotion's post-check turns on this, and a
   lookup that silently finds nothing would report every promotion as failed, so
   it lives here under test rather than inline in the book."
  [clusters namespace-name]
  (-> (kubectl/find-item clusters namespace-name) :status :currentPrimary))

(defn retaining-slots
  "Replication slots holding WAL for a consumer that is not reading it. Both
   `reserved` and `extended` are still held: `extended` is past max_wal_size and
   kept anyway, which is the state a slot passes through on its way to filling
   the volume."
  [slots]
  (filter #(and (false? (:active %)) (contains? #{"reserved" "extended"} (:wal-status %)))
          slots))

(defn lost-slots
  "Slots whose WAL has been released. The consumer cannot resume from one of
   these, whatever its own state - it needs re-cloning from the primary."
  [slots]
  (filter #(= "lost" (:wal-status %)) slots))

(defn slot-state
  "What one slot means for the operator, as :lost, :retaining, :expiring,
   :behind or nil for a slot that needs no comment. A slot is read by its
   wal_status first: `lost` is terminal whether or not a consumer is attached."
  [{:keys [active wal-status]}]
  (cond
    (= "lost" wal-status) :lost
    (and (false? active) (= "unreserved" wal-status)) :expiring
    (false? active) (when (contains? #{"reserved" "extended"} wal-status) :retaining)
    (= "unreserved" wal-status) :expiring
    (= "extended" wal-status) :behind))

(defn- slot-line
  [{:keys [slot-name instance]} state]
  (case state
    :lost (format (str "slot %s on %s is lost - its WAL has been released, so the standby it "
                       "belongs to cannot resume and needs re-cloning from the primary")
                  slot-name instance)
    :retaining (format (str "slot %s on %s is inactive and still holding WAL - it grows the "
                            "volume until its consumer returns, it is dropped, or "
                            "max_slot_wal_keep_size invalidates it")
                       slot-name instance)
    :expiring (format (str "slot %s on %s is unreserved - its WAL is next to be released, "
                           "after which the standby needs re-cloning")
                      slot-name instance)
    :behind (format (str "slot %s on %s is holding WAL past max_wal_size for a consumer that "
                         "is reading - the standby is behind, not gone")
                    slot-name instance)))

(defn describe-safe-wal-size
  "What the headroom column should read. An empty value means two different
   things: no cap configured, or no cap that applies - a lost slot reports none
   because the WAL is already gone."
  [{:keys [wal-status safe-wal-size]}]
  (cond
    (not (str/blank? safe-wal-size)) safe-wal-size
    (= "lost" wal-status) "n/a"
    :else "unbounded"))

(defn slot-lines
  [slots]
  (for [slot slots
        :let [state (slot-state slot)]
        :when state]
    (slot-line slot state)))
