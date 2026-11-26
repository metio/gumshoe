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

(defn effective-lsn
  "The position a standby is ranked by, as CloudNativePG's instance manager
   computes it: the received LSN, replaced by the replayed one when that is
   further or the received one is empty. WAL restored from the archive
   advances replay but not receive, so a standby that fell back to
   restore_command can sit beyond its own receive LSN, or have none. The
   value is {:bytes :lsn}, nil when neither position parses."
  [{:keys [receive-lsn replay-lsn]}]
  (let [received (parse-lsn receive-lsn)
        replayed (parse-lsn replay-lsn)]
    (cond
      (and (nil? received) (nil? replayed)) nil
      (< (or received 0) (or replayed 0)) {:bytes replayed :lsn (str/trim replay-lsn)}
      :else {:bytes received :lsn (str/trim receive-lsn)})))

(defn- position-of [reading] (:bytes (effective-lsn reading)))

(defn- primary? [reading] (false? (:in-recovery? reading)))

(defn- compare-readings
  "The order CloudNativePG elects in (status.go, PostgresqlStatusList Less): an
   instance not in recovery first, by pod name; then standbys by effective
   position, then replayed position, furthest first; then by pod name."
  [a b]
  (cond
    (and (primary? a) (primary? b)) (compare (:instance a) (:instance b))
    (primary? a) -1
    (primary? b) 1
    :else (let [by-position (compare (position-of b) (position-of a))
                by-replay (compare (or (parse-lsn (:replay-lsn b)) 0)
                                   (or (parse-lsn (:replay-lsn a)) 0))]
            (cond
              (not (zero? by-position)) by-position
              (not (zero? by-replay)) by-replay
              :else (compare (:instance a) (:instance b))))))

(defn positioned
  "Readings that reported a usable WAL position, in the order a promotion
   would prefer them."
  [readings]
  (->> readings
       (filter :readable?)
       (filter position-of)
       (sort compare-readings)))

(defn- unpositioned
  "Readable instances that reported no position at all. They cannot be ranked,
   and leaving them out would read as if they did not exist."
  [readings]
  (mapv :instance (filter #(and (:readable? %) (nil? (position-of %))) readings)))

(defn- tied?
  "Two instances holding WAL up to the same point: promoting either discards
   the same, though the election order still prefers the one replayed further."
  [a b]
  (= (position-of a) (position-of b)))

(defn- rank
  "The positions alone, in election order, with the cost of each alternative.
   :behind holds every other positioned instance with the bytes choosing it
   would discard; a negative count is an instance further ahead than a
   candidate that ranks first because it is not in recovery, or, when that
   instance is not in recovery either, because its pod name sorts first.
   :out-of-recovery names every positioned instance not in recovery, in
   election order; more than one means more than one accepts writes."
  [readings]
  (let [ranked (positioned readings)
        best (first ranked)
        unreadable (mapv :instance (remove :readable? readings))
        without-position (unpositioned readings)]
    (if (nil? best)
      {:candidate nil
       :unreadable unreadable
       :unpositioned without-position
       :reason (if (seq readings)
                 "no instance reported a WAL position, so nothing here can rank them"
                 "no instance was read")}
      (let [top (position-of best)]
        {:candidate (:instance best)
         :lsn (:lsn (effective-lsn best))
         :in-recovery? (:in-recovery? best)
         :ties (mapv :instance (filter #(tied? best %) (rest ranked)))
         :behind (mapv (fn [reading]
                         {:instance (:instance reading)
                          :bytes (- top (position-of reading))})
                       (rest ranked))
         :out-of-recovery (mapv :instance (filter primary? ranked))
         :unreadable unreadable
         :unpositioned without-position}))))

(defn proposal
  "What the positions suggest, with the cost of acting on it.

   A cluster whose operator reports a healthy primary gets no candidate at all.
   Promoting a standby over a working primary is not a repair, whatever the
   positions say.

   Otherwise :candidate is the instance ranked first in the order
   CloudNativePG itself elects in - the one promotion discards least from, not
   the one that is safe to promote. Standbys are ranked by `effective-lsn`. Promoting it
   discards nothing by construction, so the number worth stating is the other
   direction: :behind says what choosing each alternative instead would throw
   away. :ties names instances sharing the candidate's position, where the
   positions alone cannot choose, and :unpositioned the readable instances
   that reported no position at all."
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
   plainly as what is known: an unread instance, or one that answered without
   a position, may hold the furthest position of all, which would make the
   candidate below the wrong one."
  [{:keys [candidate lsn ties behind unreadable unpositioned reason in-recovery? out-of-recovery]}]
  (if (nil? candidate)
    [(str "no promotion candidate: " reason)]
    (let [ahead (filter #(neg? (:bytes %)) behind)
          writable (set out-of-recovery)
          gaps (concat
                (when (seq unreadable)
                  [(format "%s could not be read, and an unread instance may sit further ahead than %s"
                           (str/join ", " unreadable) candidate)])
                (when (seq unpositioned)
                  [(format (str "%s answered without a received or replayed WAL position, so it "
                                "cannot be ranked and may sit further ahead than %s")
                           (str/join ", " unpositioned) candidate)]))]
      (into
       (cond-> [(format (if (seq ahead) "ranked first: %s at %s" "furthest ahead: %s at %s")
                        candidate lsn)]
         (seq ties)
         (conj (format "tied at the same position: %s - the positions cannot choose between them"
                       (str/join ", " ties)))
         (not in-recovery?)
         (conj (str candidate " does not report itself in recovery, so it may not be a standby - "
                    "check what it is before treating it as one"))
         (< 1 (count out-of-recovery))
         (conj (format (str "%d instances report themselves out of recovery: %s - more than one "
                            "accepts writes, so promoting is not the repair; they rank by pod name, "
                            "as CloudNativePG orders instances out of recovery, and their positions "
                            "do not choose between them")
                       (count out-of-recovery) (str/join ", " out-of-recovery))))
       (concat
        (for [{:keys [instance bytes]} ahead]
          (if (contains? writable instance)
            (format (str "%s sits %s further ahead than %s and is not in recovery either - %s "
                         "ranks first only because its pod name sorts first")
                    instance (describe-bytes (- bytes)) candidate candidate)
            (format "%s sits %s further ahead than %s, which ranks first only because it is not in recovery"
                    instance (describe-bytes (- bytes)) candidate)))
        (for [{:keys [instance bytes]} behind
              :when (pos? bytes)]
          (if (contains? writable instance)
            (format (str "%s also accepts writes and sits %s behind %s - choosing %s as the "
                         "survivor would discard those %s")
                    instance (describe-bytes bytes) candidate instance (describe-bytes bytes))
            (format "promoting %s instead would discard %s that %s already holds"
                    instance (describe-bytes bytes) candidate)))
        gaps
        ["this is a proposal from WAL positions alone - it weighs no application, "
         "no backup and no disk headroom, and promoting is a write that needs your approval"])))))

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

(defn primary-slots
  "The slots held by the instance that generates WAL. Every standby carries a
   copy of the cluster's slots so a promotion does not lose them, and nothing
   ever connects to a copy - so on a standby `active = false` is the resting
   state and says nothing about retention. Only the primary's own slots hold
   back WAL on the volume that fills."
  [slots primary]
  (if primary (filter #(= primary (:instance %)) slots) slots))

(defn slot-lines
  "Comments on the primary's slots. Without a named primary every slot is read,
   since a cluster with no primary is the case where the slots matter most."
  [slots primary]
  (for [slot (primary-slots slots primary)
        :let [state (slot-state slot)]
        :when state]
    (slot-line slot state)))

;; ---------------------------------------------------------------------------
;; readings

(defn instance-reading
  "One instance's position from the answer of its position query, nil when
   the exec did not answer. An exec that fails - the container of a
   crashlooping pod does not exist - leaves the instance unreadable rather
   than silently absent, because an unread instance may be the furthest
   ahead of all."
  [pod answer]
  (if (str/blank? answer)
    {:instance pod :readable? false}
    (let [[recovery receive replay lag] (str/split answer #"\|" -1)]
      {:instance pod
       :readable? true
       :in-recovery? (= "t" recovery)
       :receive-lsn receive
       :replay-lsn replay
       :replay-lag lag})))

(defn slot-reading
  "One instance's replication slots from the answer of its slot query:
   {:instance :readable? true :slots [..]}, or {:instance :readable? false}
   when the exec did not answer (nil). An answered query with no rows is an
   instance without slots; an unanswered one says nothing about its slots, so
   it must not read as none."
  [pod answer]
  (if (nil? answer)
    {:instance pod :readable? false}
    {:instance pod
     :readable? true
     :slots (vec (for [line (str/split-lines (str/trim answer))
                       :when (not (str/blank? line))
                       :let [[slot-name active wal-status safe] (str/split line #"\|" -1)]]
                   {:instance pod :slot-name slot-name :active (= "t" active)
                    :wal-status wal-status :safe-wal-size safe}))}))

(defn unread-slot-lines
  "One warning per instance whose slots could not be read, so a missing slot
   warning is never taken for a slot that is fine."
  [slot-readings]
  (for [{:keys [instance readable?]} slot-readings
        :when (not readable?)]
    (format "could not read the replication slots on %s - a slot there that lost WAL would not be shown"
            instance)))
