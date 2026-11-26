;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.retention
  "Whether a cluster puts a floor under replication slot retention.

   A slot is a standby's promise from the primary: keep every WAL segment past
   my position. Without max_slot_wal_keep_size that promise has no limit, so one
   standby that stops reading retains WAL until the volume is full - at which
   point the instance manager refuses to start postgres and the instance
   restarts for as long as the condition lasts.

   The cap is what bounds it: past that much WAL the slot is invalidated and the
   segments are released. The standby then needs re-cloning, which is the trade -
   a standby to rebuild against a primary that cannot start."
  (:require [clojure.string :as str]
            [gumshoe.cnpg.storage :as storage]))

(def parameter "max_slot_wal_keep_size")

(def ^:private unit-factors
  {"" 1048576.0 ;; postgres reads a bare number for this setting as megabytes
   "B" 1.0 "kB" 1024.0 "MB" 1048576.0 "GB" 1073741824.0 "TB" 1.099511627776E12})

(defn parse-size
  "A postgres size setting as bytes, :unbounded for -1, or nil when it does not
   parse. The units are postgres's own (B, kB, MB, GB, TB), binary and
   case-sensitive as postgres reads them, so `10gb` or a Kubernetes quantity
   like `10Gi` is a value postgres refuses and parses to nil here."
  [value]
  (let [text (str/trim (str value))]
    (cond
      (str/blank? text) nil
      (= "-1" text) :unbounded
      :else (when-let [[_ number unit] (re-matches #"(\d+(?:\.\d+)?)\s*([A-Za-z]*)" text)]
              (when-let [factor (get unit-factors unit)]
                (* (Double/parseDouble number) factor))))))

(defn cap-of
  "The configured cap, read from the cluster's own spec. Absent means the
   postgres default, which is -1."
  [cluster]
  (parse-size (get-in cluster [:spec :postgresql :parameters (keyword parameter)])))

(defn wal-volume
  "The volume pg_wal lives on and therefore the one an abandoned slot fills:
   walStorage when the cluster declares it, the data volume otherwise. As
   {:section :walStorage|:storage :bytes}."
  [cluster]
  (let [section (if (get-in cluster [:spec :walStorage]) :walStorage :storage)]
    {:section section
     :bytes (storage/size-of (get-in cluster [:spec section]))}))

(defn volume-bytes
  [cluster]
  (:bytes (wal-volume cluster)))

(def share-of-volume
  "How much of a volume a cap may promise and still be a floor. A cap the size
   of the volume invalidates no slot before the disk is full, so it protects
   nothing."
  0.5)

(defn describe-bytes
  [bytes]
  (if (>= bytes 1073741824.0)
    (format "%.1fGiB" (/ bytes 1073741824.0))
    (format "%.0fMiB" (/ bytes 1048576.0))))

(defn assess
  "One cluster's retention floor as :bounded, :unset, :unbounded, :oversized or
   :unknown. :unknown is a value neither this nor postgres's own parser reads,
   reported rather than assumed safe."
  [cluster]
  (let [raw (get-in cluster [:spec :postgresql :parameters (keyword parameter)])
        cap (cap-of cluster)
        {volume :bytes section :section} (wal-volume cluster)]
    (cond
      (nil? raw) {:state :unset}
      (= :unbounded cap) {:state :unbounded :raw (str raw)}
      (nil? cap) {:state :unknown :raw (str raw)}
      (and volume (> cap (* share-of-volume volume)))
      {:state :oversized :raw (str raw) :cap cap :volume volume :section section}
      :else {:state :bounded :raw (str raw) :cap cap :volume volume :section section})))

(defn exposed?
  [assessment]
  (contains? #{:unset :unbounded :oversized :unknown} (:state assessment)))
