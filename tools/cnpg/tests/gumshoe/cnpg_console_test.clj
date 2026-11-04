;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-console-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gumshoe.cnpg-console :as console]))

;; The live readings from cloudnative-pg-cluster-general on
;; kube.bastelgenosse.de: -3 ahead of -1 by a wide margin, and -2 - the
;; designated primary - unreadable, because its postgres container does not
;; exist to exec into.
(def ^:private readings
  [{:instance "cloudnative-pg-cluster-general-1" :readable? true :in-recovery? true
    :receive-lsn "120/5A000000" :replay-lsn "120/5A0000A0" :replay-lag ""}
   {:instance "cloudnative-pg-cluster-general-2" :readable? false}
   {:instance "cloudnative-pg-cluster-general-3" :readable? true :in-recovery? true
    :receive-lsn "125/27000000" :replay-lsn "125/27000000"
    :replay-lag "5 days 00:00:43.169615"}])

(deftest an-lsn-parses-to-a-byte-offset-test
  (is (= 0 (console/parse-lsn "0/0")))
  (is (= 4294967296 (console/parse-lsn "1/0")))
  (is (= (+ (* 0x125 4294967296) 0x27000000) (console/parse-lsn "125/27000000"))))

(deftest a-non-lsn-has-no-position-test
  (is (nil? (console/parse-lsn "")))
  (is (nil? (console/parse-lsn nil)))
  (is (nil? (console/parse-lsn "ERROR:  recovery is in progress"))))

(deftest the-furthest-ahead-instance-is-the-candidate-test
  (let [{:keys [candidate lsn]} (console/proposal readings)]
    (is (= "cloudnative-pg-cluster-general-3" candidate))
    (is (= "125/27000000" lsn))))

(deftest the-cost-of-the-alternative-is-stated-test
  (let [{:keys [behind]} (console/proposal readings)
        one (first (filter #(str/ends-with? (:instance %) "-1") behind))]
    (is (pos? (:bytes one)))
    (is (= (- (console/parse-lsn "125/27000000") (console/parse-lsn "120/5A000000"))
           (:bytes one)))
    (is (some #(str/includes? % "promoting cloudnative-pg-cluster-general-1 instead would discard")
              (console/proposal-lines (console/proposal readings))))))

(deftest an-unreadable-instance-is-named-as-a-gap-in-the-proposal-test
  (let [lines (console/proposal-lines (console/proposal readings))]
    (is (= ["cloudnative-pg-cluster-general-2"] (:unreadable (console/proposal readings))))
    (is (some #(str/includes? % "may sit further ahead") lines))))

(deftest the-proposal-never-presents-itself-as-a-decision-test
  (is (some #(str/includes? % "needs your approval")
            (console/proposal-lines (console/proposal readings)))))

(deftest instances-at-the-same-position-are-reported-as-a-tie-test
  (let [tied (assoc-in readings [0 :receive-lsn] "125/27000000")
        {:keys [ties]} (console/proposal tied)]
    (is (= 1 (count ties)))
    (is (some #(str/includes? % "cannot choose between them")
              (console/proposal-lines (console/proposal tied))))))

(deftest a-candidate-not-in-recovery-is-flagged-test
  (let [odd (assoc-in readings [2 :in-recovery?] false)]
    (is (some #(str/includes? % "may not be a standby")
              (console/proposal-lines (console/proposal odd))))))

(deftest no-readable-instance-yields-no-candidate-test
  (let [none (console/proposal [{:instance "a" :readable? false}
                                {:instance "b" :readable? false}])]
    (is (nil? (:candidate none)))
    (is (= ["a" "b"] (:unreadable none)))
    (is (= ["no promotion candidate: no instance reported a WAL position, so nothing here can rank them"]
           (console/proposal-lines none)))))

(deftest an-inactive-slot-still-reserving-wal-is-reported-test
  (let [slots [{:slot-name "_cnpg_cloudnative_pg_cluster_general_1" :active false
                :wal-status "reserved" :safe-wal-size ""}
               {:slot-name "_cnpg_cloudnative_pg_cluster_general_3" :active true
                :wal-status "reserved" :safe-wal-size "2 GB"}]]
    (is (= 1 (count (console/retaining-slots slots))))
    (is (some #(str/includes? % "_cnpg_cloudnative_pg_cluster_general_1")
              (console/slot-lines slots nil)))
    (is (not-any? #(str/includes? % "_general_3") (console/slot-lines slots nil)))))

(deftest a-lost-slot-is-not-reported-as-retaining-test
  (is (empty? (console/retaining-slots [{:slot-name "s" :active false :wal-status "lost"}]))))

(deftest sizes-read-in-the-units-an-operator-uses-test
  (is (= "nothing" (console/describe-bytes 0)))
  (is (= "unknown" (console/describe-bytes nil)))
  (is (= "512 bytes" (console/describe-bytes 512)))
  (is (= "2.00GiB" (console/describe-bytes 2147483648))))

(deftest the-promotion-post-check-finds-the-cluster-it-is-given-test
  (let [clusters [{:metadata {:namespace "other" :name "db"}
                   :status {:currentPrimary "db-1"}}
                  {:metadata {:namespace "cloudnative-pg-cluster-general"
                              :name "cloudnative-pg-cluster-general"}
                   :status {:currentPrimary "cloudnative-pg-cluster-general-2"}}]]
    (is (= "cloudnative-pg-cluster-general-2"
           (console/current-primary clusters "cloudnative-pg-cluster-general/cloudnative-pg-cluster-general")))
    (is (nil? (console/current-primary clusters "nope/nope"))
        "a lookup that finds nothing is nil, not a crash")))

(deftest a-healthy-primary-is-not-displaced-by-a-standby-test
  (let [{:keys [candidate primary reason]}
        (console/proposal readings {:name "cloudnative-pg-cluster-general-2" :healthy? true})]
    (is (nil? candidate))
    (is (= "cloudnative-pg-cluster-general-2" primary))
    (is (re-find #"healthy primary" reason))
    (is (= ["no promotion candidate: the cluster reports cloudnative-pg-cluster-general-2 as a healthy primary, so no promotion is called for - promoting a standby over a working primary discards everything the primary holds beyond it"]
           (console/proposal-lines
            (console/proposal readings {:name "cloudnative-pg-cluster-general-2" :healthy? true}))))))

(deftest an-unhealthy-primary-leaves-the-ranking-alone-test
  (is (= "cloudnative-pg-cluster-general-3"
         (:candidate (console/proposal readings {:name "cloudnative-pg-cluster-general-2"
                                                 :healthy? false})))))

(deftest a-cluster-with-no-named-primary-leaves-the-ranking-alone-test
  (is (= "cloudnative-pg-cluster-general-3"
         (:candidate (console/proposal readings {:name nil :healthy? true})))))

;; The slot table on cloudnative-pg-cluster-general once the 10GB cap was
;; applied: -1's slot invalidated and -3's streaming with headroom.
(def ^:private live-slots
  [{:instance "cloudnative-pg-cluster-general-2"
    :slot-name "_cnpg_cloudnative_pg_cluster_general_1"
    :active false :wal-status "lost" :safe-wal-size ""}
   {:instance "cloudnative-pg-cluster-general-2"
    :slot-name "_cnpg_cloudnative_pg_cluster_general_3"
    :active true :wal-status "reserved" :safe-wal-size "10 GB"}])

(deftest a-lost-slot-names-the-rebuild-it-forces-test
  (let [lines (console/slot-lines live-slots nil)]
    (is (= 1 (count lines)))
    (is (re-find #"_cnpg_cloudnative_pg_cluster_general_1 on cloudnative-pg-cluster-general-2 is lost"
                 (first lines)))
    (is (re-find #"needs re-cloning from the primary" (first lines)))
    (is (= 1 (count (console/lost-slots live-slots))))))

(deftest a-streaming-slot-with-headroom-needs-no-comment-test
  (is (nil? (console/slot-state (second live-slots))))
  (is (not-any? #(str/includes? % "_general_3") (console/slot-lines live-slots nil))))

(deftest an-extended-slot-is-retaining-not-safe-test
  (is (= :retaining (console/slot-state {:active false :wal-status "extended"})))
  (is (= 1 (count (console/retaining-slots
                   [{:slot-name "s" :active false :wal-status "extended"}]))))
  (is (re-find #"grows the volume"
               (first (console/slot-lines [{:slot-name "s" :instance "i"
                                            :active false :wal-status "extended"}] nil)))))

(deftest an-unreserved-slot-is-about-to-be-invalidated-test
  (is (= :expiring (console/slot-state {:active false :wal-status "unreserved"})))
  (is (re-find #"next to be released"
               (first (console/slot-lines [{:slot-name "s" :instance "i"
                                            :active false :wal-status "unreserved"}] nil)))))

(deftest a-lost-slot-is-reported-even-with-a-consumer-attached-test
  (is (= :lost (console/slot-state {:active true :wal-status "lost"})))
  (is (seq (console/slot-lines [{:slot-name "s" :instance "i"
                                 :active true :wal-status "lost"}] nil))))

(deftest a-reading-consumer-past-max-wal-size-is-behind-not-gone-test
  (let [line (first (console/slot-lines [{:slot-name "s" :instance "i"
                                          :active true :wal-status "extended"}] nil))]
    (is (= :behind (console/slot-state {:active true :wal-status "extended"})))
    (is (re-find #"behind, not gone" line))))

(deftest a-lost-slots-headroom-is-not-reported-as-unbounded-test
  (is (= "n/a" (console/describe-safe-wal-size (first live-slots))))
  (is (= "10 GB" (console/describe-safe-wal-size (second live-slots))))
  (is (= "unbounded" (console/describe-safe-wal-size {:wal-status "reserved" :safe-wal-size ""}))
      "no cap configured is unbounded; a lost slot's absent cap is not"))

;; Every instance of a healthy cloudnative-pg-cluster-general carries the
;; cluster's slots: the primary -2 holds both, streaming, and each standby holds
;; a copy of the other standby's, which nothing connects to.
(def ^:private mirrored-slots
  [{:instance "cloudnative-pg-cluster-general-1"
    :slot-name "_cnpg_cloudnative_pg_cluster_general_3"
    :active false :wal-status "reserved" :safe-wal-size "1183 GB"}
   {:instance "cloudnative-pg-cluster-general-2"
    :slot-name "_cnpg_cloudnative_pg_cluster_general_1"
    :active true :wal-status "reserved" :safe-wal-size "10 GB"}
   {:instance "cloudnative-pg-cluster-general-2"
    :slot-name "_cnpg_cloudnative_pg_cluster_general_3"
    :active true :wal-status "reserved" :safe-wal-size "10 GB"}
   {:instance "cloudnative-pg-cluster-general-3"
    :slot-name "_cnpg_cloudnative_pg_cluster_general_1"
    :active false :wal-status "reserved" :safe-wal-size "1183 GB"}])

(deftest a-standbys-copy-of-a-slot-is-not-retention-test
  (is (empty? (console/slot-lines mirrored-slots "cloudnative-pg-cluster-general-2"))
      "an inactive copy on a standby is its resting state, not WAL growing a volume"))

(deftest only-the-primarys-slots-are-read-test
  (is (= 2 (count (console/primary-slots mirrored-slots "cloudnative-pg-cluster-general-2"))))
  (is (every? #(= "cloudnative-pg-cluster-general-2" (:instance %))
              (console/primary-slots mirrored-slots "cloudnative-pg-cluster-general-2"))))

(deftest an-inactive-slot-on-the-primary-is-still-reported-test
  (let [stalled (assoc-in mirrored-slots [1 :active] false)]
    (is (= 1 (count (console/slot-lines stalled "cloudnative-pg-cluster-general-2"))))
    (is (re-find #"_cnpg_cloudnative_pg_cluster_general_1 on cloudnative-pg-cluster-general-2 is inactive"
                 (first (console/slot-lines stalled "cloudnative-pg-cluster-general-2"))))))

(deftest with-no-primary-every-slot-is-read-test
  (is (= 2 (count (console/slot-lines mirrored-slots nil)))
      "a cluster with no primary is where the slots matter most, so none are filtered"))
