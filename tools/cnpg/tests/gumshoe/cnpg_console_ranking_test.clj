;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg-console-ranking-test
  "Promotion ranking in the order CloudNativePG elects in: standbys by the
   received LSN, replaced by the replayed one when that is further or empty."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [gumshoe.cnpg-console :as console]))

;; pg-2 streamed to 0/5000000; pg-3 never streamed and restored to 0/9000000
;; from the archive; pg-4 received 0/4000000 and replayed to 0/8000000.
(def ^:private archive-readings
  [(console/instance-reading "pg-2" "t|0/5000000|0/5000000|00:00:01")
   (console/instance-reading "pg-3" "t||0/9000000|00:00:01")
   (console/instance-reading "pg-4" "t|0/4000000|0/8000000|00:00:01")])

(def ^:private unhealthy-primary {:name "pg-1" :healthy? false})

(deftest effective-lsn-follows-the-replayed-position-when-it-is-further-test
  (is (= {:bytes 0x5000000 :lsn "0/5000000"}
         (console/effective-lsn {:receive-lsn "0/5000000" :replay-lsn "0/5000000"})))
  (is (= {:bytes 0x9000000 :lsn "0/9000000"}
         (console/effective-lsn {:receive-lsn "" :replay-lsn "0/9000000"})))
  (is (= {:bytes 0x8000000 :lsn "0/8000000"}
         (console/effective-lsn {:receive-lsn "0/4000000" :replay-lsn "0/8000000"})))
  (is (= {:bytes 0x6000000 :lsn "0/6000000"}
         (console/effective-lsn {:receive-lsn "0/6000000" :replay-lsn "0/5000000"})))
  (is (nil? (console/effective-lsn {:receive-lsn "" :replay-lsn ""}))))

(deftest a-standby-restored-from-the-archive-is-the-candidate-test
  (let [{:keys [candidate lsn behind unreadable unpositioned]}
        (console/proposal archive-readings unhealthy-primary)]
    (is (= "pg-3" candidate))
    (is (= "0/9000000" lsn))
    (is (= [{:instance "pg-4" :bytes 0x1000000} {:instance "pg-2" :bytes 0x4000000}] behind))
    (is (= [] unreadable))
    (is (= [] unpositioned))))

(deftest every-readable-standby-appears-in-the-lines-test
  (let [lines (console/proposal-lines (console/proposal archive-readings unhealthy-primary))]
    (is (= "furthest ahead: pg-3 at 0/9000000" (first lines)))
    (is (some #{"promoting pg-4 instead would discard 16.0MiB that pg-3 already holds"} lines))
    (is (some #{"promoting pg-2 instead would discard 64.0MiB that pg-3 already holds"} lines))
    (doseq [instance ["pg-2" "pg-3" "pg-4"]]
      (is (some #(str/includes? % instance) lines) instance))))

(deftest a-readable-instance-without-a-position-is-named-test
  (let [readings (conj archive-readings
                       (console/instance-reading "pg-5" "t|||")
                       (console/instance-reading "pg-6" nil))
        proposal (console/proposal readings unhealthy-primary)
        lines (console/proposal-lines proposal)]
    (is (= ["pg-5"] (:unpositioned proposal)))
    (is (= ["pg-6"] (:unreadable proposal)))
    (is (some #(and (str/starts-with? % "pg-5 answered without a received or replayed WAL position")
                    (str/includes? % "may sit further ahead than pg-3"))
              lines))
    (is (some #(str/starts-with? % "pg-6 could not be read") lines))))

(deftest equal-positions-are-ordered-by-replay-then-name-test
  (testing "the same effective position, the further replay first"
    (let [readings [{:instance "a" :readable? true :in-recovery? true
                     :receive-lsn "0/9000000" :replay-lsn "0/1000000"}
                    {:instance "b" :readable? true :in-recovery? true
                     :receive-lsn "0/9000000" :replay-lsn "0/2000000"}]
          proposal (console/proposal readings)]
      (is (= "b" (:candidate proposal)))
      (is (= ["a"] (:ties proposal)))))
  (testing "the same positions throughout, by pod name"
    (let [readings [{:instance "z" :readable? true :in-recovery? true
                     :receive-lsn "0/9000000" :replay-lsn "0/9000000"}
                    {:instance "y" :readable? true :in-recovery? true
                     :receive-lsn "0/9000000" :replay-lsn "0/9000000"}]]
      (is (= "y" (:candidate (console/proposal readings)))))))

(deftest an-instance-not-in-recovery-ranks-first-and-a-standby-ahead-of-it-is-named-test
  (let [readings [{:instance "pg-1" :readable? true :in-recovery? false
                   :receive-lsn "0/3000000" :replay-lsn ""}
                  {:instance "pg-2" :readable? true :in-recovery? true
                   :receive-lsn "0/5000000" :replay-lsn "0/5000000"}]
        proposal (console/proposal readings unhealthy-primary)
        lines (console/proposal-lines proposal)]
    (is (= "pg-1" (:candidate proposal)))
    (is (= "ranked first: pg-1 at 0/3000000" (first lines)))
    (is (some #{"pg-2 sits 32.0MiB further ahead than pg-1, which ranks first only because it is not in recovery"}
              lines))
    (is (not-any? #(str/includes? % "would discard") lines))))

(def ^:private split-brain
  [{:instance "p-1" :readable? true :in-recovery? false :receive-lsn "0/3000000" :replay-lsn ""}
   {:instance "p-2" :readable? true :in-recovery? false :receive-lsn "0/5000000" :replay-lsn ""}
   {:instance "s-3" :readable? true :in-recovery? true :receive-lsn "0/4000000" :replay-lsn "0/4000000"}])

(deftest two-instances-out-of-recovery-rank-by-pod-name-and-are-named-as-such-test
  (let [proposal (console/proposal split-brain unhealthy-primary)
        lines (console/proposal-lines proposal)]
    (is (= "p-1" (:candidate proposal)))
    (is (= ["p-1" "p-2"] (:out-of-recovery proposal)))
    (testing "the primary ahead is named a primary, and the reason is the pod name"
      (is (some #{(str "p-2 sits 32.0MiB further ahead than p-1 and is not in recovery either - "
                       "p-1 ranks first only because its pod name sorts first")}
                lines))
      (is (not-any? #(and (str/starts-with? % "p-2 ") (str/includes? % "only because it is not in recovery"))
                    lines)))
    (testing "the standby ahead keeps the recovery reason, which holds for it"
      (is (some #{"s-3 sits 16.0MiB further ahead than p-1, which ranks first only because it is not in recovery"}
                lines)))
    (testing "the split brain is stated"
      (is (some #(str/starts-with? % "2 instances report themselves out of recovery: p-1, p-2 - more than one accepts writes")
                lines)))))

(def ^:private split-brain-candidate-ahead
  [{:instance "p-1" :readable? true :in-recovery? false :receive-lsn "0/5000000" :replay-lsn ""}
   {:instance "p-2" :readable? true :in-recovery? false :receive-lsn "0/3000000" :replay-lsn ""}])

(deftest a-primary-behind-the-candidate-is-not-offered-for-promotion-test
  (let [proposal (console/proposal split-brain-candidate-ahead unhealthy-primary)
        lines (console/proposal-lines proposal)]
    (is (= "p-1" (:candidate proposal)))
    (is (= [{:instance "p-2" :bytes 0x2000000}] (:behind proposal)))
    (is (some #{(str "p-2 also accepts writes and sits 32.0MiB behind p-1 - choosing p-2 as the "
                     "survivor would discard those 32.0MiB")}
              lines))
    (is (not-any? #(str/includes? % "promoting p-2") lines))
    (testing "a standby behind the same candidate keeps the promotion wording"
      (let [readings (conj split-brain-candidate-ahead
                           {:instance "s-3" :readable? true :in-recovery? true
                            :receive-lsn "0/4000000" :replay-lsn "0/4000000"})
            lines (console/proposal-lines (console/proposal readings unhealthy-primary))]
        (is (some #{"promoting s-3 instead would discard 16.0MiB that p-1 already holds"} lines))
        (is (not-any? #(str/includes? % "promoting p-2") lines))))))

;; --- no line offers to promote an instance already out of recovery ---------------

(def ^:private seed 20261126)

(def ^:private gen-lsn
  (gen/one-of [(gen/return "")
               (gen/fmap #(format "0/%X" %) (gen/choose 0 0x10000000))]))

(def ^:private gen-named-readings
  (gen/let [readings (gen/vector (gen/let [readable? gen/boolean
                                           in-recovery? gen/boolean
                                           receive gen-lsn
                                           replay gen-lsn]
                                   {:readable? readable? :in-recovery? in-recovery?
                                    :receive-lsn receive :replay-lsn replay})
                                 1 5)
            names (gen/shuffle ["pg-a" "pg-b" "pg-c" "pg-d" "pg-e"])]
    (mapv #(assoc %1 :instance %2) readings names)))

(def ^:private gen-primary
  (gen/one-of [(gen/return nil)
               (gen/let [name (gen/elements ["pg-a" "pg-b" "pg-z"]) healthy? gen/boolean]
                 {:name name :healthy? healthy?})]))

(deftest no-line-offers-to-promote-an-instance-out-of-recovery-test
  (let [result (tc/quick-check
                500
                (prop/for-all [readings gen-named-readings
                               primary gen-primary]
                  (let [writable (->> readings
                                      (filter #(false? (:in-recovery? %)))
                                      (map :instance))
                        lines (console/proposal-lines (console/proposal readings primary))]
                    (every? (fn [instance]
                              (not-any? #(str/includes? % (str "promoting " instance " ")) lines))
                            writable)))
                :seed seed)]
    (is (:pass? result) (pr-str (:shrunk result)))))

(deftest a-single-instance-out-of-recovery-states-no-split-brain-test
  (let [lines (console/proposal-lines
               (console/proposal [(first split-brain) (nth split-brain 2)] unhealthy-primary))]
    (is (not-any? #(str/includes? % "out of recovery:") lines))
    (is (not-any? #(str/includes? % "pod name sorts first") lines))))

(deftest standbys-alone-report-no-instance-out-of-recovery-test
  (is (= [] (:out-of-recovery (console/proposal archive-readings unhealthy-primary)))))
