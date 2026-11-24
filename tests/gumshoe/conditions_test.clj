;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.conditions-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.conditions :as conditions]
            [gumshoe.onset :as onset]))

(def ^:private now (java.time.Instant/parse "2026-11-18T12:00:00Z"))

(defn- object
  [& conditions]
  {:metadata {:name "o"} :status {:conditions (vec conditions)}})

(def ^:private ready-true {:type "Ready" :status "True"})
(def ^:private ready-false {:type "Ready" :status "False" :reason "Broken"})
(def ^:private ready-unknown {:type "Ready" :status "Unknown"})
(def ^:private ready-blank {:type "Ready"})

(deftest condition-finds-the-first-of-a-type
  (is (= ready-false (conditions/condition (object {:type "Synced" :status "True"} ready-false) "Ready")))
  (is (nil? (conditions/condition (object ready-true) "Synced")))
  (is (nil? (conditions/condition {} "Ready")))
  (is (nil? (conditions/condition nil "Ready")))
  (testing "a repeated type: the first counts"
    (is (= ready-true (conditions/condition (object ready-true ready-false) "Ready")))))

(deftest by-type-agrees-with-condition
  (is (= {} (conditions/by-type {})))
  (is (= {"Ready" ready-true "Synced" {:type "Synced" :status "False"}}
         (conditions/by-type (object ready-true {:type "Synced" :status "False"}))))
  (testing "a repeated type keeps the first, as condition does"
    (let [o (object ready-false ready-true)]
      (is (= ready-false (get (conditions/by-type o) "Ready") (conditions/condition o "Ready"))))))

(deftest status-predicates-over-every-state
  ;; [object  holds?  status? False  status? Unknown]
  (doseq [[label o holds false? unknown?]
          [["True" (object ready-true) true false false]
           ["False" (object ready-false) false true false]
           ["Unknown" (object ready-unknown) false false true]
           ["no status" (object ready-blank) false false false]
           ["missing condition" (object {:type "Synced" :status "True"}) false false false]
           ["no conditions" (object) false false false]
           ["no status at all" {:metadata {:name "o"}} false false false]
           ["nil object" nil false false false]
           ["first of a repeated type" (object ready-false ready-true) false true false]]]
    (testing label
      (is (= holds (conditions/holds? o "Ready")))
      (is (= holds (conditions/ready? o)))
      (is (= false? (conditions/status? o "Ready" "False")))
      (is (= unknown? (conditions/status? o "Ready" "Unknown"))))))

(deftest first-false-skips-missing-and-unknown
  (let [types ["Ready" "Available" "Synced"]]
    (is (nil? (conditions/first-false (object) types)))
    (is (nil? (conditions/first-false (object ready-true ready-unknown) types)))
    (is (= {:type "Synced" :status "False"}
           (conditions/first-false (object ready-unknown {:type "Synced" :status "False"}) types)))
    (testing "the order of types decides, not the order of the list"
      (is (= ready-false
             (conditions/first-false (object {:type "Synced" :status "False"} ready-false) types))))))

(deftest stale-compares-the-observed-generation
  (let [at (fn [generation status-observed]
             (cond-> {:metadata (cond-> {} generation (assoc :generation generation))
                      :status {}}
               status-observed (assoc-in [:status :observedGeneration] status-observed)))]
    ;; [label  object  condition  stale?]
    (doseq [[label o c expected]
            [["status behind" (at 3 2) nil true]
             ["status current" (at 3 3) nil false]
             ["status ahead" (at 3 4) nil false]
             ["no status generation" (at 3 nil) nil false]
             ["no metadata generation" (at nil 2) nil false]
             ["condition behind" (at 3 3) {:type "Ready" :observedGeneration 2} true]
             ["condition current, status behind" (at 3 2) {:type "Ready" :observedGeneration 3} false]
             ["condition without one falls back to status" (at 3 2) {:type "Ready"} true]
             ["non-numeric generation" (at "3" 2) nil false]]]
      (testing label
        (is (= expected (if c (conditions/stale? o c) (conditions/stale? o))))))
    (testing "the one-argument form ignores the condition's own generation"
      (let [o (assoc-in (at 3 3) [:status :conditions] [{:type "Ready" :observedGeneration 1}])]
        (is (false? (conditions/stale? o)))
        (is (true? (conditions/stale? o (conditions/condition o "Ready"))))))))

(deftest minutes-since-is-tolerant
  (doseq [[value expected]
          [["2026-11-18T11:30:00Z" 30]
           [(java.time.Instant/parse "2026-11-18T10:00:00Z") 120]
           [(java.util.Date/from (java.time.Instant/parse "2026-11-18T11:59:00Z")) 1]
           ["2026-11-18T12:30:00Z" -30]
           [nil nil]
           ["" nil]
           ["garbage" nil]
           [42 nil]]]
    (is (= expected (conditions/minutes-since value now)) (pr-str value)))
  (is (nil? (conditions/minutes-since "2026-11-18T11:30:00Z" nil))))

(deftest first-line-of-a-message
  (doseq [[text expected]
          [["boom" "boom"]
           ["  boom  \nsecond" "boom"]
           ["\nsecond" nil]
           ["   " nil]
           ["" nil]
           [nil nil]
           [42 "42"]]]
    (is (= expected (conditions/first-line text)) (pr-str text))))

(deftest ready-check-reads-afresh-on-every-attempt
  (let [reads (atom [(object ready-false) (object ready-unknown) (object ready-true)])
        fetch (fn [] (let [o (first @reads)] (swap! reads rest) o))
        check (conditions/ready-check {:description "x is Ready" :fetch fetch :timeout 120 :interval 10})]
    (is (= {:description "x is Ready" :timeout 120 :interval 10} (dissoc check :check)))
    (is (= [false false true] (repeatedly 3 (:check check))))))

(deftest ready-check-on-another-type-and-without-an-object
  (let [check (conditions/ready-check {:description "d" :type "Synced" :fetch (constantly (object {:type "Synced" :status "True"}))})]
    (is (= {:description "d"} (dissoc check :check)))
    (is (true? ((:check check)))))
  (is (false? ((:check (conditions/ready-check {:description "d" :fetch (constantly nil)}))))))

(deftest onset-reads-a-date
  (is (= (java.time.Instant/parse "2026-11-18T10:00:00Z")
         (onset/instant (java.util.Date/from (java.time.Instant/parse "2026-11-18T10:00:00Z")))))
  (is (nil? (onset/instant "garbage")))
  (is (nil? (onset/instant 42))))
