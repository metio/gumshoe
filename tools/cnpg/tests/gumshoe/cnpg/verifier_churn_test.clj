;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.verifier-churn-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.cnpg.verifier-churn :as churn]))

(deftest parse-snapshot-reads-role-and-digest-test
  (is (= {"app" "aaa" "keycloak" "bbb"}
         (churn/parse-snapshot "app|aaa\nkeycloak|bbb"))))

(deftest parse-snapshot-drops-blank-and-malformed-lines-test
  (is (= {"app" "aaa"}
         (churn/parse-snapshot "app|aaa\n\nNOTICE: something\n|\n"))))

(deftest parse-snapshot-of-nothing-is-empty-test
  (is (= {} (churn/parse-snapshot nil)))
  (is (= {} (churn/parse-snapshot ""))))

(deftest a-moved-verifier-is-reported-by-role-test
  (let [changes (churn/compare-snapshots {"app" "aaa" "keycloak" "bbb"}
                                         {"app" "aaa" "keycloak" "ccc"})]
    (is (= ["keycloak"] (:moved changes)))
    (is (= 2 (:total changes)))
    (is (empty? (:appeared changes)))
    (is (empty? (:vanished changes)))))

(deftest roles-only-in-one-reading-are-not-moved-test
  (let [changes (churn/compare-snapshots {"app" "aaa" "gone" "bbb"}
                                         {"app" "aaa" "new" "ccc"})]
    (is (empty? (:moved changes)))
    (is (= ["new"] (:appeared changes)))
    (is (= ["gone"] (:vanished changes)))))

(deftest one-moved-verifier-is-enough-to-report-churn-test
  (let [result (churn/verdict (churn/compare-snapshots {"a" "1" "b" "2" "c" "3"}
                                                       {"a" "1" "b" "2" "c" "9"}))]
    (is (= :churning (:verdict result)))
    (is (= 1 (:moved result)))
    (is (= "1 of 3 verifiers re-derived" (:summary result)))))

(deftest an-unchanged-cluster-is-stable-test
  (let [result (churn/verdict (churn/compare-snapshots {"a" "1" "b" "2"} {"a" "1" "b" "2"}))]
    (is (= :stable (:verdict result)))
    (is (= "all 2 verifiers unchanged" (:summary result)))
    (is (nil? (:note result)))))

(deftest a-stable-verdict-notes-roles-that-came-and-went-test
  (let [result (churn/verdict (churn/compare-snapshots {"a" "1" "gone" "2"}
                                                       {"a" "1" "new" "3"}))]
    (is (= :stable (:verdict result)))
    (is (re-find #"1 role\(s\) appeared and 1 vanished" (:note result)))))

(deftest reading-no-roles-is-not-a-clean-result-test
  (let [result (churn/verdict (churn/compare-snapshots {} {}))]
    (is (= :no-roles (:verdict result)))
    (is (re-find #"says nothing about churn" (first (churn/guidance result))))))

(deftest churn-guidance-names-the-pool-mode-asymmetry-test
  (let [lines (churn/guidance {:verdict :churning :moved 7})]
    (is (some #(re-find #"transaction-mode" %) lines))
    (is (some #(re-find #"session-mode" %) lines))
    (is (some #(re-find #"every reconcile" %) lines))))

(deftest stable-guidance-keeps-the-window-caveat-test
  (is (re-find #"slower trigger than the window"
               (first (churn/guidance {:verdict :stable})))))

(deftest the-default-window-outlasts-a-five-minute-reconcile-test
  (is (> churn/default-window-seconds 300)))

(deftest a-single-role-cluster-reads-in-the-singular-test
  (is (= "1 of 1 verifier re-derived"
         (:summary (churn/verdict (churn/compare-snapshots {"a" "1"} {"a" "2"}))))))
