;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.abort-exit-contract-test
  "Every abort exits 1, whether or not anything changed, and the skill and
   the README tell an agent so: an agent reading that only an abort after a
   change exits 1 would take a declined confirmation for success."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.runbook :as runbook]))

(def ^:private abort-reasons
  "The reasons interact and flow record with outcome/aborted!; each of those
   paths returns false, so the run's outcome is :failed."
  ["declined at the confirmation"
   "the precondition no longer held after the confirmation"
   "this book does not support a dry run"])

(deftest every-abort-exits-1-whether-or-not-anything-changed
  (doseq [reason abort-reasons]
    (testing reason
      (let [unchanged (runbook/verdict {:announce? true} {} :failed {:applied 0 :aborted reason} false)
            changed (runbook/verdict {:announce? true} {} :failed {:applied 2 :aborted reason} false)]
        (is (= 1 (:exit unchanged)))
        (is (= :neutral (-> unchanged :banner :tone)))
        (is (str/starts-with? (-> unchanged :banner :text) "ABORTED"))
        (is (= 1 (:exit changed)))
        (is (= :error (-> changed :banner :tone)))))))

(defn- exit-code-bullet
  [text]
  (some #(when (str/starts-with? % "- **Read the exit code.**") %)
        (str/split text #"\n(?=- )")))

(deftest the-skill-says-every-abort-exits-1
  (let [bullet (some-> (exit-code-bullet (slurp "skills/gumshoe/SKILL.md")) (str/replace #"\s+" " "))]
    (is (some? bullet) "the skill has its exit-code bullet")
    (is (str/includes? bullet "Every abort exits 1 whether or not anything changed"))
    (is (str/includes? bullet "`ABORTED`"))
    (is (not (str/includes? bullet "an abort after something changed")))))

(deftest the-readme-says-every-abort-exits-1
  (is (str/includes? (str/replace (slurp "README.md") #"\s+" " ")
                     "`1` on failure, any abort (whether or not anything changed)")))
