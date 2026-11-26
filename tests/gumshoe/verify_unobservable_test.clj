;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.verify-unobservable-test
  "verify/unobservable declares an outcome nothing records: it never holds,
   never fails the run, and keeps the banner from claiming verification."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gumshoe.outcome :as outcome]
            [gumshoe.verify :as verify]))

(deftest an-unobservable-outcome-is-reported-as-not-confirmed
  (let [record (outcome/fresh)
        err (java.io.StringWriter.)
        start (System/currentTimeMillis)]
    (binding [outcome/*record* record *err* err]
      (is (true? (verify/eventually (verify/unobservable "the thing happened" "nothing records it")))))
    (is (< (- (System/currentTimeMillis) start) 5000) "it does not wait")
    (is (= ["the thing happened"] (:unconfirmed @record)))
    (is (str/includes? (str err) "nothing records it"))
    (is (not (str/includes? (str err) "verified: the thing happened")))))
