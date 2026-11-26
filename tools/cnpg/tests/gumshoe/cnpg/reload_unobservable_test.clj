;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.reload-unobservable-test
  "cnpg reload verifies what it can see - its request landed on a healthy
   cluster - and says it cannot confirm that the operator acted, because
   CloudNativePG records nothing for a reload; it never claims that the
   cluster took the reload on the strength of its own write."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gumshoe.cnpg.action-books :as books]
            [gumshoe.cnpg.actions :as actions]
            [gumshoe.mutation :as mutation]
            [gumshoe.outcome :as outcome]
            [gumshoe.verify :as verify]))

(def ^:private healthy
  {:metadata {:namespace "db" :name "pg" :annotations {:cnpg.io/reloadedAt "s1"}}
   :spec {:instances 2}
   :status {:phase actions/healthy-phase :readyInstances 2}})

(deftest the-reload-checks-claim-only-what-they-observe
  (let [checks ((:verify books/reload) {:context "ctx" :ref {:namespace "db" :name "pg"} :target "db/pg" :stamp "s1"})]
    (is (= 2 (count checks)))
    (is (not-any? #(str/includes? (:description %) "took the reload") checks))
    (is (str/includes? (:description (first checks)) "carries this run's stamp"))
    (is (true? (:soft? (second checks))))
    (is (str/includes? (:description (second checks)) "the operator reconciled db/pg"))))

(deftest a-reload-ends-done-but-not-confirmed
  (let [checks ((:verify books/reload) {:context "ctx" :ref {:namespace "db" :name "pg"} :target "db/pg" :stamp "s1"})
        record (outcome/fresh)
        err (java.io.StringWriter.)]
    (with-redefs [mutation/read-subject (constantly healthy)]
      (binding [outcome/*record* record *err* err]
        (is (true? (verify/all checks)))))
    (is (= ["the operator reconciled db/pg after the reload request"] (:unconfirmed @record)))
    (is (str/includes? (str err) "CloudNativePG records nothing"))
    (is (str/includes? (:text (outcome/banner {:changes? true :outcome :ok} (assoc @record :applied 1)))
                       "NOT confirmed"))))
