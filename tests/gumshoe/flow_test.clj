;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.flow-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]))

(def ^:private valid
  {:confirmation {:action "delete a thing" :target "cluster" :items ["thing"]}
   :execute! (constantly true)})

(deftest valid-config-test
  (testing "a complete config is valid"
    (is (flow/valid-config? valid)))
  (testing "a blank action can not be confirmed meaningfully"
    (is (not (flow/valid-config? (assoc-in valid [:confirmation :action] ""))))
    (is (not (flow/valid-config? (assoc-in valid [:confirmation :action] nil)))))
  (testing "the user must see what is affected"
    (is (not (flow/valid-config? (assoc-in valid [:confirmation :items] []))))
    (is (not (flow/valid-config? (update valid :confirmation dissoc :target)))))
  (testing "the work is either an :execute! function or an :effect plan"
    (is (not (flow/valid-config? (dissoc valid :execute!))))
    (is (not (flow/valid-config? (assoc valid :execute! "not a function"))))
    (is (flow/valid-config? (-> valid (dissoc :execute!)
                                (assoc :effect [(effect/kubectl "p" "cordon" "n")]))))))

(deftest malformed-change-is-refused-test
  (testing "change! refuses a malformed config before prompting anyone"
    (is (false? (flow/change! {})))
    (is (false? (flow/change! {:confirmation {:action "x"} :execute! (constantly true)})))))

(deftest dry-run-never-runs-the-mutation-test
  (testing "under dry-run a legacy thunk is never called - and there is no prompt to hang on"
    (let [called (atom false)]
      (binding [flow/*dry-run* true]
        (is (true? (flow/change! (assoc valid :execute! (fn [] (reset! called true) true))))))
      (is (false? @called) "the thunk must not run during a dry-run")))
  (testing "under dry-run an effect plan is described, not performed, and post-checks are skipped"
    (let [checked (atom false)]
      (binding [flow/*dry-run* true]
        (is (true? (flow/change! {:confirmation {:action "cordon a node" :target "prod" :items ["node-1"]}
                                  :effect [(effect/kubectl "prod" "cordon" "node-1")]
                                  :post-checks [{:description "x"
                                                 :check (fn [] (reset! checked true) true)}]}))))
      (is (false? @checked) "post-checks must not run during a dry-run"))))
