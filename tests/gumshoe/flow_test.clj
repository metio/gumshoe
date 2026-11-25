;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.flow-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.effect :as effect]
            [gumshoe.flow :as flow]
            [gumshoe.outcome :as outcome]
            [gumshoe.shell]
            [gumshoe.interact :as interact]))

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

(deftest precondition-gates-the-confirmed-change-test
  (testing "a precondition that no longer holds aborts after the confirm, before any work"
    (with-redefs [interact/confirm! (constantly true)]
      (let [ran (atom false)]
        (is (false? (flow/change! {:confirmation {:action "drain a node" :target "prod" :items ["node-1"]}
                                   :precondition (constantly false)
                                   :execute! (fn [] (reset! ran true) true)})))
        (is (false? @ran) "the work must not run when the precondition fails"))))
  (testing "a precondition that holds lets the confirmed change proceed"
    (with-redefs [interact/confirm! (constantly true)]
      (let [ran (atom false)]
        (is (true? (flow/change! {:confirmation {:action "drain a node" :target "prod" :items ["node-1"]}
                                  :precondition (constantly true)
                                  :execute! (fn [] (reset! ran true) true)})))
        (is (true? @ran) "the work runs when the precondition holds")))))

(deftest dry-run-checks-the-precondition-and-keeps-rehearsing-test
  (testing "an unmet precondition is reported and recorded, and the rehearsal goes on"
    (let [record (outcome/fresh)
          step (fn [action held?]
                 (flow/change! {:confirmation {:action action :target "prod" :items ["x"]}
                                :precondition (constantly held?)
                                :effect [(effect/kubectl "prod" "annotate" "x" action)]}))]
      (binding [flow/*dry-run* true
                outcome/*record* record]
        (is (true? (step "create the replacement" true)))
        (is (true? (step "purge the old deployment" false))
            "a later step stays visible, since an earlier step may be what it waits for")
        (is (true? (step "label the new deployment" true))))
      (is (= ["purge the old deployment"] (:unmet @record)))
      (is (= 0 (:applied @record)) "a rehearsal applies nothing")))
  (testing "a precondition that holds records nothing"
    (let [record (outcome/fresh)]
      (binding [flow/*dry-run* true
                outcome/*record* record]
        (flow/change! (assoc valid :precondition (constantly true))))
      (is (nil? (:unmet @record))))))

(defn- unknown-op-change
  "A change whose plan cordons a node through a known op and then calls one
   nothing registers."
  []
  {:confirmation {:action "cordon and notify" :target "cluster" :items ["n1"]}
   :effect [(effect/kubectl "ctx" "cordon" "n1") [:no-such-hook "url" "event"]]})

(deftest a-plan-with-an-unknown-op-is-refused-before-anything-runs-test
  (doseq [dry-run? [false true]]
    (testing (if dry-run? "under --dry-run" "in a real run")
      (let [ran (atom []) asked (atom false) err (java.io.StringWriter.)
            thrown (with-redefs [gumshoe.shell/run-with-output (fn [& args] (swap! ran conj (vec args)) 0)
                                 interact/confirm! (fn [& _] (reset! asked true) true)]
                     (binding [flow/*dry-run* dry-run? *err* err]
                       (try (flow/change! (unknown-op-change)) nil
                            (catch clojure.lang.ExceptionInfo e e))))]
        (is (some? thrown))
        (is (= [:no-such-hook] (:unknown-ops (ex-data thrown))))
        (is (re-find #":no-such-hook" (ex-message thrown)) (ex-message thrown))
        (is (re-find #"Nothing of this step was run" (ex-message thrown)))
        (is (= [] @ran) "the effect before the unknown one never ran")
        (is (false? @asked) "the operator was never asked")
        (is (not (re-find #"would run" (str err))) "the rehearsal showed nothing")))))

(deftest a-registered-custom-op-passes-the-check-test
  (let [performed (atom nil)]
    (try
      (effect/register-effect-type! :no-such-hook {:describe (fn [[u]] (str "post to " u))
                                                     :perform (fn [[u]] (reset! performed u) true)})
      (with-redefs [gumshoe.shell/run-with-output (fn [& _] 0)
                    interact/confirm! (constantly true)]
        (binding [*err* (java.io.StringWriter.)]
          (is (true? (flow/change! (unknown-op-change))))))
      (is (= "url" @performed))
      (finally (reset! @#'effect/effect-types {})))))
