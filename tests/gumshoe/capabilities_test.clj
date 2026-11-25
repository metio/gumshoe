;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.capabilities-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.capabilities :as capabilities]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.shell :as shell]))

(deftest detect-present-is-best-effort-test
  (testing "a detector that throws counts as absent, never breaks detection"
    (capabilities/register-detector! :explodes (fn [] (throw (ex-info "boom" {}))))
    ;; a plugin detector using an assert/:pre throws an AssertionError (an Error),
    ;; which must be caught too or it aborts the whole sweep
    (capabilities/register-detector! :asserts (fn [] (assert false "nope")))
    (capabilities/register-detector! :always (fn [] true))
    (let [found (set (capabilities/detect-present))]
      (is (contains? found :always))
      (is (not (contains? found :explodes)))
      (is (not (contains? found :asserts)))))
  (testing "detect-present returns a sorted vector"
    (let [found (capabilities/detect-present)]
      (is (vector? found))
      (is (= found (vec (sort found)))))))

(deftest plugin-can-register-a-detector-test
  (testing "a plugin teaches the wizard a new capability with no core change"
    (capabilities/register-detector! :my-tool (fn [] true))
    (is (contains? (set (capabilities/registered)) :my-tool))
    (is (contains? (set (capabilities/detect-present)) :my-tool))))

(deftest answer-test
  (is (= :present (capabilities/answer true)))
  (is (= :present (capabilities/answer "truthy")))
  (is (= :absent (capabilities/answer false)))
  (is (= :unanswered (capabilities/answer nil))))

(defn- with-detectors [detectors f]
  (let [registry @#'capabilities/detectors
        before @registry]
    (reset! registry detectors)
    (try (f) (finally (reset! registry before)))))

(deftest detect-answers-keeps-an-unanswered-probe-out-of-absent-test
  (with-detectors {:answers-absent (constantly false)
                   :no-answer (constantly nil)
                   :throws-answer (fn [] (throw (ex-info "boom" {})))
                   :here (constantly true)}
    (fn []
      (is (= {:present [:here] :absent [:answers-absent] :unanswered [:no-answer :throws-answer]}
             (capabilities/detect-answers)))
      (is (= [:here] (capabilities/detect-present))))))

(deftest a-crd-detector-with-no-api-server-is-unanswered-test
  (with-detectors {:crd-tool #(kubectl/serves-crd? "tools.example.com")}
    (fn []
      (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server: i/o timeout"})]
        (is (= {:present [] :absent [] :unanswered [:crd-tool]} (capabilities/detect-answers))))
      (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Error from server (NotFound): customresourcedefinitions.apiextensions.k8s.io \"tools.example.com\" not found"})]
        (is (= {:present [] :absent [:crd-tool] :unanswered []} (capabilities/detect-answers)))))))
