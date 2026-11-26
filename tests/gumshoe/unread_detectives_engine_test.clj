;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.unread-detectives-engine-test
  "run-detectives keeps no judgement of a detective that read nothing: only
   findings marked :reports-unread, which say what did not answer, and
   crashes come through."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]))

(def ^:private judging
  {:name "judging" :requires ["things"] :joins ["things"]
   :detect (fn [_] [{:severity :critical :component "c" :summary "no things found"}])})

(def ^:private explaining
  {:name "explaining" :requires ["things"]
   :detect (fn [_] [{:severity :warning :component "c" :summary "the things read did not answer"
                     :reports-unread true}
                    {:severity :critical :component "c" :summary "no things found"}])})

(def ^:private crashing
  {:name "crashing" :requires ["things"] :detect (fn [_] (throw (ex-info "boom" {})))})

(deftest an-unread-detective-contributes-no-judgement-test
  (testing "marked unread by mark-unread"
    (let [checked (detective/mark-unread [judging] {"things" nil})]
      (is (:unread (first checked)))
      (is (= [] (vec (detective/run-detectives checked {"things" nil}))))))
  (testing "unmarked, judged unread over the evidence"
    (is (= [] (vec (detective/run-detectives [judging] {}))))))

(deftest a-finding-about-the-unread-read-comes-through-without-its-marker-test
  (is (= [{:severity :warning :component "c" :summary "the things read did not answer"
           :detective "explaining"}]
         (vec (detective/run-detectives [explaining] {"things" nil})))))

(deftest a-read-detective-keeps-every-finding-and-loses-the-marker-test
  (is (= ["the things read did not answer" "no things found"]
         (map :summary (detective/run-detectives [explaining] {"things" {:items []}}))))
  (is (not-any? #(contains? % :reports-unread)
                (detective/run-detectives [explaining] {"things" {:items []}}))))

(deftest a-crash-on-unread-evidence-still-fails-the-run-test
  (let [[finding] (detective/run-detectives [crashing] {})]
    (is (:crashed finding))
    (is (= :critical (:severity finding)))))
