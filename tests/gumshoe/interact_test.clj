;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.interact-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.interact :as interact]))

(deftest valid-choice-test
  (testing "accepts a candidate"
    (is (interact/valid-choice? ["a" "b"] "a")))
  (testing "rejects a non-candidate"
    (is (not (interact/valid-choice? ["a" "b"] "c"))))
  (testing "rejects nil"
    (is (not (interact/valid-choice? ["a" "b"] nil)))))

(deftest choose-one-test
  (testing "returns nil when there is nothing to choose from"
    (is (nil? (interact/choose-one "Node" [] "worker-1"))))
  (testing "uses a valid provided value without interaction"
    (is (= "worker-1" (interact/choose-one "Node" ["worker-1" "worker-2"] "worker-1")))))

(deftest choose-many-test
  (testing "returns nil when there is nothing to choose from"
    (is (nil? (interact/choose-many "Certificate" [] ["a"]))))
  (testing "uses valid provided values without interaction"
    (is (= ["a" "b"] (interact/choose-many "Certificate" ["a" "b" "c"] ["a" "b"])))))

(deftest choose-namespaced-test
  (testing "uses valid namespace and name flags without interaction"
    (is (= "monitoring/prometheus"
           (interact/choose-namespaced "Service"
                                       ["monitoring/prometheus" "loki/loki"]
                                       "monitoring"
                                       "prometheus")))))

(deftest confirmation-message-test
  (testing "states the action, the target, and every item"
    (is (= (str "This will delete a PersistentVolumeClaim on 'kube.example.org':\n"
                "  - moodle/data\n"
                "  - moodle/cache")
           (interact/confirmation-message {:action "delete a PersistentVolumeClaim"
                                           :target "kube.example.org"
                                           :items ["moodle/data" "moodle/cache"]})))))
