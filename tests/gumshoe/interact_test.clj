;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.interact-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.fuzzy-finder :as fuzzy]
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

(deftest choose-one-seeds-picker-test
  (let [calls (atom [])]
    (with-redefs [fuzzy/select-single (fn [& args] (swap! calls conj (vec args)) nil)]
      (testing "a typed near-miss seeds the picker query and forbids auto-accept"
        (reset! calls [])
        (interact/choose-one "Node" ["worker-1" "worker-2"] "worker-9")
        (is (= [["Node" ["worker-1" "worker-2"] {:query "worker-9" :auto-select? false :preview nil}]] @calls)
            "the near-miss name seeds the fuzzy query, and auto-select? is false so it never resolves silently"))
      (testing "no provided value opens the picker without a seed (auto-select left on)"
        (reset! calls [])
        (interact/choose-one "Node" ["worker-1" "worker-2"] nil)
        (is (= [["Node" ["worker-1" "worker-2"] {:preview nil}]] @calls)
            "with nothing typed there is no seed to guard, so the plain interactive pick is used"))
      (testing "a declared preview command is passed through to the picker"
        (reset! calls [])
        (interact/choose-one "Node" ["worker-1" "worker-2"] nil "kubectl describe node {}")
        (is (= "kubectl describe node {}" (:preview (last (first @calls))))
            "the book's :preview reaches fzf so the operator sees the node before picking"))
      (testing "candidates are sorted before they reach the picker"
        (reset! calls [])
        (interact/choose-one "Node" ["worker-2" "worker-1"] "worker")
        (is (= ["worker-1" "worker-2"] (second (first @calls))))))))

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
