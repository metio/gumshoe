;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.summary-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.summary :as summary]))

(deftest built-in-providers-registered-test
  (testing "the clipboard and HedgeDoc providers ship in the core"
    (is (= #{:clipboard :hedgedoc}
           (into #{} (map :name) (filter (comp #{:clipboard :hedgedoc} :name) (summary/registered)))))))

(deftest usable-filters-by-availability-test
  (testing "only providers whose :available? holds are offered; a throwing check is treated as unavailable"
    (let [provs [{:name :yes :label "y" :available? (constantly true) :provide! identity}
                 {:name :no :label "n" :available? (constantly false) :provide! identity}
                 {:name :boom :label "b" :available? (fn [] (throw (ex-info "x" {}))) :provide! identity}]]
      (is (= [:yes] (map :name (summary/usable provs)))))))

(deftest plugin-can-register-a-provider-test
  (testing "a plugin adds a share target with no core change"
    (summary/register-provider!
     {:name :test-sink :label "test" :available? (constantly true) :provide! (constantly "sent")})
    (is (some #(= :test-sink (:name %)) (summary/registered)))
    (is (some #(= :test-sink (:name %)) (summary/usable)))))
