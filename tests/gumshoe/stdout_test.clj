;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.stdout-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.stdout :as stdout]))

(deftest data-table-test
  (testing "aligns labels and keeps insertion order"
    (is (= (str "cluster : kube.example.org\n"
                "node    : worker-1")
           (stdout/data-table {:cluster "kube.example.org"
                               :node "worker-1"}))))
  (testing "renders nothing for empty data"
    (is (= "" (stdout/data-table {})))))
