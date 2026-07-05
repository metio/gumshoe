;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.stdout-test
  (:require [clojure.test :refer [deftest is testing]]
            [infra.stdout :as stdout]))

(deftest data-table-test
  (testing "aligns labels and keeps insertion order"
    (is (= (str "cluster : kube.infra.run\n"
                "node    : worker-1")
           (stdout/data-table {:cluster "kube.infra.run"
                               :node "worker-1"}))))
  (testing "renders nothing for empty data"
    (is (= "" (stdout/data-table {})))))
