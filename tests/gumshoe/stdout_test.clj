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

(deftest table-test
  (testing "aligns columns to their widest cell and trims trailing padding"
    (is (= (str "NAMESPACE  POD         STATUS\n"
                "moodle     web-1       Running\n"
                "keycloak   keycloak-0  CrashLoopBackOff")
           (stdout/table [["NAMESPACE" :ns] ["POD" :pod] ["STATUS" :status]]
                         [{:ns "moodle" :pod "web-1" :status "Running"}
                          {:ns "keycloak" :pod "keycloak-0" :status "CrashLoopBackOff"}]))))
  (testing "an accessor may be a function of the row, not just a key"
    (is (= (str "POD    RESTARTS\n"
                "web-1  3")
           (stdout/table [["POD" :pod] ["RESTARTS" (comp str :restarts)]]
                         [{:pod "web-1" :restarts 3}]))))
  (testing "the header widens a column when it is wider than every cell"
    (is (= (str "CONTAINER  IMAGE\n"
                "a          nginx")
           (stdout/table [["CONTAINER" :c] ["IMAGE" :img]]
                         [{:c "a" :img "nginx"}]))))
  (testing "a cell carrying ANSI codes still lines up (column width excludes escapes)"
    (let [out (stdout/table [["NAME" :name] ["STATUS" :status]]
                            [{:name "a" :status "[31mbad[0m"}
                             {:name "longername" :status "ok"}])
          lines (clojure.string/split-lines (stdout/strip-colors out))]
      (is (apply = (map #(clojure.string/index-of %2 %1)
                        ["STATUS" "bad" "ok"] lines))
          "the STATUS column starts at the same offset on every row despite ANSI codes")))
  (testing "no rows render nothing"
    (is (= "" (stdout/table [["A" :a]] [])))))
