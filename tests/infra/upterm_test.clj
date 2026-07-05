;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.upterm-test
  (:require [clojure.test :refer [deftest is testing]]
            [infra.upterm :as upterm]))

(deftest host-args-test
  (testing "with no restriction, a plain host session"
    (is (= ["upterm" "host"] (upterm/host-args []))))
  (testing "each named user becomes a --github-user flag, so joins are restricted"
    (is (= ["upterm" "host" "--github-user" "alice" "--github-user" "bob"]
           (upterm/host-args ["alice" "bob"])))))
