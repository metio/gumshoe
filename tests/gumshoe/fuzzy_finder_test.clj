;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.fuzzy-finder-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.fuzzy-finder :as fuzzy]))

(def ^:private single-select-args #'fuzzy/single-select-args)

(deftest single-select-args-test
  (testing "a seed query is passed to fzf"
    (is (some #{"--query=worker-1"} (single-select-args "Node" "worker-1" false))))
  (testing "no auto-accept when auto-select? is false - the operator must confirm a seeded match"
    (let [args (single-select-args "Node" "worker-1" false)]
      (is (not (some #{"--select-1"} args))
          "a mistyped name must not resolve to a lone fuzzy match without a keypress")
      (is (not (some #{"--exit-0"} args))
          "a no-match query keeps the picker open to edit rather than resolving to nothing")))
  (testing "auto-accept when auto-select? is true - a launcher may run a unique match"
    (let [args (single-select-args "Which book?" "scale up" true)]
      (is (some #{"--select-1"} args))
      (is (some #{"--exit-0"} args))
      (is (some #{"--query=scale up"} args))))
  (testing "a blank query adds no --query flag"
    (is (not (some #(clojure.string/starts-with? % "--query=")
                   (single-select-args "Node" nil true))))))
