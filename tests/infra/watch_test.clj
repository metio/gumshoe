;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.watch-test
  (:require [clojure.test :refer [deftest is testing]]
            [infra.watch :as watch]))

(deftest combine-test
  (testing "combine merges watchers and drops nils"
    (let [w (watch/combine [(fn [] ["a"]) nil (fn [] ["b" "c"])])]
      (is (= ["a" "b" "c"] (w))))
    (testing "all-nil combines to nothing"
      (is (nil? (watch/combine [nil nil]))))))

;; The event and PVC watchers read from the cluster, so their I/O is exercised
;; only against a live cluster; combine (the pure composition) is what the
;; post-checks depend on to run several watchers as one.
