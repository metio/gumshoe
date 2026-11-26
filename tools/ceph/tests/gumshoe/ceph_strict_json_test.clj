;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.ceph-strict-json-test
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.ceph :as ceph]
            [gumshoe.shell :as shell]
            [gumshoe.ssh :as ssh]))

(defn- answering
  [out f]
  (with-redefs [shell/execute (fn [& _] {:exit 0 :out out :err ""})
                ssh/stdout-of (fn [& _] out)]
    (f)))

(deftest a-truncated-array-is-no-answer
  (answering "[{\"crash_id\":\"a\"},{\"crash_id\""
             (fn []
               (let [answer (ceph/ceph-json-answer {:host "h"} "crash" "ls-new")]
                 (is (false? (:ok? answer)) "the crash list was not read")
                 (is (nil? (:doc answer))))
               (is (nil? (ceph/ceph-json {:host "h"} "crash" "ls-new"))))))

(deftest trailing-text-is-no-answer
  (answering "[] warning: something"
             #(is (false? (:ok? (ceph/ceph-json-answer {:host "h"} "crash" "ls-new"))))))

(deftest a-whole-array-is-an-answer
  (answering "[{\"crash_id\":\"a\"}]\n"
             #(is (= {:ok? true :doc [{:crash_id "a"}]}
                     (select-keys (ceph/ceph-json-answer {:host "h"} "crash" "ls-new") [:ok? :doc])))))
