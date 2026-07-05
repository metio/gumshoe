;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.recording-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.recording :as recording]))

(deftest book-slug-test
  (testing "the slug is the book path without the top directory or extension"
    (is (= "kubernetes/nodes/drain" (recording/book-slug "runbooks/kubernetes/nodes/drain.clj")))
    (is (= "ceph/upgrade" (recording/book-slug "playbooks/ceph/upgrade.clj")))
    (testing "even from an absolute path"
      (is (= "kubernetes/nodes/drain"
             (recording/book-slug "/home/seb/repo/runbooks/kubernetes/nodes/drain.clj"))))))

(deftest redaction-test
  (testing "secret-looking option values never reach the recording"
    (is (recording/secret-key? :token))
    (is (recording/secret-key? :admin-secret))
    (is (recording/secret-key? :prometheus-password))
    (is (not (recording/secret-key? :namespace)))
    (is (= {:namespace "moodle" :token "<redacted>" :admin-secret "<redacted>" :node "worker-1"}
           (recording/redact {:namespace "moodle" :token "s3cr3t" :admin-secret "matrix/admin" :node "worker-1"})))))
