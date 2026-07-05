;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pad-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.pad :as pad]))

(deftest findings->markdown-test
  (let [findings [{:severity :critical :detective "pods" :component "moodle/web-1"
                   :summary "CrashLoopBackOff" :hint "check the previous logs"}
                  {:severity :warning :detective "pods" :component "batch/job-1"
                   :summary "pod is stuck in Pending"}
                  {:severity :info :detective "storage" :component "mail/pvc-x"
                   :summary "PersistentVolumeClaim is not used"}]
        md (pad/findings->markdown "Workloads scan" {:cluster "kube.example.org" :when "2026-07-05"} findings)]
    (testing "titled, with the run's context"
      (is (str/includes? md "# Workloads scan"))
      (is (str/includes? md "kube.example.org · 2026-07-05")))
    (testing "grouped by area, the most-severe area first"
      (is (str/includes? md "## pods (2)"))
      (is (str/includes? md "## storage (1)"))
      (is (< (str/index-of md "## pods") (str/index-of md "## storage"))))
    (testing "each finding as a severity-marked bullet, with the hint as a sub-note"
      (is (str/includes? md "🔥 **moodle/web-1** — CrashLoopBackOff"))
      (is (str/includes? md "_check the previous logs_")))
    (testing "a summary tally"
      (is (str/includes? md "**1 critical, 1 warning, 1 info**"))))
  (testing "an empty subtitle (no cluster/when) is omitted, not left blank"
    (is (not (str/includes? (pad/findings->markdown "Scan" {} [{:severity :info :detective "x" :component "y" :summary "z"}])
                            "_·")))))
