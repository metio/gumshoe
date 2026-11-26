;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.hosts-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.hosts :as detectives]
            [gumshoe.hosts :as hosts]))

(defn- vector-answer [label pairs]
  {:ok true :type :vector
   :series (vec (for [[host value extra] pairs]
                  {:labels (merge {label host} extra) :value value}))})

(def answers
  {:cores (vector-answer "node" [["w1" 8.0] ["w2" 4.0]])
   :memory (vector-answer "node" [["w1" 1.6e10] ["w2" 8.0e9]])
   :boot (vector-answer "node" [["w1" 1.7e9]])
   :uname (vector-answer "node" [["w1" 1.0 {"release" "6.8.0" "machine" "x86_64"}]])
   :os (vector-answer "node" [["w1" 1.0 {"name" "Ubuntu" "version_id" "24.04"}]])
   :up (vector-answer "node" [["w1" 1.0] ["w2" 1.0] ["w3" 0.0]])})

(deftest queries-aggregate-by-the-host-label
  (let [q (hosts/queries {:host-label "node" :selector "cluster=\"a\"" :job "node"})]
    (is (= "count by (node) (count by (node, cpu) (node_cpu_seconds_total{mode=\"idle\",cluster=\"a\"}))" (:cores q))
        "cores count distinct cpu labels, so a host scraped twice counts once")
    (is (= "max by (node) (node_memory_MemTotal_bytes{cluster=\"a\"})" (:memory q)))
    (is (= "max by (node) (up{job=~\"node\",cluster=\"a\"})" (:up q)))
    (is (= "max by (node) (node_boot_time_seconds{})" (:boot (hosts/queries {:host-label "node"}))) "no selector, no stray comma")))

(deftest host-names
  (is (= "a.example.org" (hosts/host-name "instance" "A.example.org:9100")))
  (is (= "2001:db8::1" (hosts/host-name "instance" "[2001:db8::1]:9100")))
  (is (= "10.0.0.1" (hosts/host-name "instance" "10.0.0.1")))
  (is (= "w1:x" (hosts/host-name "node" "W1:x")) "only an instance loses a port")
  (is (nil? (hosts/host-name "node" nil))))

(deftest rows-join-every-answer-per-host
  (is (= [{:host "w1" :cores 8 :memory-bytes 16000000000 :kernel "6.8.0" :machine "x86_64"
           :os "Ubuntu" :os-version "24.04" :boot-time "2023-11-14T22:13:20Z" :up true}
          {:host "w2" :cores 4 :memory-bytes 8000000000 :kernel nil :machine nil
           :os nil :os-version nil :boot-time nil :up true}
          {:host "w3" :cores nil :memory-bytes nil :kernel nil :machine nil
           :os nil :os-version nil :boot-time nil :up false}]
         (hosts/rows "node" answers)))
  (testing "a failed answer leaves its facts nil and is listed"
    (let [broken (assoc answers :cores {:ok false :error "HTTP 502"})]
      (is (every? nil? (map :cores (hosts/rows "node" broken))))
      (is (= {:cores "HTTP 502"} (hosts/failures broken))))))

(deftest declarations
  (is (= ["w1" "w2"] (hosts/declared-hosts "# staging\nw1\n\n  w2   # worker\n")))
  (is (= {:missing ["w4"] :unexpected ["w3"]}
         (hosts/compare-hosts ["W1" "w2" "w4"] (hosts/rows "node" answers)))))

(defn- run
  "The detectives over evidence whose queries all answered, unless the
   evidence itself says otherwise."
  [evidence]
  (let [evidence (merge (zipmap (vals hosts/answer-keys) (repeat true)) evidence)
        checked (detective/mark-unread detectives/detectives evidence)]
    {:checked checked :findings (detective/run-detectives checked evidence)}))

(defn- summaries [findings] (set (map (juxt :component :summary) findings)))

(deftest detectives-judge-the-rows
  (let [{:keys [findings checked]} (run {"source" "u" "hosts" (hosts/rows "node" answers) "failed" {} "declared" ["w1" "w2" "w4"]})]
    (is (= #{["w3" "node_exporter is down"]
             ["w4" "declared host reports no node_exporter facts"]
             ["w3" "host reports facts but is not declared"]}
           (summaries findings)))
    (is (not-any? :unread checked))))

(deftest a-host-without-cores-or-memory-is-incomplete
  (is (= #{["w9" "reports no CPU count or memory"] ["w8" "reports no memory"]}
         (summaries (:findings (run {"source" "u" "hosts" [{:host "w9" :up true} {:host "w8" :up nil :cores 2}] "failed" {}}))))))

(deftest a-down-host-is-not-also-incomplete
  (is (= #{["w3" "node_exporter is down"]}
         (summaries (:findings (run {"source" "u" "hosts" [{:host "w3" :up false}] "failed" {}}))))))

(deftest without-a-declaration-the-comparison-reads-nothing
  (let [{:keys [checked findings]} (run {"source" "u" "hosts" (hosts/rows "node" answers) "failed" {}})]
    (is (= #{"declared-hosts-report" "reporting-hosts-declared"} (set (map :name (filter :unread checked)))))
    (is (not-any? #(str/includes? (:summary %) "declared") findings))))

(deftest an-empty-read-is-nothing-checked
  (testing "a selector that matched nothing does not pass"
    (let [{:keys [checked findings]} (run {"source" "u" "hosts" [] "failed" {}})]
      (is (empty? findings))
      (is (detective/nothing-checked? checked findings))))
  (testing "with a declaration, every declared host is missing - a finding, not silence"
    (is (= #{["w1" "declared host reports no node_exporter facts"]}
           (summaries (:findings (run {"source" "u" "hosts" [] "failed" {} "declared" ["w1"]})))))))

(deftest failed-queries-are-reported-and-do-not-fake-missing-hosts
  (let [{:keys [findings]} (run {"source" "u" "hosts" [] "failed" {:cores "HTTP 502" :up "HTTP 502"} "declared" ["w1"]})]
    (is (= #{["u" "the cores query failed"] ["u" "the up query failed"]} (summaries findings)))))
