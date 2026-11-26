;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.hosts-failed-query-test
  "A host check whose query failed is unread and reports nothing, while the
   failed query itself is reported."
  (:require [clojure.test :refer [deftest is]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.hosts :as detectives]
            [gumshoe.hosts :as hosts]))

(defn- answer [host value] {:labels {"node" host} :value value})

(defn- ok [& series] {:ok true :series (vec series)})

(def ^:private answered
  {:cores (ok (answer "a" 4.0)) :memory (ok (answer "a" 8.0E9))
   :boot (ok) :uname (ok) :os (ok) :up (ok (answer "a" 1.0))})

(defn- evidence
  [answers]
  (merge {"source" "u"
          "hosts" (hosts/rows "node" answers)
          "failed" (hosts/failures answers)}
         (hosts/answered answers)))

(defn- judged
  [answers]
  (let [ev (evidence answers)
        checked (detective/mark-unread detectives/detectives ev)]
    {:unread (set (map :name (filter :unread checked)))
     :findings (set (map (juxt :detective :summary) (detective/run-detectives checked ev)))}))

(deftest the-answered-markers-follow-each-query-test
  (is (= {"up-answered" true "cores-answered" true "memory-answered" true} (hosts/answered answered)))
  (is (= {"up-answered" nil "cores-answered" true "memory-answered" true}
         (hosts/answered (assoc answered :up {:ok false :error "HTTP 503"})))))

(deftest a-failed-up-query-leaves-the-up-check-unread-test
  (let [{:keys [unread findings]} (judged (assoc answered :up {:ok false :error "HTTP 503"}))]
    (is (contains? unread "host-exporters-up"))
    (is (not (contains? unread "host-facts-complete")))
    (is (= #{["host-queries" "the up query failed"]} findings))))

(deftest a-failed-cores-query-blames-no-scrape-test
  (let [{:keys [unread findings]} (judged (assoc answered :cores {:ok false :error "HTTP 503"}))]
    (is (contains? unread "host-facts-complete"))
    (is (not-any? #(re-find #"reports no" (second %)) findings))
    (is (= #{["host-queries" "the cores query failed"]} findings))))

(deftest every-query-failing-still-reports-the-failures-test
  (let [down {:ok false :error "HTTP 503"}
        {:keys [findings]} (judged (zipmap (keys answered) (repeat down)))]
    (is (= 6 (count findings)))
    (is (every? #(= "host-queries" (first %)) findings))))

(deftest a-down-exporter-is-judged-when-up-answered-test
  (is (= #{["host-exporters-up" "node_exporter is down"]}
         (:findings (judged (assoc answered :up (ok (answer "a" 0.0))))))))

(deftest the-detect-functions-guard-their-joins-test
  (let [ev (assoc (evidence (assoc answered :up (ok (answer "a" 0.0)))) "up-answered" nil "cores-answered" nil)]
    (is (empty? (detectives/detect-down ev)))
    (is (empty? (detectives/detect-incomplete (assoc ev "hosts" [{:host "a" :up true}]))))))
