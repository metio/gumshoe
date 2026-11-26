;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.external-dns-partial-answer-test
  "A hostname does not resolve only when both its A and AAAA queries answered
   with no records; one query that got no answer leaves the host unread."
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.external-dns :as external-dns]))

(defn- findings
  [answer]
  (vec (external-dns/detect-unresolved-hostnames
        {"server" "ns1" "sources" {"app.example.com" #{"Ingress"}}
         "resolved" {"app.example.com" answer}})))

(deftest one-lost-query-beside-an-empty-answer-is-unread-test
  (testing "the A query was lost, AAAA answered empty"
    (let [[finding :as all] (findings {:a nil :aaaa []})]
      (is (= 1 (count all)))
      (is (= :warning (:severity finding)))
      (is (= "ns1 did not answer the A query, and the AAAA query found no records" (:summary finding)))))
  (testing "the AAAA query was lost, A answered empty"
    (let [[finding] (findings {:a [] :aaaa nil})]
      (is (= :warning (:severity finding)))
      (is (= "ns1 did not answer the AAAA query, and the A query found no records" (:summary finding))))))

(deftest both-queries-answering-empty-is-the-critical-test
  (let [[finding] (findings {:a [] :aaaa []})]
    (is (= :critical (:severity finding)))
    (is (= "hostname does not resolve (declared by Ingress)" (:summary finding)))))

(deftest one-answered-record-resolves-whatever-the-other-query-did-test
  (is (= [] (findings {:a ["192.0.2.1"] :aaaa nil})))
  (is (= [] (findings {:a nil :aaaa ["2001:db8::1"]}))))

(deftest no-answer-at-all-stays-a-warning-test
  (is (= ["ns1 did not answer the query"] (map :summary (findings {:a nil :aaaa nil})))))
