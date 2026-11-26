;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.external-dns-invalid-name-test
  "A declared hostname that is no DNS name is never put to dig, and is
   reported as an invalid name rather than as a DNS server that did not
   answer."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gumshoe.detectives.external-dns :as detective]
            [gumshoe.external-dns :as external-dns]))

(defn- recording-query
  [asked]
  (fn [type name] (swap! asked conj [type name]) []))

(deftest an-invalid-name-is-never-queried
  (doseq [host ["app.example.org " "app example.org" "-fetc.example.org" "+short"]]
    (let [asked (atom [])]
      (is (= {:invalid true} (external-dns/resolution (recording-query asked) host)) host)
      (is (= [] @asked) host))))

(deftest a-valid-name-is-queried-for-both-types
  (let [asked (atom [])]
    (is (= {:a [] :aaaa []} (external-dns/resolution (recording-query asked) "*.example.org")))
    (is (= [["A" "bookstore-probe.example.org"] ["AAAA" "bookstore-probe.example.org"]] @asked))))

(deftest an-invalid-name-is-reported-as-invalid
  (let [host "app.example.org "
        sources (external-dns/hostname-sources
                 {:endpoints {:items [{:spec {:endpoints [{:dnsName host :recordType "A"}]}}]}})
        [finding :as all] (detective/detect-unresolved-hostnames
                           {"server" "192.0.2.53"
                            "sources" sources
                            "resolved" {host (external-dns/resolution (fn [& _] (throw (ex-info "never asked" {}))) host)}})]
    (is (= 1 (count all)))
    (is (= :warning (:severity finding)))
    (is (= "declared hostname is not a valid DNS name (declared by DNSEndpoint)" (:summary finding)))
    (is (not (str/includes? (:summary finding) "did not answer")))
    (is (not (str/includes? (:hint finding) "unreachable")))))

(deftest a-server-that-did-not-answer-keeps-its-wording
  (let [[finding] (detective/detect-unresolved-hostnames
                   {"server" "192.0.2.53"
                    "sources" {"app.example.org" #{"Ingress"}}
                    "resolved" {"app.example.org" {:a nil :aaaa nil}}})]
    (is (= "192.0.2.53 did not answer the query" (:summary finding)))))
