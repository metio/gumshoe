;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.external-dns-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.external-dns :as external-dns-detectives]
            [gumshoe.external-dns :as external-dns]))

(defn- summaries
  [findings]
  (set (map :summary findings)))

(deftest hostname-harvest-test
  (testing "hostnames are collected from every source kind, deduplicated with their claimants"
    (is (= {"moodle.example.org" #{"Ingress" "HTTPRoute"}
            "*.apps.example.org" #{"Gateway"}
            "vpn.example.org" #{"DNSEndpoint"}}
           (external-dns/hostname-sources
            {:ingresses {:items [{:spec {:rules [{:host "moodle.example.org"}]}}]}
             :routes {:items [{:spec {:hostnames ["moodle.example.org"]}}]}
             :gateways {:items [{:spec {:listeners [{:hostname "*.apps.example.org"}
                                                    {:name "no-hostname"}]}}]}
             :endpoints {:items [{:spec {:endpoints [{:dnsName "vpn.example.org"}]}}]}}))))
  (testing "wildcards resolve through a synthesized probe label"
    (is (= "bookstore-probe.apps.example.org" (external-dns/probe-name "*.apps.example.org")))
    (is (= "plain.example.org" (external-dns/probe-name "plain.example.org")))))

(deftest external-dns-detective-test
  (let [evidence {"server" "dns.example.org"
                  "sources" {"resolves.example.org" #{"Ingress"}
                             "missing.example.org" #{"Ingress" "HTTPRoute"}
                             "unreachable.example.org" #{"Gateway"}}
                  "resolved" {"resolves.example.org" {:a ["203.0.113.10"] :aaaa []}
                              "missing.example.org" {:a [] :aaaa []}
                              "unreachable.example.org" {:a nil :aaaa nil}}}
        findings (external-dns-detectives/detect-unresolved-hostnames evidence)]
    (testing "missing records are critical and name their claimants"
      (is (contains? (summaries findings)
                     "hostname does not resolve (declared by HTTPRoute, Ingress)")))
    (testing "an unreachable server warns instead of blaming external-dns"
      (is (contains? (summaries findings) "dns.example.org did not answer the query")))
    (testing "resolving hostnames stay silent"
      (is (= 2 (count findings))))))
