;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.dns-test
  (:require [clojure.test :refer [deftest is testing]]
            [infra.detectives.dns :as detectives]
            [infra.dns :as dns]))

(defn- summaries
  [findings]
  (set (map :summary findings)))

(deftest dig-args-test
  (testing "queries are short-answer with tight timeouts"
    (is (= ["dig" "+short" "+time=3" "+tries=1" "@dns.infra.run" "SOA" "infra.run"]
           (dns/dig-args {:server "dns.infra.run"} "SOA" "infra.run"))))
  (testing "transports are forced explicitly"
    (is (= ["dig" "+short" "+time=3" "+tries=1" "-4" "@dns.infra.run" "SOA" "infra.run"]
           (dns/dig-args {:server "dns.infra.run" :transport :ipv4} "SOA" "infra.run")))
    (is (= ["dig" "+short" "+time=3" "+tries=1" "-6" "@dns.infra.run" "SOA" "infra.run"]
           (dns/dig-args {:server "dns.infra.run" :transport :ipv6} "SOA" "infra.run")))))

(deftest parsing-test
  (is (= "2026070401"
         (dns/parse-soa-serial "ns1.infra.run. hostmaster.infra.run. 2026070401 3600 900 1209600 300")))
  (is (nil? (dns/parse-soa-serial "short answer")))
  (is (= "ns1.infra.run" (dns/strip-dot "ns1.infra.run."))))

(deftest transport-detective-test
  (is (= #{"server does not answer over IPv4"}
         (summaries (detectives/detect-transport-problems
                     {"server" "dns.infra.run"
                      "transports" {:ipv4 false :ipv6 true}}))))
  (is (= #{"server does not answer over IPv6"}
         (summaries (detectives/detect-transport-problems
                     {"server" "dns.infra.run"
                      "transports" {:ipv4 true :ipv6 false}}))))
  (is (empty? (detectives/detect-transport-problems
               {"server" "dns.infra.run"
                "transports" {:ipv4 true :ipv6 true}}))))

(deftest nameserver-detective-test
  (testing "a zone without NS records is critical"
    (is (= #{"zone has no NS records"}
           (summaries (detectives/detect-nameserver-problems
                       {"zone" "infra.run" "nameservers" [] "addresses" {}})))))
  (testing "a single nameserver has no redundancy"
    (is (contains? (summaries (detectives/detect-nameserver-problems
                               {"zone" "infra.run"
                                "nameservers" ["ns1.infra.run"]
                                "addresses" {"ns1.infra.run" {:a ["203.0.113.1"] :aaaa ["2001:db8::1"]}}}))
                   "zone has a single nameserver - no redundancy")))
  (testing "missing A is critical, missing AAAA warns"
    (let [findings (detectives/detect-nameserver-problems
                    {"zone" "infra.run"
                     "nameservers" ["ns1.infra.run" "ns2.infra.run"]
                     "addresses" {"ns1.infra.run" {:a [] :aaaa ["2001:db8::1"]}
                                  "ns2.infra.run" {:a ["203.0.113.2"] :aaaa []}}})]
      (is (= #{"nameserver has no A record" "nameserver has no AAAA record"}
             (summaries findings))))))

(deftest replication-detective-test
  (testing "identical serials everywhere is healthy"
    (is (empty? (detectives/detect-replication-problems
                 {"zone" "infra.run"
                  "serials" {"ns1.infra.run" "2026070401"
                             "ns2.infra.run" "2026070401"}}))))
  (testing "diverging serials mean broken replication"
    (is (= #{"SOA serials diverge: ns1.infra.run=2026070401, ns2.infra.run=2026070309"}
           (summaries (detectives/detect-replication-problems
                       {"zone" "infra.run"
                        "serials" {"ns1.infra.run" "2026070401"
                                   "ns2.infra.run" "2026070309"}})))))
  (testing "a nameserver that does not answer is critical and does not count as divergence"
    (let [findings (detectives/detect-replication-problems
                    {"zone" "infra.run"
                     "serials" {"ns1.infra.run" "2026070401"
                                "ns2.infra.run" nil}})]
      (is (= #{"nameserver does not answer the SOA query"} (summaries findings))))))

(deftest probe-detective-test
  (testing "identical answers everywhere is healthy"
    (is (empty? (detectives/detect-probe-problems
                 {"probes" {"grafana.infra.run" {"ns1" ["203.0.113.10"]
                                                 "ns2" ["203.0.113.10"]}}}))))
  (testing "an empty answer on one nameserver is critical"
    (is (contains? (summaries (detectives/detect-probe-problems
                               {"probes" {"grafana.infra.run" {"ns1" ["203.0.113.10"]
                                                               "ns2" []}}}))
                   "no A record on nameserver ns2")))
  (testing "diverging answers warn with the details"
    (is (contains? (summaries (detectives/detect-probe-problems
                               {"probes" {"grafana.infra.run" {"ns1" ["203.0.113.10"]
                                                               "ns2" ["203.0.113.99"]}}}))
                   "nameservers return different answers for this name"))))
