;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dns-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.dns :as detectives]
            [gumshoe.dns :as dns]
            [gumshoe.reads.result :as result]))

(defn- answered
  "A DNS read that answered with these records; none is NOERROR without
   records, which is :absent."
  [& records]
  (if (seq records)
    (result/present {:source :dns} {:items (vec records) :status "NOERROR"})
    (result/absent {:source :dns} {:items [] :status "NOERROR"})))

(def ^:private nxdomain (result/absent {:source :dns} {:items [] :status "NXDOMAIN"}))

(defn- unanswered [why] (result/unread {:source :dns} why {:status why}))

(defn- serial [s] (result/derived (answered (str "ns1. host. " s " 3600 900 1209600 300")) #(some-> (first %) dns/parse-soa-serial)))

(defn- summaries
  [findings]
  (set (map :summary findings)))

(deftest dig-args-test
  (testing "queries print the header and answer with tight timeouts, type and name as option values"
    (is (= ["dig" "+noall" "+comments" "+answer" "+time=3" "+tries=1" "@dns.example.org" "-t" "SOA" "-q" "example.org"]
           (dns/dig-args {:server "dns.example.org"} "SOA" "example.org"))))
  (testing "transports are forced explicitly"
    (is (= ["dig" "+noall" "+comments" "+answer" "+time=3" "+tries=1" "-4" "@dns.example.org" "-t" "SOA" "-q" "example.org"]
           (dns/dig-args {:server "dns.example.org" :transport :ipv4} "SOA" "example.org")))
    (is (= ["dig" "+noall" "+comments" "+answer" "+time=3" "+tries=1" "-6" "@dns.example.org" "-t" "SOA" "-q" "example.org"]
           (dns/dig-args {:server "dns.example.org" :transport :ipv6} "SOA" "example.org"))))
  (testing "a reverse lookup uses -x, not the bare IP as a PTR name"
    (is (= ["dig" "+noall" "+comments" "+answer" "+time=3" "+tries=1" "@dns.example.org" "-x" "203.0.113.5"]
           (dns/dig-args {:server "dns.example.org" :reverse? true} "PTR" "203.0.113.5")))))

(deftest parsing-test
  (is (= "2026070401"
         (dns/parse-soa-serial "ns1.example.org. hostmaster.example.org. 2026070401 3600 900 1209600 300")))
  (is (nil? (dns/parse-soa-serial "short answer")))
  (is (= "ns1.example.org" (dns/strip-dot "ns1.example.org."))))

(deftest transport-detective-test
  (is (= #{"server does not answer over IPv4"}
         (summaries (detectives/detect-transport-problems
                     {"server" "dns.example.org"
                      "transports" {:ipv4 (unanswered "REFUSED") :ipv6 (answered "x")}}))))
  (is (= #{"server does not answer over IPv6"}
         (summaries (detectives/detect-transport-problems
                     {"server" "dns.example.org"
                      "transports" {:ipv4 (answered "x") :ipv6 (unanswered "dig exited 9")}}))))
  (is (empty? (detectives/detect-transport-problems
               {"server" "dns.example.org"
                "transports" {:ipv4 (answered "x") :ipv6 nxdomain}}))))

(deftest nameserver-detective-test
  (testing "a zone without NS records is critical"
    (is (= #{"zone has no NS records"}
           (summaries (detectives/detect-zone-problems {"zone" "example.org" "nameservers" (answered)}))))
    (is (empty? (detectives/detect-nameserver-problems
                 {"zone" "example.org" "nameservers" (answered) "addresses" {}}))))
  (testing "NXDOMAIN is a zone the entry server does not have"
    (is (= #{"zone does not exist on the entry server"}
           (summaries (detectives/detect-zone-problems {"zone" "example.org" "nameservers" nxdomain})))))
  (testing "an NS query that did not answer is unread, naming the read"
    (is (= [{:severity :warning :reports-unread true :component "example.org"
             :summary "could not read NS example.org at 192.0.2.53 (REFUSED)"
             :hint "whether the zone has nameservers is unknown, so the checks that ask each nameserver judge nothing"}]
           (detectives/detect-zone-problems
            {"zone" "example.org"
             "nameservers" (result/unread (dns/ref-of {:server "192.0.2.53"} "NS" "example.org") "REFUSED")})))
    (is (empty? (detectives/detect-nameserver-problems
                 {"zone" "example.org" "nameservers" (unanswered "REFUSED") "addresses" nil}))))
  (testing "nameservers that answered are no zone problem"
    (is (empty? (detectives/detect-zone-problems {"zone" "example.org" "nameservers" (answered "ns1.example.org")}))))
  (testing "a single nameserver has no redundancy"
    (is (contains? (summaries (detectives/detect-nameserver-problems
                               {"zone" "example.org"
                                "nameservers" (answered "ns1.example.org")
                                "addresses" {"ns1.example.org" {:a (answered "203.0.113.1") :aaaa (answered "2001:db8::1")}}}))
                   "zone has a single nameserver - no redundancy")))
  (testing "missing A is critical, missing AAAA warns"
    (let [findings (detectives/detect-nameserver-problems
                    {"zone" "example.org"
                     "nameservers" (answered "ns1.example.org" "ns2.example.org")
                     "addresses" {"ns1.example.org" {:a (answered) :aaaa (answered "2001:db8::1")}
                                  "ns2.example.org" {:a (answered "203.0.113.2") :aaaa nxdomain}}})]
      (is (= #{"nameserver has no A record" "nameserver has no AAAA record"}
             (summaries findings))))))

(deftest replication-detective-test
  (testing "identical serials everywhere is healthy"
    (is (empty? (detectives/detect-replication-problems
                 {"zone" "example.org"
                  "serials" {"ns1.example.org" (serial "2026070401")
                             "ns2.example.org" (serial "2026070401")}}))))
  (testing "diverging serials mean broken replication"
    (is (= #{"SOA serials diverge: ns1.example.org=2026070401, ns2.example.org=2026070309"}
           (summaries (detectives/detect-replication-problems
                       {"zone" "example.org"
                        "serials" {"ns1.example.org" (serial "2026070401")
                                   "ns2.example.org" (serial "2026070309")}})))))
  (testing "a nameserver that does not answer is critical and does not count as divergence"
    (let [findings (detectives/detect-replication-problems
                    {"zone" "example.org"
                     "serials" {"ns1.example.org" (serial "2026070401")
                                "ns2.example.org" (unanswered "SERVFAIL")}})]
      (is (= #{"nameserver does not answer the SOA query"} (summaries findings)))))
  (testing "a nameserver that answers without an SOA record does not serve the zone"
    (is (= #{"nameserver serves no SOA record for the zone"}
           (summaries (detectives/detect-replication-problems
                       {"zone" "example.org"
                        "serials" {"ns1.example.org" (serial "2026070401")
                                   "ns2.example.org" (result/derived nxdomain (constantly nil))}})))))
  (testing "an SOA answer without a serial is named, not compared"
    (is (= #{"nameserver's SOA record carries no serial"}
           (summaries (detectives/detect-replication-problems
                       {"zone" "example.org"
                        "serials" {"ns1.example.org" (serial "2026070401")
                                   "ns2.example.org" (result/derived (answered "garbage") (constantly nil))}}))))))

(deftest probe-detective-test
  (testing "identical answers everywhere is healthy"
    (is (empty? (detectives/detect-probe-problems
                 {"probes" {"grafana.example.org" {"ns1" (answered "203.0.113.10")
                                                 "ns2" (answered "203.0.113.10")}}}))))
  (testing "an empty answer on one nameserver is critical"
    (is (contains? (summaries (detectives/detect-probe-problems
                               {"probes" {"grafana.example.org" {"ns1" (answered "203.0.113.10")
                                                               "ns2" nxdomain}}}))
                   "no A record on nameserver ns2")))
  (testing "diverging answers warn with the details"
    (is (contains? (summaries (detectives/detect-probe-problems
                               {"probes" {"grafana.example.org" {"ns1" (answered "203.0.113.10")
                                                               "ns2" (answered "203.0.113.99")}}}))
                   "nameservers return different answers for this name")))
  (testing "a nameserver that did not answer judges nothing for the name"
    (is (empty? (detectives/detect-probe-problems
                 {"probes" {"grafana.example.org" {"ns1" (answered "203.0.113.10")
                                                 "ns2" (unanswered "SERVFAIL")}}})))))

(deftest dig-diagnostics-are-not-records
  (testing "dig prints ; lines besides the records; they are not answers"
    (is (= ["ns1.example.org. hostmaster.example.org. 2026070401 3600 900 1209600 300"]
           (dns/parse-answer (str ";; communications error to 192.0.2.1#53: timed out\n"
                                  "example.org.\t300\tIN\tSOA\tns1.example.org. hostmaster.example.org. 2026070401 3600 900 1209600 300\n"))))
    (is (= [] (dns/parse-answer ";; communications error to 192.0.2.1#53: timed out\n"))))
  (testing "the SOA serial comes from the record, not from the error line"
    (with-redefs [gumshoe.shell/execute
                  (fn [& _] {:exit 0 :out (str ";; communications error to 192.0.2.1#53: timed out\n"
                                               ";; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 1\n"
                                               "example.org. 300 IN SOA ns1. host. 2026070401 3600 900 1209600 300\n")})]
      (is (= "2026070401" (dns/soa-serial {} "example.org"))))))
