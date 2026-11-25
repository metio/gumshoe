;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.external-dns-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.external-dns :as external-dns-detectives]
            [gumshoe.dns :as dns]
            [gumshoe.external-dns :as external-dns]
            [gumshoe.shell :as shell]))

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

(deftest only-address-records-are-checked-with-address-queries
  (is (= ["app.example.org" "plain.example.org" "www.example.org"]
         (keys (external-dns/hostname-sources
                {:endpoints {:items [{:spec {:endpoints [{:dnsName "_acme-challenge.example.org" :recordType "TXT"}
                                                         {:dnsName "example.org" :recordType "MX"}
                                                         {:dnsName "_sip._tcp.example.org" :recordType "SRV"}
                                                         {:dnsName "app.example.org" :recordType "A"}
                                                         {:dnsName "www.example.org" :recordType "CNAME"}
                                                         {:dnsName "plain.example.org"}]}}]}})))))

(def ^:private ingress-list
  {:items [{:spec {:rules [{:host "moodle.example.org"}]}}]})

(defn- collect
  "The evidence collected against a cluster that answers each kubectl get
   with (answer type), every DNS query answering one address."
  [answer]
  (with-redefs [shell/execute (fn [& argv]
                                (let [type (second (drop-while #(not= "get" %) argv))]
                                  (answer type)))
                dns/query (fn [& _] ["192.0.2.1"])]
    (binding [*err* (java.io.StringWriter.)]
      (external-dns/collect-evidence! "ctx" "dns.example.org"))))

(def ^:private unanswered
  {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 10.0.0.1:6443: i/o timeout"})

(defn- not-served
  [type]
  {:exit 1 :out "" :err (format "error: the server doesn't have a resource type \"%s\"" type)})

(defn- unread?
  [evidence]
  (detective/unread? (first external-dns-detectives/detectives) evidence))

(deftest answered-sources-test
  (is (nil? (external-dns/answered-sources {:ingresses ingress-list :routes nil :gateways {:items []} :endpoints {:items []}})))
  (is (= {"moodle.example.org" #{"Ingress"}}
         (external-dns/answered-sources {:ingresses ingress-list :routes {:items []}
                                         :gateways {:items []} :endpoints {:items []}}))))

(deftest the-check-is-unread-when-every-cluster-read-failed-test
  (let [evidence (collect (constantly unanswered))]
    (is (nil? (get evidence "sources")))
    (is (unread? evidence))))

(deftest the-check-is-unread-when-one-cluster-read-failed-test
  (let [evidence (collect #(if (= "ingresses" %) unanswered {:exit 0 :out "{\"items\":[]}" :err ""}))]
    (is (nil? (get evidence "sources")) "the ingresses' hostnames would drop out unseen")
    (is (unread? evidence))))

(deftest a-kind-the-cluster-does-not-serve-is-read-as-empty-test
  (let [evidence (collect #(if (= "ingresses" %)
                             {:exit 0 :out (json/generate-string ingress-list) :err ""}
                             (not-served %)))]
    (is (= {"moodle.example.org" #{"Ingress"}} (get evidence "sources")))
    (is (not (unread? evidence)))
    (is (empty? (external-dns-detectives/detect-unresolved-hostnames evidence)))))
