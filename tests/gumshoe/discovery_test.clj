;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.discovery-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.discovery :as discovery]))

(deftest url-test
  (testing "a bare domain becomes its well-known SRE-config URL"
    (is (= "https://example.org/.well-known/sre.json" (discovery/url "example.org")))
    (is (= "https://example.org/.well-known/sre.json" (discovery/url "  example.org  "))))
  (testing "a value that already looks like a URL is passed through"
    (is (= "https://example.org/custom/sre.json" (discovery/url "https://example.org/custom/sre.json"))))
  (testing "blank/nil is no URL"
    (is (nil? (discovery/url "")))
    (is (nil? (discovery/url nil)))))

(deftest value-test
  (let [discovered {:dns {:server "dns.example.org"}
                    :matrix {:domain "example.org" :homeserver "https://synapse.example.org"}
                    :loki {:namespace "loki"}}]
    (testing "a published value is read at its env.edn path"
      (is (= "dns.example.org" (discovery/value discovered [:dns :server])))
      (is (= "https://synapse.example.org" (discovery/value discovered [:matrix :homeserver])))
      (is (= "loki" (discovery/value discovered [:loki :namespace]))))
    (testing "an unpublished value, or no discovery at all, is nil"
      (is (nil? (discovery/value discovered [:opennebula :frontend])))
      (is (nil? (discovery/value nil [:dns :server]))))))
