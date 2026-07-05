;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.discovery-test
  (:require [clojure.test :refer [deftest is testing]]
            [infra.discovery :as discovery]))

(deftest url-test
  (testing "a bare domain becomes its well-known SRE-config URL"
    (is (= "https://infra.run/.well-known/sre.json" (discovery/url "infra.run")))
    (is (= "https://infra.run/.well-known/sre.json" (discovery/url "  infra.run  "))))
  (testing "a value that already looks like a URL is passed through"
    (is (= "https://example.org/custom/sre.json" (discovery/url "https://example.org/custom/sre.json"))))
  (testing "blank/nil is no URL"
    (is (nil? (discovery/url "")))
    (is (nil? (discovery/url nil)))))

(deftest value-test
  (let [discovered {:dns {:server "dns.infra.run"}
                    :matrix {:domain "infra.run" :homeserver "https://synapse.infra.run"}
                    :loki {:namespace "loki"}}]
    (testing "a published value is read at its env.edn path"
      (is (= "dns.infra.run" (discovery/value discovered [:dns :server])))
      (is (= "https://synapse.infra.run" (discovery/value discovered [:matrix :homeserver])))
      (is (= "loki" (discovery/value discovered [:loki :namespace]))))
    (testing "an unpublished value, or no discovery at all, is nil"
      (is (nil? (discovery/value discovered [:opennebula :frontend])))
      (is (nil? (discovery/value nil [:dns :server]))))))
