;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.mail-test
  (:require [clojure.test :refer [deftest is testing]]
            [infra.detectives.mail :as detectives]
            [infra.mail :as mail]))

(defn- summaries
  [findings]
  (set (map :summary findings)))

;; ---------------------------------------------------------------------------
;; pure parsing

(deftest txt-record-value-test
  (is (= "v=spf1 -all" (mail/txt-record-value "\"v=spf1 -all\"")))
  (testing "long records split into chunks are rejoined"
    (is (= "v=DKIM1; k=rsa; p=AAAABBBB"
           (mail/txt-record-value "\"v=DKIM1; k=rsa; p=AAAA\" \"BBBB\"")))))

(deftest spf-parsing-test
  (is (= ["v=spf1 mx -all"]
         (mail/spf-records ["\"v=spf1 mx -all\"" "\"google-site-verification=abc\""])))
  (testing "the terminating all qualifier, defaulting bare all to +"
    (is (= "-" (mail/spf-all-qualifier "v=spf1 mx -all")))
    (is (= "~" (mail/spf-all-qualifier "v=spf1 mx ~all")))
    (is (= "+" (mail/spf-all-qualifier "v=spf1 +all")))
    (is (= "+" (mail/spf-all-qualifier "v=spf1 mx all")))
    (is (= "?" (mail/spf-all-qualifier "v=spf1 ?all")))
    (is (nil? (mail/spf-all-qualifier "v=spf1 include:_spf.example.org")))))

(deftest dmarc-parsing-test
  (is (= "reject" (mail/dmarc-policy ["\"v=DMARC1; p=reject; rua=mailto:x@example.org\""])))
  (is (= "none" (mail/dmarc-policy ["\"v=DMARC1; p=none\""])))
  (is (nil? (mail/dmarc-policy ["\"some other txt\""]))))

(deftest mx-parsing-test
  (is (= {:preference 10 :host "mail.example.org"} (mail/parse-mx "10 mail.example.org.")))
  (is (nil? (mail/parse-mx "malformed"))))

(deftest certificate-days-test
  (let [now (java.time.Instant/parse "2026-07-04T00:00:00Z")]
    (is (= 10 (mail/certificate-days-left now "notAfter=Jul 14 00:00:00 2026 GMT")))
    (is (nil? (mail/certificate-days-left now "notAfter=garbage")))))

;; ---------------------------------------------------------------------------
;; deliverability detectives

(deftest mx-detective-test
  (is (= #{"domain has no MX records"}
         (summaries (detectives/detect-mx-problems {"domain" "infra.run" "mx" []}))))
  (is (= #{"domain has a single MX record - no fallback receiver"}
         (summaries (detectives/detect-mx-problems {"domain" "infra.run" "mx" ["10 mail.infra.run."]}))))
  (is (empty? (detectives/detect-mx-problems {"domain" "infra.run"
                                              "mx" ["10 mail.infra.run." "20 mail2.infra.run."]}))))

(deftest spf-detective-test
  (testing "missing SPF is critical"
    (is (= #{"domain has no SPF record"}
           (summaries (detectives/detect-spf-problems {"domain" "infra.run" "spf" []})))))
  (testing "multiple SPF records are a critical permerror"
    (is (= :critical
           (:severity (first (detectives/detect-spf-problems
                              {"domain" "infra.run"
                               "spf" ["\"v=spf1 -all\"" "\"v=spf1 mx ~all\""]}))))))
  (testing "+all authorizes the whole internet - critical"
    (is (contains? (summaries (detectives/detect-spf-problems
                               {"domain" "infra.run" "spf" ["\"v=spf1 +all\""]}))
                   "SPF ends in +all - it authorizes the entire internet to send as this domain")))
  (testing "a clean -all record is silent"
    (is (empty? (detectives/detect-spf-problems {"domain" "infra.run" "spf" ["\"v=spf1 mx -all\""]})))))

(deftest dmarc-detective-test
  (is (= :warning (:severity (first (detectives/detect-dmarc-problems {"domain" "infra.run" "dmarc" []})))))
  (is (= :info (:severity (first (detectives/detect-dmarc-problems
                                  {"domain" "infra.run" "dmarc" ["\"v=DMARC1; p=none\""]})))))
  (is (empty? (detectives/detect-dmarc-problems {"domain" "infra.run" "dmarc" ["\"v=DMARC1; p=reject\""]}))))

(deftest dkim-detective-test
  (let [evidence {"domain" "infra.run"
                  "dkim" {"default" ["\"v=DKIM1; k=rsa; p=AAAA\""]
                          "mail" []}}]
    (is (= #{"DKIM selector has no key published"}
           (summaries (detectives/detect-dkim-problems evidence))))
    (is (= ["mail._domainkey.infra.run"]
           (map :component (detectives/detect-dkim-problems evidence))))))

(deftest reverse-dns-detective-test
  (testing "no PTR is critical"
    (is (= #{"203.0.113.5 has no PTR record"}
           (summaries (detectives/detect-reverse-dns
                       {"host" "mail.infra.run" "ptr" [["203.0.113.5" ""]]})))))
  (testing "a PTR pointing elsewhere breaks forward-confirmed rDNS"
    (is (contains? (summaries (detectives/detect-reverse-dns
                               {"host" "mail.infra.run"
                                "ptr" [["203.0.113.5" "other.example.org."]]}))
                   "203.0.113.5 resolves back to other.example.org, not mail.infra.run (no forward-confirmed rDNS)")))
  (testing "matching forward-confirmed rDNS is silent"
    (is (empty? (detectives/detect-reverse-dns
                 {"host" "mail.infra.run" "ptr" [["203.0.113.5" "mail.infra.run."]]})))))

;; ---------------------------------------------------------------------------
;; live service detectives

(deftest service-detective-test
  (let [now (java.time.Instant/parse "2026-07-04T00:00:00Z")]
    (testing "an unreachable port is critical"
      (is (= #{"service port is not reachable"}
             (summaries (detectives/detect-service-problems
                         {:now now "host" "mail.infra.run"
                          "probes" [{:service "smtp" :port 25 :tls :starttls :reachable false}]})))))
    (testing "a cleartext port without STARTTLS is critical"
      (is (= #{"port does not offer STARTTLS"}
             (summaries (detectives/detect-service-problems
                         {:now now "host" "mail.infra.run"
                          "probes" [{:service "imap" :port 143 :tls :starttls
                                     :reachable true :starttls-ok false}]})))))
    (testing "a broken implicit-TLS handshake is critical"
      (is (= #{"TLS handshake failed on the implicit-TLS port"}
             (summaries (detectives/detect-service-problems
                         {:now now "host" "mail.infra.run"
                          "probes" [{:service "imaps" :port 993 :tls :implicit
                                     :reachable true :tls-ok false}]})))))
    (testing "an expiring certificate warns, an expired one is critical"
      (is (contains? (summaries (detectives/detect-service-problems
                                 {:now now "host" "mail.infra.run"
                                  "probes" [{:service "imaps" :port 993 :tls :implicit
                                             :reachable true :tls-ok true
                                             :not-after "notAfter=Jul 10 00:00:00 2026 GMT"}]}))
                     "TLS certificate expires in 6 days"))
      (is (contains? (summaries (detectives/detect-service-problems
                                 {:now now "host" "mail.infra.run"
                                  "probes" [{:service "imaps" :port 993 :tls :implicit
                                             :reachable true :tls-ok true
                                             :not-after "notAfter=Jul 01 00:00:00 2026 GMT"}]}))
                     "TLS certificate expired 3 days ago")))
    (testing "a healthy service with a fresh certificate is silent"
      (is (empty? (detectives/detect-service-problems
                   {:now now "host" "mail.infra.run"
                    "probes" [{:service "imaps" :port 993 :tls :implicit
                               :reachable true :tls-ok true
                               :not-after "notAfter=Dec 31 00:00:00 2026 GMT"}]}))))))

(deftest service-catalogue-test
  (testing "every cleartext port declares a STARTTLS protocol and a greeting"
    (doseq [service mail/services]
      (is (string? (:greeting service)) (str (:name service) " has no greeting"))
      (when (= :starttls (:tls service))
        (is (keyword? (:starttls service))
            (str (:name service) " is STARTTLS but declares no protocol")))))
  (testing "the classic seven mail ports are all covered"
    (is (= #{25 587 465 110 995 143 993} (set (map :port mail/services))))))
