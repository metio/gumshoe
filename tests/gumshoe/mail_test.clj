;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mail-test
  (:require [clojure.test :refer [deftest is testing]]
            [gumshoe.detectives.mail :as detectives]
            [gumshoe.mail :as mail]
            [gumshoe.shell :as shell]))

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
    (is (nil? (mail/spf-all-qualifier "v=spf1 include:_spf.example.org"))))
  (testing "'all' inside an earlier mechanism is not mistaken for the terminating one"
    (is (= "-" (mail/spf-all-qualifier "v=spf1 include:_spf.firewall.net -all")))
    (is (= "-" (mail/spf-all-qualifier "v=spf1 a:mail.install.example.com -all")))
    (is (= "~" (mail/spf-all-qualifier "v=spf1 include:allowlist.example.org ~all")))))

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
         (summaries (detectives/detect-mx-problems {"domain" "example.org" "mx" []}))))
  (is (= #{"domain has a single MX record - no fallback receiver"}
         (summaries (detectives/detect-mx-problems {"domain" "example.org" "mx" ["10 mail.example.org."]}))))
  (is (empty? (detectives/detect-mx-problems {"domain" "example.org"
                                              "mx" ["10 mail.example.org." "20 mail2.example.org."]}))))

(deftest spf-detective-test
  (testing "missing SPF is critical"
    (is (= #{"domain has no SPF record"}
           (summaries (detectives/detect-spf-problems {"domain" "example.org" "spf" []})))))
  (testing "multiple SPF records are a critical permerror"
    (is (= :critical
           (:severity (first (detectives/detect-spf-problems
                              {"domain" "example.org"
                               "spf" ["\"v=spf1 -all\"" "\"v=spf1 mx ~all\""]}))))))
  (testing "+all authorizes the whole internet - critical"
    (is (contains? (summaries (detectives/detect-spf-problems
                               {"domain" "example.org" "spf" ["\"v=spf1 +all\""]}))
                   "SPF ends in +all - it authorizes the entire internet to send as this domain")))
  (testing "a clean -all record is silent"
    (is (empty? (detectives/detect-spf-problems {"domain" "example.org" "spf" ["\"v=spf1 mx -all\""]})))))

(deftest dmarc-detective-test
  (is (= :warning (:severity (first (detectives/detect-dmarc-problems {"domain" "example.org" "dmarc" []})))))
  (is (= :info (:severity (first (detectives/detect-dmarc-problems
                                  {"domain" "example.org" "dmarc" ["\"v=DMARC1; p=none\""]})))))
  (is (empty? (detectives/detect-dmarc-problems {"domain" "example.org" "dmarc" ["\"v=DMARC1; p=reject\""]}))))

(deftest dkim-detective-test
  (let [evidence {"domain" "example.org"
                  "dkim" {"default" ["\"v=DKIM1; k=rsa; p=AAAA\""]
                          "mail" []}}]
    (is (= #{"DKIM selector has no key published"}
           (summaries (detectives/detect-dkim-problems evidence))))
    (is (= ["mail._domainkey.example.org"]
           (map :component (detectives/detect-dkim-problems evidence))))))

(deftest reverse-dns-detective-test
  (testing "no PTR is critical"
    (is (= #{"203.0.113.5 has no PTR record"}
           (summaries (detectives/detect-reverse-dns
                       {"host" "mail.example.org" "ptr" [["203.0.113.5" ""]]})))))
  (testing "a PTR pointing elsewhere breaks forward-confirmed rDNS"
    (is (contains? (summaries (detectives/detect-reverse-dns
                               {"host" "mail.example.org"
                                "ptr" [["203.0.113.5" "other.example.org."]]}))
                   "203.0.113.5 resolves back to other.example.org, not mail.example.org (no forward-confirmed rDNS)")))
  (testing "matching forward-confirmed rDNS is silent"
    (is (empty? (detectives/detect-reverse-dns
                 {"host" "mail.example.org" "ptr" [["203.0.113.5" "mail.example.org."]]}))))
  (testing "a case-only difference is still forward-confirmed (DNS names are case-insensitive)"
    (is (empty? (detectives/detect-reverse-dns
                 {"host" "mail.example.org" "ptr" [["203.0.113.5" "Mail.Example.ORG."]]})))))

;; ---------------------------------------------------------------------------
;; live service detectives

(deftest service-detective-test
  (let [now (java.time.Instant/parse "2026-07-04T00:00:00Z")]
    (testing "an unreachable port is critical"
      (is (= #{"service port is not reachable"}
             (summaries (detectives/detect-service-problems
                         {:now now "host" "mail.example.org"
                          "probes" [{:service "smtp" :port 25 :tls :starttls :reachable false}]})))))
    (testing "a cleartext port without STARTTLS is critical"
      (is (= #{"port does not offer STARTTLS"}
             (summaries (detectives/detect-service-problems
                         {:now now "host" "mail.example.org"
                          "probes" [{:service "imap" :port 143 :tls :starttls
                                     :reachable true :starttls-ok false}]})))))
    (testing "a broken implicit-TLS handshake is critical"
      (is (= #{"TLS handshake failed on the implicit-TLS port"}
             (summaries (detectives/detect-service-problems
                         {:now now "host" "mail.example.org"
                          "probes" [{:service "imaps" :port 993 :tls :implicit
                                     :reachable true :tls-ok false}]})))))
    (testing "an expiring certificate warns, an expired one is critical"
      (is (contains? (summaries (detectives/detect-service-problems
                                 {:now now "host" "mail.example.org"
                                  "probes" [{:service "imaps" :port 993 :tls :implicit
                                             :reachable true :tls-ok true
                                             :not-after "notAfter=Jul 10 00:00:00 2026 GMT"}]}))
                     "TLS certificate expires in 6 days"))
      (is (contains? (summaries (detectives/detect-service-problems
                                 {:now now "host" "mail.example.org"
                                  "probes" [{:service "imaps" :port 993 :tls :implicit
                                             :reachable true :tls-ok true
                                             :not-after "notAfter=Jul 01 00:00:00 2026 GMT"}]}))
                     "TLS certificate expired 3 days ago")))
    (testing "a healthy service with a fresh certificate is silent"
      (is (empty? (detectives/detect-service-problems
                   {:now now "host" "mail.example.org"
                    "probes" [{:service "imaps" :port 993 :tls :implicit
                               :reachable true :tls-ok true
                               :not-after "notAfter=Dec 31 00:00:00 2026 GMT"}]}))))))

(def ^:private sample-pem
  (str "-----BEGIN CERTIFICATE-----\nMIIBmock\n-----END CERTIFICATE-----\n"))

(deftest probe-service-test
  (testing "a STARTTLS port that presents a certificate is reachable AND starttls-ok"
    ;; The certificate is the success signal for the handshake; the enddate is
    ;; read from that same PEM. Guards against keying success off a marker (e.g.
    ;; 'CONNECTION ESTABLISHED') that openssl only prints in another mode.
    (with-redefs [shell/execute-with-stdin
                  (fn [_ & args]
                    (if (= "s_client" (second args))
                      {:exit 0 :out sample-pem :err "depth=0 CN=mail\n"}
                      {:exit 0 :out "notAfter=Dec 31 00:00:00 2026 GMT\n" :err ""}))]
      (let [probe (mail/probe-service "mail.example.org" (first (filter #(= "smtp" (:name %)) mail/services)))]
        (is (true? (:reachable probe)))
        (is (true? (:starttls-ok probe)))
        (is (= "notAfter=Dec 31 00:00:00 2026 GMT" (:not-after probe))))))
  (testing "an implicit-TLS port that presents a certificate is reachable AND tls-ok"
    (with-redefs [shell/execute-with-stdin
                  (fn [_ & args]
                    (if (= "s_client" (second args))
                      {:exit 0 :out sample-pem :err ""}
                      {:exit 0 :out "notAfter=Dec 31 00:00:00 2026 GMT\n" :err ""}))]
      (let [probe (mail/probe-service "mail.example.org" (first (filter #(= "smtps" (:name %)) mail/services)))]
        (is (true? (:tls-ok probe))))))
  (testing "a reachable port whose TLS negotiation fails is reachable but not ok"
    (with-redefs [shell/execute-with-stdin
                  (fn [_ & _] {:exit 1 :out "" :err "wrong version number\n"})]
      (let [probe (mail/probe-service "mail.example.org" (first (filter #(= "smtp" (:name %)) mail/services)))]
        (is (true? (:reachable probe)))
        (is (false? (:starttls-ok probe)))
        (is (nil? (:not-after probe))))))
  (testing "a refused/timed-out port is not reachable"
    (with-redefs [shell/execute-with-stdin
                  (fn [_ & _] {:exit 124 :out "" :err "'openssl ...' did not finish within 5000ms and was stopped"})]
      (let [probe (mail/probe-service "mail.example.org" (first (filter #(= "smtp" (:name %)) mail/services)))]
        (is (false? (:reachable probe)))))))

(deftest service-catalogue-test
  (testing "every cleartext port declares a STARTTLS protocol and a greeting"
    (doseq [service mail/services]
      (is (string? (:greeting service)) (str (:name service) " has no greeting"))
      (when (= :starttls (:tls service))
        (is (keyword? (:starttls service))
            (str (:name service) " is STARTTLS but declares no protocol")))))
  (testing "the classic seven mail ports are all covered"
    (is (= #{25 587 465 110 995 143 993} (set (map :port mail/services))))))
