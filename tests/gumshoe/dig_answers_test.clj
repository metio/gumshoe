;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dig-answers-test
  "What a dig run answers through the older entry points: records on
   NOERROR, [] on NXDOMAIN, and nil for a server that did not reply or failed
   the query. The mail checks read three-state results, and read the older
   shapes as results too: a missing record only on [], nil as unanswered;
   the DNS checks read three-state results and judge a
   missing record only on :absent. Names from cluster objects never reach dig
   as options."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.dns :as dns-detectives]
            [gumshoe.detectives.mail :as mail-detectives]
            [gumshoe.dns :as dns]
            [gumshoe.mail :as mail]
            [gumshoe.reads.result :as result]
            [gumshoe.shell :as shell]))

(defn- answered
  "A DNS read that answered with these records; none is :absent."
  [& records]
  (if (seq records)
    (result/present {:source :dns} {:items (vec records) :status "NOERROR"})
    (result/absent {:source :dns} {:items [] :status "NOERROR"})))

(def ^:private unanswered (result/unread {:source :dns} "SERVFAIL" {:status "SERVFAIL"}))

(defn- response
  "dig's +noall +comments +answer output for a response with this status and
   these answer lines."
  [status & answers]
  (str ";; Got answer:\n"
       ";; ->>HEADER<<- opcode: QUERY, status: " status ", id: 4711\n"
       ";; flags: qr rd ra; QUERY: 1, ANSWER: " (count answers) ", AUTHORITY: 0, ADDITIONAL: 1\n"
       "\n;; OPT PSEUDOSECTION:\n; EDNS: version: 0, flags:; udp: 1232\n"
       (when (seq answers) (str "\n;; ANSWER SECTION:\n" (str/join "\n" answers) "\n"))))

(defn- answering [result]
  (fn [& _] result))

;; --- status ---------------------------------------------------------------------

(deftest a-noerror-response-answers-its-records
  (with-redefs [shell/execute (answering {:exit 0 :out (response "NOERROR"
                                                                 "example.org.\t300\tIN\tA\t203.0.113.1"
                                                                 "example.org.\t300\tIN\tA\t203.0.113.2")})]
    (is (= ["203.0.113.1" "203.0.113.2"] (dns/query {:server "ns"} "A" "example.org")))))

(deftest a-noerror-response-without-records-answers-empty
  (with-redefs [shell/execute (answering {:exit 0 :out (response "NOERROR")})]
    (is (= [] (dns/query {:server "ns"} "TXT" "example.org")))))

(deftest an-nxdomain-response-answers-empty
  (with-redefs [shell/execute (answering {:exit 0 :out (response "NXDOMAIN")})]
    (is (= [] (dns/query {:server "ns"} "TXT" "_dmarc.example.org")))))

(deftest a-failing-server-does-not-answer
  (doseq [status ["SERVFAIL" "REFUSED" "FORMERR" "NOTIMP" "NOTAUTH"]]
    (testing status
      (with-redefs [shell/execute (answering {:exit 0 :out (response status)})]
        (is (nil? (dns/query {:server "ns"} "NS" "example.org")))))))

(deftest a-run-without-a-response-header-does-not-answer
  (testing "dig exits 0 yet prints no header: nothing tells what the server said"
    (with-redefs [shell/execute (answering {:exit 0 :out ""})]
      (is (nil? (dns/query {:server "ns"} "A" "example.org")))))
  (testing "dig failed"
    (with-redefs [shell/execute (answering {:exit 9 :out ";; communications error to 192.0.2.1#53: timed out\n"})]
      (is (nil? (dns/query {:server "ns"} "A" "example.org"))))))

(deftest the-last-header-is-the-answer-kept
  (testing "a truncated UDP answer retried over TCP prints two headers"
    (is (= "NOERROR" (dns/response-status (str (response "SERVFAIL") ";; Truncated, retrying in TCP mode.\n"
                                               (response "NOERROR")))))))

(deftest record-data-reads-as-short-output-did
  (is (= ["10 mx.example.org."
          "\"v=spf1 include:_spf.example.net -all\""
          "\"v=DKIM1; k=rsa; \" \"p=MIIB\""
          "ns1.example.org. hostmaster.example.org. 2026070401 3600 900 1209600 300"
          "alias.example.net."]
         (dns/parse-answer (str (response "NOERROR"
                                          "example.org.\t300\tIN\tMX\t10 mx.example.org."
                                          "example.org.\t300\tIN\tTXT\t\"v=spf1 include:_spf.example.net -all\""
                                          "s._domainkey.example.org. 300 IN TXT \"v=DKIM1; k=rsa; \" \"p=MIIB\""
                                          "example.org.\t300\tIN\tSOA\tns1.example.org. hostmaster.example.org. 2026070401 3600 900 1209600 300"
                                          "www.example.org.\t300\tIN\tCNAME\talias.example.net."))))))

;; --- names from cluster data ----------------------------------------------------

(deftest an-option-like-name-never-reaches-dig
  (doseq [name ["-f/etc/passwd" "+tcp" "-x" "example.org -f/etc/passwd" "" "exa mple.org" "a..b" ".example.org"
                "example.org/../x" (apply str (repeat 255 "a")) nil]]
    (testing (pr-str name)
      (is (nil? (dns/dig-args {:server "ns"} "A" name)))
      (let [calls (atom 0)]
        (with-redefs [shell/execute (fn [& _] (swap! calls inc) {:exit 0 :out (response "NOERROR")})]
          (is (nil? (dns/query {:server "ns"} "A" name)) "no answer was obtained")
          (is (zero? @calls) "dig never ran"))))))

(deftest a-reverse-lookup-takes-only-an-address
  (is (= ["-x" "2001:db8::1"] (take-last 2 (dns/dig-args {:server "ns" :reverse? true} "PTR" "2001:db8::1"))))
  (doseq [address ["-f/etc/passwd" "mail.example.org" "+short" ""]]
    (testing address
      (is (nil? (dns/dig-args {:server "ns" :reverse? true} "PTR" address))))))

(deftest the-names-a-check-asks-about-pass
  (doseq [name ["example.org" "example.org." "_dmarc.example.org" "selector1._domainkey.example.org"
                "*.apps.example.org" "bookstore-probe.apps.example.org" "xn--bcher-kva.example" "a"]]
    (testing name
      (is (= ["-t" "A" "-q" name] (take-last 4 (dns/dig-args {} "A" name)))))))

;; --- the collected evidence -----------------------------------------------------

(deftest an-unanswered-ns-query-leaves-every-nameserver-read-unanswered
  (with-redefs [shell/execute (answering {:exit 9 :out ""})]
    (let [evidence (binding [*err* (java.io.StringWriter.)]
                     (dns/collect-evidence! {:server "192.0.2.53" :zone "example.org" :probes ["www.example.org"]}))]
      (is (result/unread? (get evidence "nameservers")))
      (is (= "dig exited 9" (:reason (get evidence "nameservers"))))
      (is (nil? (get evidence "addresses")))
      (is (nil? (get evidence "serials")))
      (is (nil? (get evidence "probes")))
      (testing "and the checks that ask the nameservers are unread; only the NS read is reported, as unread"
        (let [checks (detective/mark-unread dns-detectives/detectives evidence)]
          (is (= #{"dns-nameservers" "dns-replication" "dns-probes"}
                 (set (map :name (filter :unread checks)))))
          (is (= [["dns-zone" "could not read NS example.org at 192.0.2.53 (dig exited 9)"]]
                 (map (juxt :detective :summary)
                      (filter #(#{"dns-zone" "dns-nameservers" "dns-replication" "dns-probes"} (:detective %))
                              (detective/run-detectives checks evidence))))))))))

(deftest a-servfail-on-the-ns-query-is-no-zone-without-nameservers
  (with-redefs [shell/execute (answering {:exit 0 :out (response "SERVFAIL")})]
    (let [evidence (binding [*err* (java.io.StringWriter.)]
                     (dns/collect-evidence! {:server "192.0.2.53" :zone "example.org" :probes []}))]
      (is (result/unread? (get evidence "nameservers")))
      (is (empty? (dns-detectives/detect-nameserver-problems evidence)))
      (is (= [[:warning "could not read NS example.org at 192.0.2.53 (SERVFAIL)"]]
             (map (juxt :severity :summary) (dns-detectives/detect-zone-problems evidence)))))))

;; --- the nameserver check -------------------------------------------------------

(defn- nameserver-findings [addresses]
  (set (map (juxt :severity :component :summary)
            (dns-detectives/detect-nameserver-problems
             {"zone" "example.org" "nameservers" (apply answered (keys addresses)) "addresses" addresses}))))

(deftest an-unanswered-address-query-is-no-missing-record
  (is (= #{[:warning "ns1.example.org" "could not resolve the nameserver's A record and AAAA record"]}
         (nameserver-findings {"ns1.example.org" {:a unanswered :aaaa nil}
                               "ns2.example.org" {:a (answered "203.0.113.2") :aaaa (answered "2001:db8::2")}})))
  (is (= #{[:warning "ns1.example.org" "could not resolve the nameserver's AAAA record"]}
         (nameserver-findings {"ns1.example.org" {:a (answered "203.0.113.1") :aaaa unanswered}
                               "ns2.example.org" {:a (answered "203.0.113.2") :aaaa (answered "2001:db8::2")}})))
  (testing "a nameserver absent from the address map was never asked either"
    (is (= #{[:warning "ns1.example.org" "could not resolve the nameserver's A record and AAAA record"]}
           (set (map (juxt :severity :component :summary)
                     (dns-detectives/detect-nameserver-problems
                      {"zone" "example.org" "nameservers" (answered "ns1.example.org" "ns2.example.org")
                       "addresses" {"ns2.example.org" {:a (answered "203.0.113.2") :aaaa (answered "2001:db8::2")}}})))))))

(deftest an-answered-empty-address-query-is-a-missing-record
  (is (= #{[:critical "ns1.example.org" "nameserver has no A record"]
           [:warning "ns2.example.org" "nameserver has no AAAA record"]}
         (nameserver-findings {"ns1.example.org" {:a (answered) :aaaa (answered "2001:db8::1")}
                               "ns2.example.org" {:a (answered "203.0.113.2") :aaaa (answered)}}))))

;; --- the mail checks ------------------------------------------------------------

(defn- check [name] (first (filter #(= name (:name %)) mail-detectives/detectives)))

(defn- judged [name evidence]
  (let [checks (detective/mark-unread [(check name)] evidence)]
    {:unread? (boolean (:unread (first checks)))
     :summaries (mapv :summary (detective/run-detectives checks evidence))}))

(deftest an-unanswered-record-query-judges-nothing
  (doseq [[name record] [["mail-mx" "mx"] ["mail-spf" "spf"] ["mail-dmarc" "dmarc"] ["mail-dkim" "dkim"]]]
    (testing name
      (is (= {:unread? true :summaries []} (judged name {"domain" "example.org" record nil})))
      (is (empty? ((:detect (check name)) {"domain" "example.org" record nil}))))))

(deftest an-answered-empty-record-query-is-a-missing-record
  (is (= {:unread? false :summaries ["domain has no MX records"]} (judged "mail-mx" {"domain" "example.org" "mx" []})))
  (is (= {:unread? false :summaries ["domain has no SPF record"]} (judged "mail-spf" {"domain" "example.org" "spf" []})))
  (is (= {:unread? false :summaries ["domain has no DMARC record"]} (judged "mail-dmarc" {"domain" "example.org" "dmarc" []}))))

(deftest a-dkim-selector-whose-query-did-not-answer-is-no-missing-key
  (is (= [[:info "a._domainkey.example.org" "DKIM selector could not be queried - the DNS server did not answer"]
          [:warning "b._domainkey.example.org" "DKIM selector has no key published"]]
         (map (juxt :severity :component :summary)
              (mail-detectives/detect-dkim-problems
               {"domain" "example.org" "dkim" {"b" [] "a" nil "c" ["\"v=DKIM1; k=rsa; p=MIIB\""]}})))))

(deftest a-ptr-query-that-did-not-answer-is-no-missing-ptr
  (is (= [[:info "PTR record of 203.0.113.5 could not be queried - the DNS server did not answer"]
          [:critical "203.0.113.6 has no PTR record"]]
         (map (juxt :severity :summary)
              (mail-detectives/detect-reverse-dns
               {"host" "mail.example.org" "ptr" [["203.0.113.5" nil] ["203.0.113.6" ""]]})))))

(deftest ptr-evidence-keeps-an-unanswered-query-apart
  (testing "a failed address query leaves the whole read unanswered, so the check is unread"
    (with-redefs [dns/lookup (fn [_ _ _] unanswered)]
      (is (result/unread? (mail/ptr-evidence "ns" "mail.example.org")))
      (is (:unread? (judged "mail-rdns" {"host" "mail.example.org" "ptr" (mail/ptr-evidence "ns" "mail.example.org")})))))
  (testing "each address keeps its PTR result: unread, absent or present"
    (with-redefs [dns/lookup (fn [connection type name]
                               (if (= "A" type)
                                 (answered "203.0.113.5" "203.0.113.6" "203.0.113.7")
                                 (do (assert (:reverse? connection))
                                     (case name
                                       "203.0.113.5" unanswered
                                       "203.0.113.6" (answered)
                                       "203.0.113.7" (answered "mail.example.org.")))))]
      (let [r (mail/ptr-evidence "ns" "mail.example.org")]
        (is (result/present? r))
        (is (= {"203.0.113.5" :unread "203.0.113.6" :absent "203.0.113.7" :present}
               (update-vals (:addresses r) result/state-of)))
        (is (= [["203.0.113.5" unanswered]] (result/unread-in (:addresses r))))))))

(deftest a-resource-list-under-a-mail-key-is-no-domain
  ;; a cluster serving a CRD that resolves as "domain" hands the mail checks
  ;; a list document under that key
  (let [crd-list {:apiVersion "v1" :kind "List" :items [{:metadata {:name "a"}}]}]
    (doseq [{:keys [name detect]} mail-detectives/detectives]
      (testing name
        (is (empty? (detect {"domain" crd-list "host" crd-list "mx" nil "spf" nil "dmarc" nil
                             "dkim" nil "ptr" nil "probes" crd-list})))))))
