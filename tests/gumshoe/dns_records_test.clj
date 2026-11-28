;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dns-records-test
  "The parsed answer records of a DNS read, {:name :ttl :type :data} per
   answer line, beside the record data in :items. The fixtures under
   tests/fixtures/dns are recorded dig +noall +comments +answer runs, so the
   parser meets dig's real column layout: tab and space separated fields,
   CNAME chains, TXT records of several quoted strings, trailing dots, and
   the comment lines dig prints for a failed or timed out query. dig never
   runs: gumshoe.shell/execute is stubbed throughout."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.dns :as dns]
            [gumshoe.reads.result :as result]
            [gumshoe.shell :as shell]))

(defn- fixture [name] (slurp (str "tests/fixtures/dns/" name ".txt")))

(defn- answered-run [name] {:exit 0 :out (fixture name)})

(def ^:private ref-a {:source :dns :type "A" :name "www.microsoft.com" :server "1.1.1.1"})

(def ^:private dkim-key
  (str "\"k=rsa; t=s; p=MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAyn3fMCVpb7ryIRKOGXhXVGYmsWUitNlSckqGHOwNFZgFadplOrD+Qzf1XQkP7MH/VB/97DsAAJGtEXW1Uq71Hjnfr/DuBN/YfjF/gU70qEFb7q1sdIiNtjFL2TkOpoW+X/bhhPNheW/fYwyFb6ZHFM6LTgXyuimWRHTOUP3VjZzhNVda79nt+2WZYbS4l8HdMgWpT\""
       " "
       "\"NHjpVw5PtXESA9KBg/evSRk5fIaXIX5eRXW3baoV9yVzD8O29/IL/DiSk+yNvaO0EHL5c4yGuZJhGzvpiznb2IDVdemJK4Dqzdy5FTN/SGYZhAEr7MguG3Z314hMS2scgMsOMgB64uj/6+6UwIDAQAB\""))

;; --- the parser ----------------------------------------------------------------------

(deftest a-cname-chain-parses-into-one-record-per-answer-line
  (is (= [{:name "www.microsoft.com." :ttl 3594 :type "CNAME" :data "www.microsoft.com-c-3.edgekey.net."}
          {:name "www.microsoft.com-c-3.edgekey.net." :ttl 894 :type "CNAME" :data "e13678.dscb.akamaiedge.net."}
          {:name "e13678.dscb.akamaiedge.net." :ttl 14 :type "A" :data "23.209.214.100"}]
         (dns/parse-records (fixture "cname-chain-long")))))

(deftest a-txt-record-of-several-quoted-strings-keeps-its-data-whole
  (is (= [{:name "s1._domainkey.github.com." :ttl 3600 :type "CNAME" :data "s1.domainkey.u51742174.wl175.sendgrid.net."}
          {:name "s1.domainkey.u51742174.wl175.sendgrid.net." :ttl 1800 :type "TXT" :data dkim-key}]
         (dns/parse-records (fixture "cname-txt-multi-string")))))

(deftest a-txt-record-with-spaces-keeps-them
  (let [records (dns/parse-records (fixture "txt"))]
    (is (= 17 (count records)))
    (is (every? #(= {:name "google.com." :ttl 166 :type "TXT"} (dissoc % :data)) records))
    (is (some #{"\"v=spf1 include:_spf.google.com ~all\""} (map :data records)))))

(deftest names-and-data-keep-their-trailing-dots
  (is (= [{:name "example.org." :ttl 75804 :type "NS" :data "mitch.ns.cloudflare.com."}
          {:name "example.org." :ttl 75804 :type "NS" :data "katelyn.ns.cloudflare.com."}]
         (dns/parse-records (fixture "ns"))))
  (is (= [{:name "example.org." :ttl 180 :type "MX" :data "0 ."}]
         (dns/parse-records (fixture "mx-null")))))

(deftest responses-without-answer-lines-parse-into-no-records
  (doseq [name ["nxdomain" "servfail" "timeout"]]
    (testing name
      (is (= [] (dns/parse-records (fixture name)))))))

(deftest a-line-that-is-no-record-is-skipped
  (testing "a TTL that is not a number, too few fields, a type that is not a type"
    (is (= [] (dns/parse-records (str "dig: couldn't get address for 'ns.invalid': not found\n"
                                      "example.org. 300 IN\n"
                                      "example.org. 300 IN a-type 192.0.2.1\n")))))
  (testing "a lowercase type is read in dig's uppercase spelling"
    (is (= [{:name "example.org." :ttl 300 :type "A" :data "192.0.2.1"}]
           (dns/parse-records "example.org. 300 IN a 192.0.2.1\n")))))

(deftest the-items-are-the-data-of-the-records
  (doseq [name ["cname-chain-long" "cname-txt-multi-string" "txt" "ns" "mx-null" "nxdomain" "servfail"]]
    (testing name
      (let [out (fixture name)]
        (is (= (mapv :data (dns/parse-records out)) (dns/parse-answer out)))))))

;; --- the result ----------------------------------------------------------------------

(deftest an-answered-read-carries-the-records-beside-the-items
  (let [r (dns/classify ref-a (answered-run "cname-chain-long"))]
    (is (result/present? r))
    (is (= ["www.microsoft.com-c-3.edgekey.net." "e13678.dscb.akamaiedge.net." "23.209.214.100"]
           (result/items! r))
        ":items stays the record data, CNAME targets included")
    (is (= [3594 894 14] (map :ttl (dns/records! r))))
    (is (= ["CNAME" "CNAME" "A"] (map :type (dns/records! r))))
    (is (= "NOERROR" (:status r)))))

(deftest an-answer-without-records-carries-no-records
  (testing "NXDOMAIN"
    (let [r (dns/classify ref-a (answered-run "nxdomain"))]
      (is (result/absent? r))
      (is (= "NXDOMAIN" (:status r)))
      (is (= [] (result/items! r) (dns/records! r)))))
  (testing "NOERROR without records"
    (let [r (dns/classify ref-a {:exit 0 :out (str/replace (fixture "nxdomain") "NXDOMAIN" "NOERROR")})]
      (is (result/absent? r))
      (is (= "NOERROR" (:status r)))
      (is (= [] (dns/records! r))))))

(deftest servfail-and-a-timeout-stay-unread-and-distinguishable
  (let [servfail (dns/classify ref-a (answered-run "servfail"))
        timeout (dns/classify ref-a {:exit 9 :out (fixture "timeout")})]
    (is (result/unread? servfail))
    (is (result/unread? timeout))
    (is (= "SERVFAIL" (:reason servfail) (:status servfail)))
    (is (= "dig exited 9: no servers could be reached" (:reason timeout)))
    (is (not (contains? timeout :status)) "a query that timed out has no response status")
    (testing "neither carries records, and records! refuses both"
      (doseq [r [servfail timeout]]
        (is (not (contains? r :records)))
        (let [e (try (dns/records! r) nil (catch clojure.lang.ExceptionInfo e e))]
          (is (result/unread-exception? e)))))))

(deftest records-refuses-a-result-that-never-carried-records
  (is (thrown? IllegalArgumentException
               (dns/records! (result/present {:source :dns} {:items ["192.0.2.1"]}))))
  (testing "nil counts as a read that did not answer, as it does for items!"
    (is (result/unread-exception? (try (dns/records! nil) nil (catch clojure.lang.ExceptionInfo e e))))))

(deftest lookup-answers-with-records
  (with-redefs [shell/execute (fn [& _] (answered-run "cname-txt-multi-string"))]
    (let [r (dns/lookup {:server "1.1.1.1"} "TXT" "s1._domainkey.github.com")]
      (is (= [{:name "s1.domainkey.u51742174.wl175.sendgrid.net." :ttl 1800 :type "TXT" :data dkim-key}]
             (filter #(= "TXT" (:type %)) (dns/records! r)))))))

(deftest the-older-entry-points-answer-as-they-did
  (with-redefs [shell/execute (fn [& _] (answered-run "cname-chain-long"))]
    (is (= ["www.microsoft.com-c-3.edgekey.net." "e13678.dscb.akamaiedge.net." "23.209.214.100"]
           (dns/query {:server "1.1.1.1"} "A" "www.microsoft.com"))))
  (is (= [] (dns/answer (answered-run "nxdomain"))))
  (is (nil? (dns/answer (answered-run "servfail"))))
  (is (nil? (dns/answer {:exit 9 :out (fixture "timeout")}))))

;; --- negative controls ---------------------------------------------------------------

(defn- keeps-multi-string-txt?
  "The property the parser must keep: a TXT record of several quoted strings
   reads back as the whole of its data."
  [parse]
  (= dkim-key (:data (last (parse (fixture "cname-txt-multi-string"))))))

(deftest the-multi-string-property-fails-for-a-parser-that-keeps-only-the-fifth-field
  (is (keeps-multi-string-txt? dns/parse-records))
  (testing "positive control: splitting every whitespace run and taking the fifth field cuts the TXT at its first space"
    (is (not (keeps-multi-string-txt?
              (fn [out]
                (for [line (str/split-lines out)
                      :when (not (or (str/blank? line) (str/starts-with? line ";")))
                      :let [[n ttl _ t data] (str/split (str/trim line) #"\s+")]]
                  {:name n :ttl (parse-long ttl) :type t :data data})))))))

(defn- ttl-typed?
  "Whether every record of the CNAME chain carries its TTL as an integer."
  [parse]
  (every? (comp integer? :ttl) (parse (fixture "cname-chain-long"))))

(deftest the-ttl-property-fails-for-a-parser-that-keeps-the-ttl-as-text
  (is (ttl-typed? dns/parse-records))
  (testing "positive control"
    (is (not (ttl-typed? (fn [out] (map #(update % :ttl str) (dns/parse-records out))))))))
