;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dns-result-test
  "The dig classifier and the DNS reads as three-state results: every
   response status, a run without a header, a failed dig, names dig may not
   be asked about, the operator-facing lines, the converted DNS detective,
   and an unanswered DNS read holding a change as an unanswered kubectl read
   does. dig never runs: gumshoe.shell/execute is stubbed throughout."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.dns :as dns-detectives]
            [gumshoe.dns :as dns]
            [gumshoe.effect :as effect]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mutation :as mutation]
            [gumshoe.reads :as reads]
            [gumshoe.reads.result :as result]
            [gumshoe.shell :as shell]
            [gumshoe.verify :as verify]))

(defn- response
  "dig's +noall +comments +answer output for a response with this status and
   these answer lines."
  [status & answers]
  (str ";; Got answer:\n"
       ";; ->>HEADER<<- opcode: QUERY, status: " status ", id: 4711\n"
       ";; flags: qr rd ra; QUERY: 1, ANSWER: " (count answers) ", AUTHORITY: 0, ADDITIONAL: 1\n"
       (when (seq answers) (str "\n;; ANSWER SECTION:\n" (str/join "\n" answers) "\n"))))

(def ^:private txt-ref {:source :dns :type "TXT" :name "_dmarc.example.org" :server "10.0.0.1"})

(defn- run [status & answers] {:exit 0 :out (apply response status answers)})

(def ^:private failing-statuses ["SERVFAIL" "REFUSED" "FORMERR" "NOTIMP" "NOTAUTH" "YXDOMAIN"])

;; --- the classifier ------------------------------------------------------------------

(deftest noerror-with-records-is-present
  (let [r (dns/classify txt-ref (run "NOERROR" "_dmarc.example.org. 300 IN TXT \"v=DMARC1; p=reject\""))]
    (is (result/present? r))
    (is (= ["\"v=DMARC1; p=reject\""] (result/items! r)))
    (is (= "NOERROR" (:status r)))
    (is (= txt-ref (:ref r)))))

(deftest an-answer-without-records-is-absent-and-keeps-its-status
  (testing "NXDOMAIN: the name does not exist"
    (let [r (dns/classify txt-ref (run "NXDOMAIN"))]
      (is (result/absent? r))
      (is (= [] (result/items! r)))
      (is (= "NXDOMAIN" (:status r)))))
  (testing "NOERROR without records: the name exists without records of the type"
    (let [r (dns/classify txt-ref (run "NOERROR"))]
      (is (result/absent? r))
      (is (not (result/present? r)) "a gate asserting the record must not pass on an empty answer")
      (is (= "NOERROR" (:status r))))))

(deftest every-other-status-is-unread-with-the-status-as-reason
  (doseq [status failing-statuses]
    (testing status
      (let [r (dns/classify txt-ref (run status))]
        (is (result/unread? r))
        (is (not (result/absent? r)) "a server failing to answer is no name without records")
        (is (= status (:reason r)))
        (is (= status (:status r)))))))

(deftest a-run-without-a-header-or-with-a-failed-dig-is-unread
  (is (= (result/unread txt-ref "dig printed no response header") (dns/classify txt-ref {:exit 0 :out ""})))
  (is (= "dig exited 9: communications error to 10.0.0.1#53: timed out"
         (:reason (dns/classify txt-ref {:exit 9 :out ";; communications error to 10.0.0.1#53: timed out\n"}))))
  (is (= "dig exited 10" (:reason (dns/classify txt-ref {:exit 10 :out ""}))))
  (testing "a failed dig is unread even when it printed a NOERROR header"
    (is (result/unread? (dns/classify txt-ref {:exit 9 :out (response "NOERROR" "x. 1 IN A 192.0.2.1")})))))

(deftest a-failed-dig-cannot-write-terminal-escapes-into-the-reason
  (let [reason (:reason (dns/classify txt-ref {:exit 9 :out ";; error \u001b[31mred\u001b[0m\u0007 here\n"}))]
    (is (= "dig exited 9: error red here" reason))))

;; --- lookup --------------------------------------------------------------------------

(deftest lookup-reads-one-name-through-dig
  (let [argv (atom nil)]
    (with-redefs [shell/execute (fn [& args] (reset! argv (vec args)) (run "SERVFAIL"))]
      (let [r (dns/lookup {:server "10.0.0.1"} "TXT" "_dmarc.example.org")]
        (is (= (result/unread txt-ref "SERVFAIL" {:status "SERVFAIL"}) r))
        (is (= ["-t" "TXT" "-q" "_dmarc.example.org"] (take-last 4 @argv)))))))

(deftest a-name-dig-may-not-be-asked-about-is-unread-and-dig-never-runs
  (doseq [name ["-f/etc/passwd" "+tcp" "" "a..b" nil (apply str (repeat 255 "a"))]]
    (testing (pr-str name)
      (let [calls (atom 0)]
        (with-redefs [shell/execute (fn [& _] (swap! calls inc) (run "NOERROR"))]
          (let [r (dns/lookup {:server "ns"} "A" name)]
            (is (result/unread? r))
            (is (= "not a name dig may be asked about" (:reason r)))
            (is (zero? @calls)))))))
  (testing "a reverse lookup takes only an address"
    (with-redefs [shell/execute (fn [& _] (throw (AssertionError. "dig must not run")))]
      (is (= "not an address dig can reverse"
             (:reason (dns/lookup {:server "ns" :reverse? true} "PTR" "mail.example.org")))))))

(deftest the-older-entry-points-keep-their-shape
  (doseq [[raw expected] [[(run "NOERROR" "example.org. 300 IN A 192.0.2.1") ["192.0.2.1"]]
                          [(run "NOERROR") []]
                          [(run "NXDOMAIN") []]
                          [(run "SERVFAIL") nil]
                          [{:exit 0 :out ""} nil]
                          [{:exit 9 :out ""} nil]]]
    (testing (pr-str raw)
      (is (= expected (dns/answer raw)))
      (with-redefs [shell/execute (fn [& _] raw)]
        (is (= expected (dns/query {:server "ns"} "A" "example.org")))))))

;; --- what an operator reads ----------------------------------------------------------

(deftest a-dns-ref-reads-as-type-name-and-server
  (is (= "TXT _dmarc.example.org at 10.0.0.1" (result/describe txt-ref)))
  (is (= "A example.org at the system resolver" (result/describe (dns/ref-of {} "A" "example.org"))))
  (is (= "SOA example.org at ns1 over IPv6" (result/describe (dns/ref-of {:server "ns1" :transport :ipv6} "SOA" "example.org"))))
  (is (= "PTR of 192.0.2.1 at ns" (result/describe (dns/ref-of {:server "ns" :reverse? true} "PTR" "192.0.2.1"))))
  (testing "a refused name is quoted and stripped of control characters"
    (is (= "A \"-f/etc/passwd x\" at ns" (result/describe (dns/ref-of {:server "ns"} "A" "-f/etc/passwd\u001b[2J\nx"))))))

(deftest an-operator-line-reads-naturally-for-a-dns-read
  (let [r (dns/classify txt-ref (run "SERVFAIL"))]
    (is (= "could not read TXT _dmarc.example.org at 10.0.0.1 (SERVFAIL) - acting on a wrong answer could do harm, so this refuses rather than act on a read that did not answer"
           (result/unread-refusal r)))
    (is (= "could not read TXT _dmarc.example.org at 10.0.0.1 (SERVFAIL)" (result/unread-line-of r)))
    (is (= "TXT _dmarc.example.org at 10.0.0.1 does not exist"
           (result/refusal-unless-present (dns/classify txt-ref (run "NXDOMAIN")))))
    (testing "through the names gumshoe.reads forwards"
      (is (= (result/unread-refusal r) (reads/unread-refusal r))))
    (testing "and the kube line stays as it was"
      (is (= "could not read deployments web/api (timeout) - acting on a wrong answer could do harm, so this refuses rather than act on a read that did not answer"
             (reads/unread-refusal (result/unread {:type "deployments" :namespace "web" :name "api"} "timeout")))))))

;; --- an unanswered DNS read holds a change -------------------------------------------

(defn- dig-servfail
  "A shell that answers every dig with SERVFAIL and every other command with
   an empty kubectl list."
  [& argv]
  (if (= "dig" (first argv)) (run "SERVFAIL") {:exit 0 :out "{\"items\":[]}" :err ""}))

(defn- dmarc! [] (result/items! (dns/lookup {:server "10.0.0.1"} "TXT" "_dmarc.example.org")))

(deftest a-post-check-on-an-unanswered-dns-read-is-not-verified
  (with-redefs [shell/execute dig-servfail]
    (is (= "could not read TXT _dmarc.example.org at 10.0.0.1: SERVFAIL"
           (verify/attempt #(seq (dmarc!)))))
    (testing "and the same check on an answered empty read is false, not a message"
      (with-redefs [shell/execute (fn [& _] (run "NXDOMAIN"))]
        (is (false? (verify/attempt #(boolean (seq (dmarc!))))))))))

(deftest an-unanswered-dns-read-holds-a-change-before-the-confirmation
  (with-redefs [kubectl/current-context (constantly "ctx")
                kubectl/current-cluster (constantly "cluster")
                shell/execute dig-servfail]
    (let [base {:select {:mode :namespaced :label "CronJob"} :confirm {:action "x"}
                :effect (fn [_] (effect/plan (effect/note "go")))}]
      (testing "a :reads entry"
        (is (= {:unread "could not read TXT _dmarc.example.org at 10.0.0.1: SERVFAIL"}
               (mutation/prejudge (merge base {:reads {:dmarc (fn [_] (dmarc!))}}) "batch/nightly" {} nil))))
      (testing "a :derive"
        (is (= {:unread "could not read TXT _dmarc.example.org at 10.0.0.1: SERVFAIL"}
               (mutation/prejudge (merge base {:derive (fn [_] {:dmarc (dmarc!)})}) "batch/nightly" {} nil))))
      (testing "a precondition refuses with the read named"
        (let [refused (atom nil)]
          (with-redefs-fn {#'mutation/refuse! (fn [why] (reset! refused why) false)}
            #((mutation/strict-precondition (fn [] (seq (dmarc!))))))
          (is (= "could not read what the precondition rests on - could not read TXT _dmarc.example.org at 10.0.0.1: SERVFAIL"
                 @refused)))))))

(deftest a-catcher-of-either-key-sees-an-unread-from-any-source
  (with-redefs [shell/execute dig-servfail]
    (let [e (try (dmarc!) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (:gumshoe.kubectl/unread (ex-data e)) "a catcher written against the kubectl key holds for DNS")
      (is (:gumshoe.reads/unread (ex-data e)))))
  (testing "a strict kubectl read carries the neutral key too"
    (with-redefs [shell/execute (fn [& _] {:exit 1 :out "" :err "Unable to connect to the server"})]
      (let [e (try (binding [kubectl/*strict-reads* true] (kubectl/get-json "ctx" "pods" "a")) nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (some? e))
        (is (:gumshoe.reads/unread (ex-data e)))
        (is (:gumshoe.kubectl/unread (ex-data e))))))
  (testing "an exception carrying only the kubectl key, as a casebook throws it, still holds a post-check"
    (is (= "old" (verify/attempt #(throw (ex-info "old" {:gumshoe.kubectl/unread true}))))))
  (testing "and one carrying only the neutral key"
    (is (= "new" (verify/attempt #(throw (ex-info "new" {:gumshoe.reads/unread true})))))))

;; --- negative controls ---------------------------------------------------------------

(defn- servfail-is-unread?
  "The property the classifier must keep: a SERVFAIL is a read that did not
   answer, never an answer without records."
  [classify]
  (every? #(result/unread? (classify txt-ref (run %))) failing-statuses))

(deftest the-servfail-property-fails-for-a-classifier-that-maps-it-to-absent
  (is (servfail-is-unread? dns/classify))
  (testing "positive control: the older nil-punning answer, read as empty, maps SERVFAIL to absent"
    (is (not (servfail-is-unread?
              (fn [ref raw] (if (empty? (dns/answer raw))
                              (result/absent ref {:items []})
                              (result/present ref {:items (dns/answer raw)}))))))))

(defn- treats-unread-as-missing-record?
  "Whether the nameserver check reports a missing A record for a nameserver
   whose address query did not answer."
  [detect a-read]
  (contains? (set (map :summary (detect {"zone" "example.org"
                                         "nameservers" (result/present {:source :dns} {:items ["ns1" "ns2"]})
                                         "addresses" {"ns1" {:a a-read :aaaa (result/present {} {:items ["2001:db8::1"]})}
                                                      "ns2" {:a (result/present {} {:items ["192.0.2.2"]})
                                                             :aaaa (result/present {} {:items ["2001:db8::2"]})}}})))
             "nameserver has no A record"))

(deftest the-detective-never-reads-nil-or-unread-as-empty
  (doseq [a [nil (result/unread {} "SERVFAIL")]]
    (testing (pr-str a)
      (is (not (treats-unread-as-missing-record? dns-detectives/detect-nameserver-problems a)))))
  (is (treats-unread-as-missing-record? dns-detectives/detect-nameserver-problems (result/absent {} {:items []}))
      "an answer without records is a missing record")
  (testing "positive control: a check that reads (empty? (items-or-nil a)) as missing fails the property"
    (is (treats-unread-as-missing-record?
         (fn [evidence]
           (for [[ns {:keys [a]}] (get evidence "addresses")
                 :when (empty? (:items a))]
             {:component ns :summary "nameserver has no A record"}))
         (result/unread {} "SERVFAIL")))))

;; --- the converted detective, end to end ---------------------------------------------

(defn- collect
  "The evidence collect-evidence! gathers when every dig answers with what
   answer-of returns for its argv."
  [answer-of]
  (with-redefs [shell/execute (fn [& argv] (answer-of (vec argv)))]
    (binding [*out* (java.io.StringWriter.)]
      (dns/collect-evidence! {:server "192.0.2.53" :zone "example.org" :probes ["www.example.org"]}))))

(defn- asked [argv] [(last (filter #(str/starts-with? % "@") argv)) (nth argv (- (count argv) 3)) (last argv)])

(defn- judged
  [evidence]
  (let [checks (detective/mark-unread dns-detectives/detectives evidence)]
    {:unread (set (map :name (filter :unread checks)))
     :findings (set (map (juxt :detective :severity :summary) (detective/run-detectives checks evidence)))}))

(deftest an-unread-ns-query-reports-as-unread
  (let [evidence (collect (fn [argv] (if (= "NS" (second (asked argv))) (run "REFUSED") (run "NOERROR" "example.org. 1 IN SOA a. b. 1 2 3 4 5"))))]
    (is (result/unread? (get evidence "nameservers")))
    (is (= {:unread #{"dns-nameservers" "dns-replication" "dns-probes"}
            :findings #{["dns-zone" :warning "could not read NS example.org at 192.0.2.53 (REFUSED)"]}}
           (judged evidence)))))

(deftest an-nxdomain-ns-answer-is-a-zone-that-does-not-exist
  (let [evidence (collect (fn [argv] (if (= "NS" (second (asked argv))) (run "NXDOMAIN") (run "NXDOMAIN"))))]
    (is (= #{["dns-zone" :critical "zone does not exist on the entry server"]}
           (:findings (judged evidence))))
    (testing "and the transports answered, NXDOMAIN being an answer"
      (is (empty? (dns-detectives/detect-transport-problems evidence))))))

(deftest a-noerror-ns-answer-without-records-is-a-zone-without-nameservers
  (let [evidence (collect (fn [_] (run "NOERROR")))]
    (is (contains? (:findings (judged evidence)) ["dns-zone" :critical "zone has no NS records"]))))

(deftest a-healthy-zone-reports-nothing
  (let [evidence (collect (fn [argv]
                            (let [[_ type name] (asked argv)]
                              (case type
                                "NS" (run "NOERROR" "example.org. 1 IN NS ns1.example.org." "example.org. 1 IN NS ns2.example.org.")
                                "SOA" (run "NOERROR" "example.org. 1 IN SOA ns1. host. 2026070401 3600 900 1209600 300")
                                "A" (run "NOERROR" (str name ". 1 IN A 192.0.2.10"))
                                "AAAA" (run "NOERROR" (str name ". 1 IN AAAA 2001:db8::10"))))))]
    (is (= ["ns1.example.org" "ns2.example.org"] (result/items! (get evidence "nameservers"))))
    (is (= {:unread #{} :findings #{}} (judged evidence)))))
