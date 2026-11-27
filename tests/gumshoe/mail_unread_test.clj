;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mail-unread-test
  "The mail evidence holds a three-state result for every read. A read that
   did not answer is :unread, a read that was never made because its input
   was not given is :unread and not-asked, so the checks over either are
   unread instead of clean, a run that probed nothing fails instead of
   ending \"nothing critical\", and the report says which of the two it was.
   No test here reaches a real DNS server or port: dig and openssl are
   stubbed, and shell/execute throws."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gumshoe.config :as config]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.mail :as detectives]
            [gumshoe.dns :as dns]
            [gumshoe.evidence :as evidence]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.mail :as mail]
            [gumshoe.reads.result :as result]
            [gumshoe.runbook :as runbook]
            [gumshoe.shell :as shell]))

;; --- answers ---------------------------------------------------------------------

(defn- records
  "A lookup answer: records present, none absent."
  [connection type name & items]
  (let [ref (dns/ref-of connection type name)]
    (if (seq items)
      (result/present ref {:items (vec items) :status "NOERROR"})
      (result/absent ref {:items [] :status "NOERROR"}))))

(defn- servfail
  [connection type name]
  (result/unread (dns/ref-of connection type name) "SERVFAIL" {:status "SERVFAIL"}))

(defn- no-network
  [& args]
  (throw (ex-info "a test reached shell/execute" {:args args})))

(defn- collect
  "The evidence collect-evidence! gathers for opts against a DNS server
   answering every lookup with (answer connection type name) and ports that
   refuse every probe. Returns [evidence probed-hosts looked-up]."
  [opts answer]
  (let [probed (atom [])
        looked-up (atom [])]
    [(with-redefs [shell/execute no-network
                   dns/lookup (fn [connection type name]
                                (swap! looked-up conj [type name])
                                (answer connection type name))
                   shell/execute-with-stdin (fn [_ & args]
                                              (swap! probed conj (vec args))
                                              {:exit 1 :out "" :err "connect: Connection refused"})]
       (let [evidence (atom nil)]
         (binding [*err* (java.io.StringWriter.)]
           (with-out-str (reset! evidence (mail/collect-evidence! opts))))
         @evidence))
     @probed
     @looked-up]))

(defn- empty-answers [connection type name] (records connection type name))

(defn- marks
  "Each check's name to what mark-unread says about it."
  [evidence]
  (into {} (map (juxt :name #(select-keys % [:unread :not-asked :reasons])))
        (detective/mark-unread detectives/detectives evidence)))

(defn- unread-names
  [evidence]
  (set (keep (fn [[name m]] (when (:unread m) name)) (marks evidence))))

;; --- not asked against not answered -----------------------------------------------

(deftest without-a-host-the-services-and-rdns-reads-are-not-asked-test
  (let [[evidence probed looked-up] (collect {:domain "example.org" :dkim-selectors ["default"]} empty-answers)]
    (doseq [key ["probes" "ptr"]]
      (testing key
        (is (result/unread? (get evidence key)) "never a pass")
        (is (result/not-asked? (get evidence key)))
        (is (evidence/missing? (get evidence key)))
        (is (= "no host was given" (:reason (get evidence key))))))
    (is (empty? probed) "no port was probed on a host nobody named")
    (is (not-any? #(= "A" (first %)) looked-up) "no address was looked up")
    (is (= {:unread true :not-asked ["no host was given"]} (get (marks evidence) "mail-services")))
    (is (= {:unread true :not-asked ["no host was given"]} (get (marks evidence) "mail-rdns")))
    (testing "the domain checks still read"
      (is (not-any? (unread-names evidence) ["mail-mx" "mail-spf" "mail-dmarc" "mail-dkim"])))))

(deftest a-blank-host-is-no-host-test
  (let [[evidence probed] (collect {:domain "example.org" :host "  " :dkim-selectors []} empty-answers)]
    (is (result/not-asked? (get evidence "probes")))
    (is (empty? probed))))

(deftest without-a-domain-no-record-is-queried-test
  (let [[evidence _ looked-up] (collect {:dkim-selectors ["default"]} empty-answers)]
    (is (empty? looked-up))
    (doseq [key ["mx" "spf" "dmarc" "dkim"]]
      (is (result/not-asked? (get evidence key)) key)
      (is (= "no domain was given" (:reason (get evidence key))) key))
    (is (= (set (map :name detectives/detectives)) (unread-names evidence)))
    (is (every? (comp seq :not-asked) (vals (marks evidence))))))

(deftest a-server-that-does-not-answer-is-unread-and-asked-test
  (let [[evidence] (collect {:domain "example.org" :host "mail.example.org" :server "192.0.2.53"
                             :dkim-selectors ["default"]}
                            servfail)
        m (marks evidence)]
    (doseq [key ["mx" "spf" "dmarc" "dkim" "ptr"]]
      (testing key
        (is (result/unread? (get evidence key)))
        (is (not (result/not-asked? (get evidence key))))))
    (doseq [name ["mail-mx" "mail-spf" "mail-dmarc" "mail-dkim" "mail-rdns"]]
      (testing name
        (is (:unread (get m name)))
        (is (nil? (:not-asked (get m name))) "a failed read is no missing input")))
    (is (= ["could not read MX example.org at 192.0.2.53 (SERVFAIL)"] (:reasons (get m "mail-mx"))))
    (is (= ["could not read A mail.example.org at 192.0.2.53 (SERVFAIL)"] (:reasons (get m "mail-rdns"))))
    (is (= ["could not read the DKIM keys of example.org (selector default) at 192.0.2.53 (no selector answered: SERVFAIL)"]
           (:reasons (get m "mail-dkim"))))
    (testing "the probes answered, so the services check reads"
      (is (not (:unread (get m "mail-services")))))))

(deftest each-record-read-keeps-present-and-absent-apart-test
  (let [[evidence] (collect {:domain "example.org" :dkim-selectors []}
                            (fn [connection type name]
                              (case [type name]
                                ["MX" "example.org"] (records connection type name "10 mx.example.org.")
                                ["TXT" "example.org"] (records connection type name)
                                ["TXT" "_dmarc.example.org"] (servfail connection type name))))]
    (is (result/present? (get evidence "mx")))
    (is (result/absent? (get evidence "spf")))
    (is (result/unread? (get evidence "dmarc")))
    (is (= #{"domain has a single MX record - no fallback receiver" "domain has no SPF record"}
           (set (map :summary (mapcat #((:detect %) evidence)
                                      (filter #(#{"mail-mx" "mail-spf" "mail-dmarc"} (:name %))
                                              detectives/detectives))))))))

;; --- DKIM ---------------------------------------------------------------------------

(deftest dkim-evidence-test
  (let [txt (fn [selector] (dns/ref-of {} "TXT" (str selector "._domainkey.example.org")))
        key (result/present (txt "a") {:items ["\"v=DKIM1; k=rsa; p=AAAA\""]})
        none (result/absent (txt "b") {:items []})
        failed (result/unread (txt "c") "SERVFAIL")]
    (testing "no selector to ask about is a read never made"
      (is (result/not-asked? (mail/dkim-evidence {}))))
    (testing "no selector answered: the whole is unread, never not-asked"
      (let [r (mail/dkim-evidence {"c" failed})]
        (is (result/unread? r))
        (is (not (result/not-asked? r)))
        (is (= "no selector answered: SERVFAIL" (:reason r)))))
    (testing "some answered: present, keeping the others as unread"
      (let [r (mail/dkim-evidence {"a" key "b" none "c" failed})]
        (is (result/present? r))
        (is (= [["c" failed]] (result/unread-in (:selectors r))))))))

(deftest dkim-partial-answers-are-judged-per-selector-test
  (let [[evidence] (collect {:domain "example.org" :dkim-selectors ["default" "mail" "old"]}
                            (fn [connection type name]
                              (case name
                                "default._domainkey.example.org" (records connection type name "\"v=DKIM1; k=rsa; p=AAAA\"")
                                "mail._domainkey.example.org" (servfail connection type name)
                                "old._domainkey.example.org" (records connection type name)
                                (records connection type name))))]
    (is (not (contains? (unread-names evidence) "mail-dkim")))
    (is (= [[:info "mail._domainkey.example.org"] [:warning "old._domainkey.example.org"]]
           (map (juxt :severity :component) (detectives/detect-dkim-problems evidence))))))

;; --- reverse DNS ------------------------------------------------------------------------

(defn- rdns
  [ptr-answers]
  (let [[evidence] (collect {:host "mail.example.org"}
                            (fn [connection type name]
                              (if (= "A" type)
                                (apply records connection type name (keys ptr-answers))
                                ((get ptr-answers name) connection type name))))]
    evidence))

(deftest an-unread-ptr-is-never-reported-as-no-ptr-record-test
  (let [evidence (rdns (sorted-map "203.0.113.5" servfail
                                   "203.0.113.6" records
                                   "203.0.113.7" #(records %1 %2 %3 "mail.example.org.")))
        findings (detectives/detect-reverse-dns evidence)]
    (is (= [[:info "PTR record of 203.0.113.5 could not be queried - the DNS server did not answer"]
            [:critical "203.0.113.6 has no PTR record"]]
           (map (juxt :severity :summary) findings)))
    (is (not-any? #(str/includes? (:summary %) "203.0.113.5 has no PTR") findings))))

(deftest every-ptr-unread-leaves-the-check-unread-test
  (let [evidence (rdns {"203.0.113.5" servfail "203.0.113.6" servfail})]
    (is (result/unread? (get evidence "ptr")))
    (is (not (result/not-asked? (get evidence "ptr"))))
    (is (contains? (unread-names evidence) "mail-rdns"))
    (is (empty? (detective/run-detectives (detective/mark-unread detectives/detectives evidence)
                                          (select-keys evidence ["host" "ptr"]))))))

(deftest a-host-without-an-address-is-named-test
  (let [evidence (rdns {})]
    (is (result/absent? (get evidence "ptr")))
    (is (= ["mail host resolves to no IPv4 address, so it has no reverse DNS to check"]
           (map :summary (detectives/detect-reverse-dns evidence))))))

(defn- false-no-ptr?
  "Whether the reverse-DNS check reports an address whose PTR query did not
   answer as having no PTR record."
  [evidence]
  (some #(= "203.0.113.5 has no PTR record" (:summary %)) (detectives/detect-reverse-dns evidence)))

(deftest the-ptr-order-hazard-has-a-negative-control-test
  ;; The older per-address value put nil (not answered) and "" (no record)
  ;; through two predicates that overlap: (str/blank? (str nil)) is true.
  ;; The control swaps in a reading of that value that tests blank before
  ;; nil, the reordering a review would not catch, and shows false-no-ptr?
  ;; catches it; without the swap the same evidence is reported unanswered.
  (let [legacy {"host" "mail.example.org" "ptr" [["203.0.113.5" nil]]}
        results (rdns {"203.0.113.5" servfail "203.0.113.6" records})
        blank-first (fn [address ptr]
                      (let [ref {:source :dns :type "PTR" :name address :reverse? true}]
                        (cond
                          (str/blank? (str ptr)) (result/absent ref {:items []})
                          (nil? ptr) (result/unread ref "the read did not answer")
                          :else (result/present ref {:items [ptr]}))))]
    (testing "an unanswered PTR is not reported as missing, in either shape"
      (is (not (false-no-ptr? legacy)))
      (is (not (false-no-ptr? results))))
    (testing "the control: the reordered reading makes the false critical, and the check sees it"
      (with-redefs-fn {#'detectives/ptr-result blank-first}
        #(is (false-no-ptr? legacy))))))

;; --- the book's report ---------------------------------------------------------------

(defn- run-book
  "Runs the mail book's action with opts against a DNS server answering with
   answer and refused ports, the report in output. Returns [result report]."
  ([opts] (run-book opts (fn [connection type name] (servfail connection type name)) "edn"))
  ([opts answer output]
   (let [{:keys [action]} (with-redefs [kubectl/current-cluster (constantly nil)
                                        config/env-value (fn [& _] nil)]
                            (binding [runbook/*loading* true]
                              (load-file "runbooks/detectives/mail.clj")))
         result (atom nil)
         out (with-redefs [detective/interactive? (constantly false)
                           kubectl/current-cluster (constantly nil)
                           kubectl/current-context (constantly "ctx")
                           config/env-value (fn [& _] nil)
                           shell/execute no-network
                           dns/lookup answer
                           shell/execute-with-stdin (fn [& _] {:exit 1 :out "" :err "refused"})]
               (binding [*err* (java.io.StringWriter.)]
                 (with-out-str (reset! result (action (merge {:dkim-selectors ["default" "mail"] :output output} opts)
                                                      nil)))))]
     [@result (if (= "edn" output) (edn/read-string out) out)])))

(deftest a-run-that-probed-nothing-fails-test
  (testing "a domain whose DNS server never answers, and no host"
    (let [[result report] (run-book {:domain "example.org"})
          checked (into {} (map (juxt :name identity)) (:checked report))]
      (is (false? result) "the run must not end nothing critical")
      (is (true? (:nothing-checked report)))
      (is (every? :unread (:checked report)) (pr-str (remove :unread (:checked report))))
      (testing "checked tells the failed reads from the inputs not given"
        (is (= ["no host was given"] (:not-asked (checked "mail-rdns"))))
        (is (nil? (:not-asked (checked "mail-mx"))))
        (is (= ["could not read MX example.org at the system resolver (SERVFAIL)"] (:reasons (checked "mail-mx")))))))
  (testing "neither domain nor host"
    (let [[result report] (run-book {})]
      (is (false? result))
      (is (true? (:nothing-checked report)))
      (is (= #{["no domain was given"] ["no host was given"]} (set (map :not-asked (:checked report))))))))

(deftest the-text-report-names-what-was-not-given-test
  (let [[result out] (run-book {:domain "example.org"}
                               (fn [connection type name]
                                 (case type
                                   "MX" (records connection type name "10 mx.example.org." "20 mx2.example.org.")
                                   "TXT" (case name
                                           "example.org" (records connection type name "\"v=spf1 mx -all\"")
                                           "_dmarc.example.org" (records connection type name "\"v=DMARC1; p=reject\"")
                                           (records connection type name "\"v=DKIM1; k=rsa; p=AAAA\""))))
                               "text")]
    (is (true? result))
    (is (str/includes? out "2 checks judged nothing because an input was not given - mail-rdns, mail-services (no host was given)") out)
    (is (not (str/includes? out "read nothing on this cluster")) out)))
