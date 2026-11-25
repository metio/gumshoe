;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.mail
  "Detectives for mail infrastructure, judging the evidence collected by
   gumshoe.mail. Two families: deliverability (MX/SPF/DMARC/DKIM/rDNS records
   that decide whether mail lands in the inbox) and the live services
   (reachable ports, STARTTLS on cleartext ports, valid TLS certificates)."
  (:require [clojure.string :as str]
            [gumshoe.evidence :as evidence]
            [gumshoe.mail :as mail]))

(def ^:private certificate-warning-days 14)

;; ---------------------------------------------------------------------------
;; deliverability

(defn detect-mx-problems
  "Nothing is judged when the MX query did not answer: dig's nil is a server
   that did not reply, and its [] a server that answered with no records."
  [evidence]
  (let [domain (get evidence "domain")
        records (keep mail/parse-mx (get evidence "mx"))]
    (cond
      (not (evidence/joined? evidence ["mx"])) []

      (empty? records)
      [{:severity :critical
        :component domain
        :summary "domain has no MX records"
        :hint "no mail server can receive mail for this domain"}]

      :else
      (concat
       (when (= 1 (count records))
         [{:severity :info
           :component domain
           :summary "domain has a single MX record - no fallback receiver"}])
       (for [{:keys [host preference]} records
             :when (str/blank? (str host))]
         {:severity :critical
          :component domain
          :summary (format "MX with preference %s has no hostname" preference)})))))

(defn detect-spf-problems
  "Nothing is judged when the TXT query did not answer."
  [evidence]
  (let [domain (get evidence "domain")
        records (mail/spf-records (get evidence "spf"))]
    (cond
      (not (evidence/joined? evidence ["spf"])) []

      (empty? records)
      [{:severity :critical
        :component domain
        :summary "domain has no SPF record"
        :hint "receivers can not tell which servers may send for this domain - mail gets flagged as spam"}]

      (> (count records) 1)
      [{:severity :critical
        :component domain
        :summary (format "domain has %d SPF records - RFC 7208 allows exactly one" (count records))
        :hint "multiple SPF records make SPF permerror, which fails authentication outright"}]

      :else
      (let [qualifier (mail/spf-all-qualifier (first records))]
        (cond
          (nil? qualifier)
          [{:severity :warning
            :component domain
            :summary "SPF record has no 'all' mechanism"
            :hint "add ~all or -all so receivers know how to treat unlisted senders"}]

          (= "+" qualifier)
          [{:severity :critical
            :component domain
            :summary "SPF ends in +all - it authorizes the entire internet to send as this domain"
            :hint "change +all to -all (reject) or ~all (soft fail)"}]

          (= "?" qualifier)
          [{:severity :warning
            :component domain
            :summary "SPF ends in ?all (neutral) - it asserts nothing about senders"
            :hint "tighten to ~all or -all"}]

          :else [])))))

(defn detect-dmarc-problems
  "Nothing is judged when the TXT query did not answer."
  [evidence]
  (let [domain (get evidence "domain")
        policy (mail/dmarc-policy (get evidence "dmarc"))]
    (cond
      (not (evidence/joined? evidence ["dmarc"])) []

      (nil? policy)
      [{:severity :warning
        :component domain
        :summary "domain has no DMARC record"
        :hint "without DMARC, SPF and DKIM are advisory - publish at least p=none to start collecting reports"}]

      (= "none" policy)
      [{:severity :info
        :component domain
        :summary "DMARC policy is p=none - monitoring only, nothing is enforced"
        :hint "move to p=quarantine or p=reject once reports look clean"}]

      :else [])))

(defn detect-dkim-problems
  "A selector whose query answered without a key is a missing key; one whose
   query did not answer is reported as unanswered, never as missing."
  [evidence]
  (for [[selector txt] (sort-by key (get evidence "dkim"))
        :let [records (filter #(str/includes? % "v=DKIM1") (map mail/txt-record-value txt))
              component (format "%s._domainkey.%s" selector (get evidence "domain"))]
        :when (empty? records)]
    (if (nil? txt)
      {:severity :info
       :component component
       :summary "DKIM selector could not be queried - the DNS server did not answer"
       :hint "run the check again; a server that keeps failing to answer is a DNS problem of its own"}
      {:severity :warning
       :component component
       :summary "DKIM selector has no key published"
       :hint "mail signed with this selector fails DKIM - publish the key or stop signing with it"})))

(defn detect-reverse-dns
  "Each address of the host against its PTR name. An address whose PTR query
   did not answer (nil) is reported as unanswered; one answered without a
   record (\"\") has no PTR."
  [evidence]
  (let [host (get evidence "host")]
    (for [[address ptr] (get evidence "ptr")
          :let [ptr-host (some-> ptr str str/trim (str/replace #"\.$" ""))]
          finding [(cond
                     (nil? ptr)
                     {:severity :info
                      :component host
                      :summary (format "PTR record of %s could not be queried - the DNS server did not answer" address)
                      :hint "run the check again; a server that keeps failing to answer is a DNS problem of its own"}

                     (str/blank? (str ptr))
                     {:severity :critical
                      :component host
                      :summary (format "%s has no PTR record" address)
                      :hint "many receivers reject mail from IPs without reverse DNS outright"}

                     ;; DNS names are case-insensitive (RFC 4343), so a PTR that
                     ;; differs only in letter-case is still a forward-confirmed match.
                     (not= (str/lower-case (str host)) (str/lower-case (str ptr-host)))
                     {:severity :warning
                      :component host
                      :summary (format "%s resolves back to %s, not %s (no forward-confirmed rDNS)" address ptr-host host)
                      :hint "FCrDNS mismatches lower sender reputation with strict receivers"}

                     :else nil)]
          :when finding]
      finding)))

;; ---------------------------------------------------------------------------
;; live services

(defn detect-service-problems
  [evidence]
  (let [host (get evidence "host")
        now (:now evidence)]
    (apply concat
           (for [probe (get evidence "probes")]
             (let [label (format "%s:%d (%s)" host (:port probe) (:service probe))]
               (cond
                 (not (:reachable probe))
                 [{:severity :critical
                   :component label
                   :summary "service port is not reachable"
                   :hint "the daemon is down, or a firewall blocks the port"}]

                 (and (= :starttls (:tls probe)) (not (:starttls-ok probe)))
                 [{:severity :critical
                   :component label
                   :summary "port does not offer STARTTLS"
                   :hint "clients would send credentials and mail in cleartext - fix the TLS config"}]

                 (and (= :implicit (:tls probe)) (not (:tls-ok probe)))
                 [{:severity :critical
                   :component label
                   :summary "TLS handshake failed on the implicit-TLS port"
                   :hint "the certificate or TLS config is broken - clients can not connect securely"}]

                 :else
                 (let [days (mail/certificate-days-left now (:not-after probe))]
                   (cond
                     (nil? days) []
                     (neg? days) [{:severity :critical
                                   :component label
                                   :summary (format "TLS certificate expired %d days ago" (- days))
                                   :hint "clients reject the expired certificate - renew immediately"}]
                     (< days certificate-warning-days)
                     [{:severity :warning
                       :component label
                       :summary (format "TLS certificate expires in %d days" days)
                       :hint "renew before it lapses"}]
                     :else []))))))))

(defn- about
  "Runs a detective only when the evidence names its subject as a string. The
   mail evidence comes from the mail book; any other scan collects none, and
   judging the records of no domain would report every record as missing. A
   resource list read under the same key is no domain either."
  [subject detect]
  (fn [evidence]
    (when (string? (get evidence subject))
      (detect evidence))))

(def detectives
  [{:name "mail-mx"
    :description "MX records: mail can be received at all"
    :requires ["mx" "domain"]
    :joins ["mx"]
    :detect (about "domain" detect-mx-problems)}
   {:name "mail-spf"
    :description "SPF record present, unique, and not dangerously permissive"
    :requires ["spf" "domain"]
    :joins ["spf"]
    :detect (about "domain" detect-spf-problems)}
   {:name "mail-dmarc"
    :description "DMARC policy published and enforcing"
    :requires ["dmarc" "domain"]
    :joins ["dmarc"]
    :detect (about "domain" detect-dmarc-problems)}
   {:name "mail-dkim"
    :description "DKIM keys published for the configured selectors"
    :requires ["dkim" "domain"]
    :joins ["dkim"]
    :detect (about "domain" detect-dkim-problems)}
   {:name "mail-rdns"
    :description "Forward-confirmed reverse DNS for the mail host"
    :requires ["ptr" "host"]
    :joins ["ptr"]
    :detect (about "host" detect-reverse-dns)}
   {:name "mail-services"
    :description "SMTP/POP3/IMAP ports reachable, STARTTLS offered, certificates valid"
    :requires ["probes" "host"]
    :detect (about "host" detect-service-problems)}])
