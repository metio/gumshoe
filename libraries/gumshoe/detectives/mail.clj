;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.mail
  "Detectives for mail infrastructure, judging the evidence collected by
   gumshoe.mail. Two families: deliverability (MX/SPF/DMARC/DKIM/rDNS records
   that decide whether mail lands in the inbox) and the live services
   (reachable ports, STARTTLS on cleartext ports, valid TLS certificates)."
  (:require [clojure.string :as str]
            [gumshoe.mail :as mail]))

(def ^:private certificate-warning-days 14)

;; ---------------------------------------------------------------------------
;; deliverability

(defn detect-mx-problems
  [evidence]
  (let [domain (get evidence "domain")
        records (keep mail/parse-mx (get evidence "mx"))]
    (if (empty? records)
      [{:severity :critical
        :component domain
        :summary "domain has no MX records"
        :hint "no mail server can receive mail for this domain"}]
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
  [evidence]
  (let [domain (get evidence "domain")
        records (mail/spf-records (get evidence "spf"))]
    (cond
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
  [evidence]
  (let [domain (get evidence "domain")
        policy (mail/dmarc-policy (get evidence "dmarc"))]
    (cond
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
  [evidence]
  (for [[selector txt] (get evidence "dkim")
        :let [records (filter #(str/includes? % "v=DKIM1") (map mail/txt-record-value txt))]
        :when (empty? records)]
    {:severity :warning
     :component (format "%s._domainkey.%s" selector (get evidence "domain"))
     :summary "DKIM selector has no key published"
     :hint "mail signed with this selector fails DKIM - publish the key or stop signing with it"}))

(defn detect-reverse-dns
  [evidence]
  (let [host (get evidence "host")]
    (for [[address ptr] (get evidence "ptr")
          :let [ptr-host (some-> ptr str str/trim (str/replace #"\.$" ""))]
          finding [(cond
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

(def detectives
  [{:name "mail-mx"
    :description "MX records: mail can be received at all"
    :requires ["mx" "domain"]
    :detect detect-mx-problems}
   {:name "mail-spf"
    :description "SPF record present, unique, and not dangerously permissive"
    :requires ["spf" "domain"]
    :detect detect-spf-problems}
   {:name "mail-dmarc"
    :description "DMARC policy published and enforcing"
    :requires ["dmarc" "domain"]
    :detect detect-dmarc-problems}
   {:name "mail-dkim"
    :description "DKIM keys published for the configured selectors"
    :requires ["dkim" "domain"]
    :detect detect-dkim-problems}
   {:name "mail-rdns"
    :description "Forward-confirmed reverse DNS for the mail host"
    :requires ["ptr" "host"]
    :detect detect-reverse-dns}
   {:name "mail-services"
    :description "SMTP/POP3/IMAP ports reachable, STARTTLS offered, certificates valid"
    :requires ["probes" "host"]
    :detect detect-service-problems}])
