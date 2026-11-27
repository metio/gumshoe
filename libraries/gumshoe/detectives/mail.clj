;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.mail
  "Detectives for mail infrastructure, judging the evidence collected by
   gumshoe.mail. Two families: deliverability (MX/SPF/DMARC/DKIM/rDNS records
   that decide whether mail lands in the inbox) and the live services
   (reachable ports, STARTTLS on cleartext ports, valid TLS certificates).

   Every check branches on the state of a gumshoe.reads.result result -
   :present, :absent or :unread - never on nil against an empty value, so
   an unanswered read cannot be judged as a missing record whatever order
   the branches are written in. A read that was never made (no domain, no
   host) is :unread too, and the engine marks the check unread with what
   was not given."
  (:require [clojure.string :as str]
            [gumshoe.dns :as dns]
            [gumshoe.mail :as mail]
            [gumshoe.reads.result :as result]))

(def ^:private certificate-warning-days 14)

;; ---------------------------------------------------------------------------
;; reading the evidence

(defn- records-result
  "A result from the older evidence shape of a record read: nil when it did
   not answer, a list of records, empty when there were none."
  [ref v]
  (cond
    (not (sequential? v)) (result/unread ref "the read did not answer")
    (empty? v) (result/absent ref {:items []})
    :else (result/present ref {:items (vec v)})))

(defn- ptr-result
  "A PTR result from the older per-address value: a name, \"\" when the
   server answered with no PTR record, nil when it did not answer. Only a
   string is an answer, so nil never reaches the blank test."
  [address ptr]
  (let [ref {:source :dns :type "PTR" :name address :reverse? true}]
    (cond
      (not (string? ptr)) (result/unread ref "the read did not answer")
      (str/blank? ptr) (result/absent ref {:items []})
      :else (result/present ref {:items [ptr]}))))

(defn reading
  "The result held under key of the mail evidence. collect-evidence! stores
   results; a caller handing the detectives the older shapes - nil, a list
   of records, [[address ptr] ..] for \"ptr\", {selector txt} for \"dkim\" -
   gets the same result, so every check judges states alone."
  [evidence key]
  (let [v (get evidence key)
        ref {:source :mail :read (keyword key)}]
    (cond
      (result/result? v) v
      (and (= "ptr" key) (sequential? v) (seq v))
      (result/present ref {:items (mapv first v)
                           :addresses (into (sorted-map) (map (fn [[address ptr]] [address (ptr-result address ptr)])) v)})
      (and (= "dkim" key) (map? v) (seq v))
      (result/present ref {:selectors (into (sorted-map)
                                            (map (fn [[selector txt]]
                                                   [selector (records-result {:source :dns :type "TXT" :name selector} txt)]))
                                            v)})
      (and (= "dkim" key) (map? v)) (result/absent ref {:selectors {}})
      :else (records-result ref v))))

(defn- items-of
  "The items of an answered result, [] for an :absent one."
  [r]
  (vec (:items r)))

;; ---------------------------------------------------------------------------
;; deliverability

(defn detect-mx-problems
  "Nothing is judged when the MX query did not answer or was never made;
   an :absent answer (NXDOMAIN, or NOERROR without records) is a domain
   with no MX record."
  [evidence]
  (let [domain (get evidence "domain")
        no-mx (fn [_] [{:severity :critical
                        :component domain
                        :summary "domain has no MX records"
                        :hint "no mail server can receive mail for this domain"}])]
    (result/on-state
     (reading evidence "mx")
     {:unread (constantly [])
      :absent no-mx
      :present
      (fn [r]
        (let [records (keep mail/parse-mx (items-of r))]
          (if (empty? records)
            (no-mx r)
            (concat
             (when (= 1 (count records))
               [{:severity :info
                 :component domain
                 :summary "domain has a single MX record - no fallback receiver"}])
             (for [{:keys [host preference]} records
                   :when (str/blank? (str host))]
               {:severity :critical
                :component domain
                :summary (format "MX with preference %s has no hostname" preference)})))))})))

(defn- spf-findings
  [domain records]
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

        :else []))))

(defn detect-spf-problems
  "Nothing is judged when the TXT query did not answer or was never made."
  [evidence]
  (let [domain (get evidence "domain")
        judge #(spf-findings domain (mail/spf-records (items-of %)))]
    (result/on-state (reading evidence "spf")
                     {:unread (constantly []) :absent judge :present judge})))

(defn- dmarc-findings
  [domain policy]
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

    :else []))

(defn detect-dmarc-problems
  "Nothing is judged when the TXT query did not answer or was never made."
  [evidence]
  (let [domain (get evidence "domain")
        judge #(dmarc-findings domain (mail/dmarc-policy (items-of %)))]
    (result/on-state (reading evidence "dmarc")
                     {:unread (constantly []) :absent judge :present judge})))

(defn- dkim-finding
  [component r]
  (let [no-key (fn [_] [{:severity :warning
                         :component component
                         :summary "DKIM selector has no key published"
                         :hint "mail signed with this selector fails DKIM - publish the key or stop signing with it"}])]
    (result/on-state
     r
     {:unread (fn [_] [{:severity :info
                        :component component
                        :summary "DKIM selector could not be queried - the DNS server did not answer"
                        :hint "run the check again; a server that keeps failing to answer is a DNS problem of its own"}])
      :absent no-key
      :present (fn [r]
                 (if (some #(str/includes? % "v=DKIM1") (map mail/txt-record-value (items-of r)))
                   []
                   (no-key r)))})))

(defn detect-dkim-problems
  "A selector whose query answered without a key is a missing key; one whose
   query did not answer is reported as unanswered, never as missing. Nothing
   is judged when no selector answered or none was asked about."
  [evidence]
  (let [domain (get evidence "domain")
        per-selector (fn [r]
                       (mapcat (fn [[selector answer]]
                                 (dkim-finding (format "%s._domainkey.%s" selector domain) answer))
                               (sort-by key (:selectors r))))]
    (result/on-state (reading evidence "dkim")
                     {:unread (constantly []) :absent per-selector :present per-selector})))

(defn- ptr-finding
  [host address r]
  (result/on-state
   r
   {:unread (fn [r]
              [{:severity :info
                :component host
                :summary (format "PTR record of %s could not be queried - the DNS server did not answer" address)
                :hint (str "run the check again; a server that keeps failing to answer is a DNS problem of its own"
                           (when-let [reason (:reason r)] (str " (" reason ")")))}])
    :absent (fn [_]
              [{:severity :critical
                :component host
                :summary (format "%s has no PTR record" address)
                :hint "many receivers reject mail from IPs without reverse DNS outright"}])
    :present (fn [r]
               (let [ptr-host (dns/strip-dot (str/trim (str (first (items-of r)))))]
                 ;; DNS names are case-insensitive (RFC 4343), so a PTR that
                 ;; differs only in letter-case is still a forward-confirmed match.
                 (if (= (str/lower-case (str host)) (str/lower-case ptr-host))
                   []
                   [{:severity :warning
                     :component host
                     :summary (format "%s resolves back to %s, not %s (no forward-confirmed rDNS)" address ptr-host host)
                     :hint "FCrDNS mismatches lower sender reputation with strict receivers"}])))}))

(defn detect-reverse-dns
  "Each address of the host against its PTR result: :absent has no PTR,
   :unread is reported as unanswered, :present is compared with the host.
   A host whose address query answered with no address has nothing to look
   up and is reported as such; nothing is judged when that query did not
   answer or was never made."
  [evidence]
  (let [host (get evidence "host")]
    (result/on-state
     (reading evidence "ptr")
     {:unread (constantly [])
      :absent (fn [_]
                [{:severity :warning
                  :component host
                  :summary "mail host resolves to no IPv4 address, so it has no reverse DNS to check"
                  :hint "receivers look up the sending address; publish an A record for the mail host"}])
      :present (fn [r]
                 (mapcat (fn [[address answer]] (ptr-finding host address answer))
                         (:addresses r)))})))

;; ---------------------------------------------------------------------------
;; live services

(defn detect-service-problems
  "Each probe of the host's service ports. Nothing is judged when no host
   was given, so no port was probed."
  [evidence]
  (let [host (get evidence "host")
        now (:now evidence)
        probes (let [r (reading evidence "probes")]
                 (if (result/unread? r) [] (items-of r)))]
    (apply concat
           (for [probe probes]
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
    :joins ["probes"]
    :detect (about "host" detect-service-problems)}])
