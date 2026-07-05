;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mail
  "Evidence collection for mail infrastructure. Two independent angles:
   deliverability records pulled from DNS (dig), and live protocol probes
   against the running services (openssl s_client / bash TCP). Pure parsing
   and planning stay here; the detectives judge the evidence."
  (:require [clojure.string :as str]
            [gumshoe.dns :as dns]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

;; ---------------------------------------------------------------------------
;; service catalogue

(def services
  "Every mail service port worth probing. :implicit-tls ports wrap TLS from
   the first byte; :starttls ports begin in cleartext and must advertise
   STARTTLS so credentials are never sent in the clear."
  [{:name "smtp" :port 25 :tls :starttls :starttls :smtp :greeting "220"}
   {:name "submission" :port 587 :tls :starttls :starttls :smtp :greeting "220"}
   {:name "smtps" :port 465 :tls :implicit :greeting "220"}
   {:name "pop3" :port 110 :tls :starttls :starttls :pop3 :greeting "+OK"}
   {:name "pop3s" :port 995 :tls :implicit :greeting "+OK"}
   {:name "imap" :port 143 :tls :starttls :starttls :imap :greeting "* OK"}
   {:name "imaps" :port 993 :tls :implicit :greeting "* OK"}])

;; ---------------------------------------------------------------------------
;; pure record parsing

(defn txt-record-value
  "dig +short returns TXT records wrapped in quotes and possibly split into
   chunks; join the chunks and strip the quotes."
  [line]
  (-> (str line)
      (str/replace #"\"\s+\"" "")
      (str/replace #"^\"|\"$" "")))

(defn spf-records
  [txt-lines]
  (filter #(str/starts-with? % "v=spf1") (map txt-record-value txt-lines)))

(defn spf-all-qualifier
  "The qualifier of the terminating 'all' mechanism: \"+\", \"-\", \"~\", \"?\",
   or nil when there is no 'all'."
  [spf]
  ;; Only a standalone `all` mechanism counts - it is bounded by start/space on
  ;; the left and space/end on the right, so 'all' inside include:_spf.firewall
  ;; or a:mail.install is not mistaken for it. Take the last such token, since
  ;; `all` terminates the record.
  (when-let [match (last (re-seq #"(?:^|\s)([-+~?]?)all(?=\s|$)" (str spf)))]
    (let [q (second match)]
      (if (str/blank? q) "+" q))))

(defn dmarc-policy
  [txt-lines]
  (when-let [record (first (filter #(str/starts-with? % "v=DMARC1") (map txt-record-value txt-lines)))]
    (some-> (re-find #"\bp=([a-z]+)" record) second)))

(defn parse-mx
  "'10 mail.example.org.' -> {:preference 10 :host \"mail.example.org\"}"
  [line]
  (let [[preference host] (str/split (str/trim (str line)) #"\s+" 2)]
    (when host
      {:preference (parse-long preference)
       :host (dns/strip-dot host)})))

(defn certificate-days-left
  "Days until notAfter, parsed from openssl's 'notAfter=' line. nil when the
   date can not be read."
  [now not-after-line]
  (try
    (let [date-str (str/trim (str/replace (str not-after-line) #"^notAfter=" ""))
          formatter (java.time.format.DateTimeFormatter/ofPattern "MMM ppd HH:mm:ss yyyy zzz" java.util.Locale/US)
          expiry (.toInstant (java.time.ZonedDateTime/parse date-str formatter))]
      (.toDays (java.time.Duration/between now expiry)))
    (catch Exception _ nil)))

;; ---------------------------------------------------------------------------
;; live probes

(defn- s-client
  "Runs `openssl s_client` against host:port as an argument vector, so the host
   is never reparsed by a shell. starttls is a protocol keyword (:smtp/:pop3/
   :imap) for a cleartext port that upgrades, or nil for an implicit-TLS port.
   Feeds QUIT on stdin so the client exits once the handshake is done."
  [host port starttls]
  (apply shell/execute-with-stdin "QUIT\n"
         "openssl" "s_client" "-connect" (format "%s:%d" host port) "-servername" host
         (when starttls ["-starttls" (name starttls)])))

(defn- connected?
  "Whether openssl reached the port at all. A refused, filtered, or timed-out
   connection never opens the socket; a reachable port that only fails the TLS
   or STARTTLS negotiation still counts as reachable."
  [{:keys [exit out err]}]
  (not (or (#{124 127} exit)
           (re-find #"(?i)errno|connection refused|no route to host|could not resolve|name or service not known"
                    (str out err)))))

(defn- enddate-of
  "The certificate's notAfter line, extracted from s_client's PEM output via
   `openssl x509`. nil when no certificate was presented or it can not be read."
  [pem]
  (when (str/includes? (str pem) "BEGIN CERTIFICATE")
    (let [result (shell/execute-with-stdin (str pem) "openssl" "x509" "-noout" "-enddate")]
      (when (zero? (:exit result)) (str/trim (:out result))))))

(defn- probe-tls
  "Probes one service port. A completed handshake prints the server certificate,
   so its presence is the success signal for both implicit-TLS and STARTTLS
   ports; the notAfter is read from that same certificate. The port is reachable
   whenever openssl connected, even if the negotiation then failed."
  [host {:keys [port tls starttls]}]
  (let [result (s-client host port starttls)
        established (str/includes? (str (:out result) (:err result)) "BEGIN CERTIFICATE")
        base {:reachable (or established (connected? result))
              :not-after (enddate-of (:out result))}]
    (if (= :implicit tls)
      (assoc base :tls-ok established)
      (assoc base :starttls-ok established))))

(defn probe-service
  [host service]
  (assoc (probe-tls host service)
         :service (:name service)
         :port (:port service)
         :tls (:tls service)))

;; ---------------------------------------------------------------------------
;; evidence collection

(defn collect-evidence!
  "Collects deliverability records and live protocol probes in parallel."
  [{:keys [domain host server dkim-selectors]}]
  (stdout/print-section "🔍 Evidence (dig + openssl)")
  (stdout/err-println (format "  %s domain %s, host %s" (stdout/blue "▸") domain host))
  (let [q (fn [type name] (dns/query {:server server} type name))
        mx (future (q "MX" domain))
        spf (future (q "TXT" domain))
        dmarc (future (q "TXT" (str "_dmarc." domain)))
        ptr (future (mapv #(vector % (first (dns/ptr-records {:server server} %)))
                          (remove str/blank? (q "A" host))))
        dkim (future (into {} (map (fn [selector]
                                     [selector (q "TXT" (format "%s._domainkey.%s" selector domain))])
                                   dkim-selectors)))
        probes (mapv (fn [service] (future (probe-service host service))) services)]
    (stdout/err-println (format "  %s probing %d service ports on %s" (stdout/blue "▸") (count services) host))
    {:now (java.time.Instant/now)
     "domain" domain
     "host" host
     "mx" @mx
     "spf" @spf
     "dmarc" @dmarc
     "ptr" @ptr
     "dkim" @dkim
     "probes" (mapv deref probes)}))
