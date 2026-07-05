;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns infra.mail
  "Evidence collection for mail infrastructure. Two independent angles:
   deliverability records pulled from DNS (dig), and live protocol probes
   against the running services (openssl s_client / bash TCP). Pure parsing
   and planning stay here; the detectives judge the evidence."
  (:require [clojure.string :as str]
            [infra.dns :as dns]
            [infra.shell :as shell]
            [infra.stdout :as stdout]))

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
  (some-> (re-find #"([-+~?]?)all\b" (str spf)) second (as-> q (if (str/blank? q) "+" q))))

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

(defn- probe-implicit-tls
  "Handshake TLS immediately and read the certificate's notAfter."
  [host {:keys [port]}]
  (let [result (shell/execute-with-stdin
                "QUIT\n"
                "openssl" "s_client" "-connect" (format "%s:%d" host port)
                "-servername" host "-brief")
        cert (shell/execute-with-stdin
              ""
              "sh" "-c"
              (format "echo QUIT | openssl s_client -connect %s:%d -servername %s 2>/dev/null | openssl x509 -noout -enddate"
                      host port host))]
    {:reachable (zero? (:exit result))
     :tls-ok (str/includes? (str (:err result)) "CONNECTION ESTABLISHED")
     :not-after (when (zero? (:exit cert)) (str/trim (:out cert)))}))

(defn- probe-starttls
  "Connect in cleartext, read the greeting, and check STARTTLS is advertised
   by completing a STARTTLS handshake via openssl."
  [host {:keys [port starttls greeting]}]
  (let [banner (shell/execute-with-stdin
                ""
                "sh" "-c"
                (format "echo QUIT | timeout 5 openssl s_client -connect %s:%d -starttls %s -servername %s 2>&1"
                        host port (name starttls) host))
        output (str (:out banner))]
    {:reachable (or (zero? (:exit banner))
                    (str/includes? output greeting)
                    (str/includes? output "CONNECTION ESTABLISHED"))
     :starttls-ok (str/includes? output "CONNECTION ESTABLISHED")
     :not-after (let [enddate (shell/execute-with-stdin
                               ""
                               "sh" "-c"
                               (format "echo QUIT | timeout 5 openssl s_client -connect %s:%d -starttls %s -servername %s 2>/dev/null | openssl x509 -noout -enddate"
                                       host port (name starttls) host))]
                  (when (zero? (:exit enddate)) (str/trim (:out enddate))))}))

(defn probe-service
  [host service]
  (assoc (if (= :implicit (:tls service))
           (probe-implicit-tls host service)
           (probe-starttls host service))
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
        ptr (future (mapv #(vector % (first (q "PTR" %)))
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
