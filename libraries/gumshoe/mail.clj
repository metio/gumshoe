;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.mail
  "Evidence collection for mail infrastructure. Two independent angles:
   deliverability records pulled from DNS (dig), and live protocol probes
   against the running services (openssl s_client / bash TCP). Pure parsing
   and planning stay here; the detectives judge the evidence.

   Every value collect-evidence! reads is a three-state result of
   gumshoe.reads.result. The MX, SPF and DMARC values are the results of
   gumshoe.dns/lookup. The reverse DNS value (ptr-evidence) is the result
   of the address query, carrying one PTR result per address under
   :addresses; the DKIM value (dkim-evidence) carries one TXT result per
   selector under :selectors; the service probes are a :present result
   with the probe maps as :items. A read that depends on an input the
   operator did not give - a domain, a host, a DKIM selector - is never
   made, and its value is result/not-asked: :unread like a read that did
   not answer, so no check passes on it, and marked so the report can say
   which input was missing."
  (:require [clojure.string :as str]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.dns :as dns]
            [gumshoe.reads.result :as result]
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

(def default-probe-timeout-ms
  "How long one TLS probe may take. openssl s_client has no connect timeout of
   its own, so a filtered port would otherwise wait out the kernel's SYN retries
   (about two minutes on Linux); the deadline turns that into exit 124, which
   connected? reads as unreachable."
  15000)

(defn- s-client
  "Runs `openssl s_client` against host:port as an argument vector, so the host
   is never reparsed by a shell. starttls is a protocol keyword (:smtp/:pop3/
   :imap) for a cleartext port that upgrades, or nil for an implicit-TLS port.
   Feeds QUIT on stdin so the client exits once the handshake is done, and gives
   up after timeout-ms."
  [host port starttls timeout-ms]
  (binding [shell/*timeout-ms* timeout-ms]
    (apply shell/execute-with-stdin "QUIT\n"
           "openssl" "s_client" "-connect" (format "%s:%d" host port) "-servername" host
           (when starttls ["-starttls" (name starttls)]))))

(defn- connected?
  "Whether openssl reached the port at all. A refused, filtered, or timed-out
   connection never opens the socket; a reachable port that only fails the TLS
   or STARTTLS negotiation still counts as reachable."
  [{:keys [exit out err]}]
  (or
   ;; s_client prints CONNECTED(<fd>) once the socket is open; an errno it
   ;; prints after that (write:errno=32 from a server that drops the line)
   ;; belongs to the negotiation, not to the connect
   (boolean (re-find #"(?m)^CONNECTED\(" (str out)))
   (not (or (#{124 127} exit)
            (re-find #"(?i)errno|connection refused|no route to host|could not resolve|name or service not known"
                     (str out err))))))

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
  [host {:keys [port tls starttls]} timeout-ms]
  (let [result (s-client host port starttls timeout-ms)
        established (str/includes? (str (:out result) (:err result)) "BEGIN CERTIFICATE")
        base {:reachable (or established (connected? result))
              :not-after (enddate-of (:out result))}]
    (if (= :implicit tls)
      (assoc base :tls-ok established)
      (assoc base :starttls-ok established))))

(defn probe-service
  "Probes one service port on host, each probe bounded by timeout-ms
   (default-probe-timeout-ms when not given)."
  ([host service] (probe-service host service default-probe-timeout-ms))
  ([host service timeout-ms]
   (assoc (probe-tls host service timeout-ms)
          :service (:name service)
          :port (:port service)
          :tls (:tls service))))

;; ---------------------------------------------------------------------------
;; evidence collection

(def ^:private read-words
  {:mx "the MX records"
   :spf "the SPF record"
   :dmarc "the DMARC record"
   :dkim "the DKIM keys"
   :ptr "the reverse DNS"
   :probes "the service ports"})

(defmethod result/describe :mail
  [{:keys [read domain host selectors server]}]
  (str (get read-words read (str "the " (name (or read :mail)) " read"))
       (cond
         domain (str " of " domain)
         host (str " of " host)
         (#{:mx :spf :dmarc :dkim} read) " of the mail domain"
         (#{:ptr :probes} read) " of the mail host"
         :else "")
       (when (seq selectors) (str " (selector" (when (next selectors) "s") " " (str/join ", " selectors) ")"))
       (when server (str " at " server))))

(defn- address?
  [s]
  (boolean (re-matches #"\d{1,3}(\.\d{1,3}){3}|[0-9A-Fa-f:]*:[0-9A-Fa-f:.]*" s)))

(defn- reasons-of
  [results]
  (str/join ", " (distinct (keep :reason results))))

(defn ptr-evidence
  "The reverse DNS of host as a result. The address query decides the
   state: :unread when it did not answer, :absent when it answered with no
   address. When it answered with addresses, :addresses holds one PTR
   result per address (sorted by address), each :present with the PTR
   names as :items, :absent when that server answered with no PTR record,
   or :unread when that query did not answer; the whole is :present, and
   :unread only when no PTR query answered, so the check is unread instead
   of reporting each address as unanswered. dig lists a CNAME's target
   before the addresses it leads to; that name is no address and has no
   reverse record of its own, so it is skipped."
  [server host]
  (let [connection {:server server}
        a (dns/lookup connection "A" host)
        addresses (when (result/present? a) (filterv address? (:items a)))]
    (cond
      (result/unread? a) a
      (empty? addresses) (result/absent (:ref a) {:items [] :status (:status a)})
      :else
      (let [ptrs (into (sorted-map)
                       (map (fn [address]
                              [address (dns/lookup (assoc connection :reverse? true) "PTR" address)]))
                       addresses)
            ref (cond-> {:source :mail :read :ptr :host host} server (assoc :server server))]
        (if (every? result/unread? (vals ptrs))
          (result/unread ref (str "no PTR query answered: " (reasons-of (vals ptrs)))
                         {:items addresses :addresses ptrs})
          (result/present ref {:items addresses :addresses ptrs}))))))

(defn- given?
  [s]
  (and (string? s) (not (str/blank? s))))

(defn dkim-evidence
  "The DKIM keys as one result over answers, a map of selector to the TXT
   result of its query, kept under :selectors. :present when at least one
   selector answered, the others named there as :unread; :unread when none
   answered, so the DKIM check is unread instead of passing on no keys; and
   not-asked when there was no selector to ask about. ref names the read
   (default {:source :mail :read :dkim} with the selectors)."
  ([answers] (dkim-evidence {:source :mail :read :dkim} answers))
  ([ref answers]
   (let [selectors (into (sorted-map) answers)
         ref (assoc ref :selectors (vec (keys selectors)))]
     (cond
       (empty? selectors) (result/not-asked (dissoc ref :selectors) "no DKIM selector was given")
       (every? result/unread? (vals selectors))
       (result/unread ref (str "no selector answered: " (reasons-of (vals selectors)))
                      {:selectors selectors})
       :else (result/present ref {:selectors selectors})))))

(defn- not-asked
  [read reason]
  (fn [] (result/not-asked {:source :mail :read read} reason)))

(defn collect-evidence!
  "Collects deliverability records and live protocol probes in parallel. Each
   probe is bounded by :probe-timeout-ms (default-probe-timeout-ms).

   Every value is a result (see the namespace docstring). What is not
   probed is result/not-asked, naming the input that was not given: without
   a domain the MX, SPF, DMARC and DKIM records, without a host the reverse
   DNS and the service probes. The checks reading them are then unread,
   never clean, and the report says which input was missing."
  [{:keys [domain host server dkim-selectors probe-timeout-ms]
    :or {probe-timeout-ms default-probe-timeout-ms}}]
  (stdout/print-section "🔍 Evidence (dig + openssl)")
  (stdout/item :info (format "domain %s, host %s" (or domain "-") (or host "-")))
  (let [connection {:server server}
        q (fn [type name] (dns/lookup connection type name))
        domain? (given? domain)
        host? (given? host)
        no-domain "no domain was given"
        no-host "no host was given"
        _ (when host?
            (stdout/item :info (format "probing %d service ports on %s" (count services) host)))
        [mx spf dmarc ptr dkim & probes]
        (concurrent/all
         (into [(if domain? #(q "MX" domain) (not-asked :mx no-domain))
                (if domain? #(q "TXT" domain) (not-asked :spf no-domain))
                (if domain? #(q "TXT" (str "_dmarc." domain)) (not-asked :dmarc no-domain))
                (if host? #(ptr-evidence server host) (not-asked :ptr no-host))
                (if domain?
                  #(dkim-evidence
                    (cond-> {:source :mail :read :dkim :domain domain} server (assoc :server server))
                    (into {} (map (fn [selector]
                                    [selector (q "TXT" (format "%s._domainkey.%s" selector domain))])
                                  dkim-selectors)))
                  (not-asked :dkim no-domain))]
               (when host?
                 (map (fn [service] #(probe-service host service probe-timeout-ms)) services))))]
    {:now (java.time.Instant/now)
     "domain" domain
     "host" host
     "mx" mx
     "spf" spf
     "dmarc" dmarc
     "ptr" ptr
     "dkim" dkim
     "probes" (if host?
                (result/present {:source :mail :read :probes :host host} {:items (vec probes)})
                ((not-asked :probes no-host)))}))
