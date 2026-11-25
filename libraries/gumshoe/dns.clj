;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dns
  "Querying DNS servers with dig: short answers, tight timeouts, explicit
   transport selection. nil means the server did not answer at all, an empty
   vector means it answered with no records - the difference matters."
  (:require [clojure.string :as str]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(defn query-name?
  "Whether a name is one dig may be asked about: a hostname with an optional
   leading wildcard label and trailing dot, underscores allowed for the
   _dmarc and _domainkey style labels, at most 253 characters. Names reach
   dig from cluster objects a tenant writes, and dig reads an option-like
   token (`-f<file>`, `+<option>`) anywhere on its command line."
  [name]
  (boolean (and (string? name)
                (<= 1 (count name) 254)
                (re-matches #"(?:\*\.)?(?:[A-Za-z0-9_](?:[A-Za-z0-9_-]*[A-Za-z0-9_])?\.)*[A-Za-z0-9_](?:[A-Za-z0-9_-]*[A-Za-z0-9_])?\.?"
                            name))))

(defn address?
  "Whether a string is an IPv4 or IPv6 literal dig can reverse with -x."
  [address]
  (boolean (and (string? address)
                (or (re-matches #"\d{1,3}(?:\.\d{1,3}){3}" address)
                    (and (re-matches #"[0-9A-Fa-f:.]+" address) (str/includes? address ":"))))))

(defn dig-args
  "Pure assembly of a dig invocation, or nil when the name is not one to ask
   about (see query-name?). Without a server the system resolver answers -
   that is the right view for deliverability questions. The type and the name
   go in as the values of -t and -q, so neither is read as an option. dig
   prints the response header and the answer section only, which carries the
   response status +short leaves out. A :reverse? connection uses `dig -x`,
   which forms the in-addr.arpa / ip6.arpa query name from an IP; querying the
   bare IP as a PTR name never resolves."
  [{:keys [server transport reverse?]} type name]
  (when (if reverse? (address? name) (query-name? name))
    (vec (concat ["dig" "+noall" "+comments" "+answer" "+time=3" "+tries=1"]
                 (case transport
                   :ipv4 ["-4"]
                   :ipv6 ["-6"]
                   [])
                 (when server [(str "@" server)])
                 (if reverse? ["-x" name] ["-t" type "-q" name])))))

(defn response-status
  "The status of the last response header dig printed (NOERROR, NXDOMAIN,
   SERVFAIL, REFUSED, ...), or nil when it printed none. dig prints a second
   header when it retries a truncated answer over TCP; the last one is the
   answer it kept."
  [out]
  (last (keep #(second (re-find #"->>HEADER<<-.*\bstatus: ([A-Z]+)" %))
              (str/split-lines (str out)))))

(defn parse-answer
  "The record data of each answer line, as +short would print it: the fields
   after name, TTL, class and type. Every line dig prints besides the answer
   records - the header, section titles, `;; communications error to …` when
   one server times out and another answers - starts with `;` and is no
   record."
  [out]
  (into [] (keep (fn [line]
                   (when-not (or (str/blank? line) (str/starts-with? (str/triml line) ";"))
                     (nth (str/split (str/trim line) #"\s+" 5) 4 nil))))
        (str/split-lines (str out))))

(defn answer
  "What one dig run says: the records when the server answered NOERROR, []
   when it answered NXDOMAIN, and nil when dig failed, printed no response
   header, or the server answered with any other status - SERVFAIL, REFUSED
   and the like are a server failing to answer, not a name without records."
  [{:keys [exit out]}]
  (when (zero? exit)
    (case (response-status out)
      "NOERROR" (parse-answer out)
      "NXDOMAIN" []
      nil)))

(defn query
  "Answers as a vector of record data; [] when the server answered that there
   are none, nil when no answer was obtained: the server did not reply or
   failed the query, or the name is not one dig may be asked about."
  [connection type name]
  (when-let [args (dig-args connection type name)]
    (answer (apply shell/execute args))))

(defn ptr-records
  "Reverse-DNS (PTR) answers for an IP address, via `dig -x`. nil when the
   server did not reply."
  [connection address]
  (query (assoc connection :reverse? true) "PTR" address))

(defn strip-dot
  [name]
  (str/replace (str name) #"\.$" ""))

(defn parse-soa-serial
  "The serial is the third field of a +short SOA answer."
  [answer-line]
  (some-> answer-line (str/split #"\s+") (nth 2 nil)))

(defn soa-serial
  [connection zone]
  (some-> (first (query connection "SOA" zone)) parse-soa-serial))

(defn- answers?
  [connection zone]
  (boolean (seq (query connection "SOA" zone))))

(defn collect-evidence!
  "Probes the DNS setup in parallel: transports on the entry server, the
   zone's nameservers with their A/AAAA records, every nameserver's SOA
   serial, and the probe names on every nameserver. When the entry server
   does not answer the NS query, the nameservers, their addresses, serials
   and probes are all nil."
  [{:keys [server zone probes]}]
  (stdout/print-section "🔍 Evidence (dig)")
  (stdout/err-println (str "  " (stdout/blue "▸") " zone " zone " via " server))
  (let [nameservers (some->> (query {:server server} "NS" zone) (mapv strip-dot))
        _ (doseq [ns nameservers]
            (stdout/err-println (str "  " (stdout/blue "▸") " nameserver " ns)))
        [transports addresses serials probed]
        (concurrent/all
         [(fn [] {:ipv4 (answers? {:server server :transport :ipv4} zone)
                  :ipv6 (answers? {:server server :transport :ipv6} zone)})
          #(into {} (concurrent/map-all (fn [ns] [ns {:a (query {:server server} "A" ns)
                                                      :aaaa (query {:server server} "AAAA" ns)}])
                                        nameservers))
          #(into {} (concurrent/map-all (fn [ns] [ns (soa-serial {:server ns} zone)]) nameservers))
          #(into {} (concurrent/map-all (fn [probe]
                                          [probe (into {} (map (fn [ns] [ns (query {:server ns} "A" probe)])
                                                               nameservers))])
                                        probes))])]
    ;; without the NS answer there is no nameserver to ask, so the reads
    ;; that ask each nameserver stay unanswered rather than empty
    {:now (java.time.Instant/now)
     "zone" zone
     "server" server
     "transports" transports
     "nameservers" nameservers
     "addresses" (when nameservers addresses)
     "serials" (when nameservers serials)
     "probes" (when nameservers probed)}))
