;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.dns
  "Querying DNS servers with dig: short answers, tight timeouts, explicit
   transport selection.

   lookup reads one name as a three-state result of gumshoe.reads.result,
   and classify is its pure half over one dig run:

     NOERROR with records        :present, :items the record data,
                                 :records the parsed answer lines
     NOERROR without records     :absent, :items [], :records [],
                                 :status \"NOERROR\"
     NXDOMAIN                    :absent, :items [], :records [],
                                 :status \"NXDOMAIN\"
     SERVFAIL, REFUSED, ...      :unread, :reason the status
     no response header          :unread
     dig exited non-zero         :unread
     a name dig may not be asked :unread, and dig never runs

   An :absent result keeps the status, so a check can tell a name that does
   not exist (NXDOMAIN) from one that exists without records of the type
   (NOERROR, sometimes called NODATA). The ref of a result is
   {:source :dns :type :name :server :transport :reverse?}, described as
   \"TXT _dmarc.example.org at 10.0.0.1\".

   :records holds one map per answer line, {:name :ttl :type :data}, as dig
   printed it: :name and :data keep their trailing dot (strip-dot removes
   it), :ttl is an integer, :type is dig's uppercase spelling as a string
   (\"CNAME\", \"TXT\"), the same spelling a ref's :type uses, and :data is
   the record data exactly as in :items, a TXT record's quoted strings
   included. A CNAME chain answers with every link, so a caller that wants
   the records of the asked type filters on :type. records! reads them and
   throws for a read that did not answer, as items! does.

   query, ptr-records, soa-serial and answer keep the older shape: the
   records, [] for an answer without records, nil for a read that did not
   answer."
  (:require [clojure.string :as str]
            [gumshoe.concurrent :as concurrent]
            [gumshoe.reads.result :as result]
            [gumshoe.render :as render]
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

(defn- parse-record
  "One answer line as {:name :ttl :type :data}, or nil when the line is no
   record: name, a TTL of digits, a class, a type and the record data,
   separated by whitespace. The data is everything after the type, so a TXT
   record of several quoted strings, or one with spaces inside its quotes,
   stays whole."
  [line]
  (let [[name ttl class type data] (str/split (str/trim line) #"\s+" 5)]
    (when (and data
               (re-matches #"\d{1,10}" ttl)
               (re-matches #"[A-Za-z]+[0-9]*" class)
               (re-matches #"[A-Za-z][A-Za-z0-9]*" type))
      {:name name :ttl (parse-long ttl) :type (str/upper-case type) :data data})))

(defn parse-records
  "The answer records dig printed, one {:name :ttl :type :data} per answer
   line (see the namespace docstring). Every line dig prints besides the
   answer records - the header, section titles, `;; communications error to
   …` when one server times out and another answers - starts with `;` and
   is no record."
  [out]
  (into [] (keep (fn [line]
                   (when-not (or (str/blank? line) (str/starts-with? (str/triml line) ";"))
                     (parse-record line))))
        (str/split-lines (str out))))

(defn parse-answer
  "The record data of each answer line, as +short would print it: the fields
   after name, TTL, class and type (the :data of parse-records)."
  [out]
  (mapv :data (parse-records out)))

(defn- dig-failure
  "Why a dig run that exited non-zero failed: its exit and the last line it
   printed about it (\";; communications error ...: timed out\"), when any."
  [exit out]
  (let [line (some->> (str/split-lines (str out))
                      (keep #(let [t (str/trim %)]
                               (when (and (str/starts-with? t ";;")
                                          (not (re-find #"->>HEADER<<-" t)))
                                 (str/trim (subs t 2)))))
                      (remove str/blank?)
                      last)]
    (str "dig exited " exit (when line (str ": " (render/elide 120 (render/printable line)))))))

(defn classify
  "Pure: the result of one dig run {:exit :out} for ref, as the namespace
   docstring tabulates. NOERROR with records is :present with the record
   data as :items and the parsed answer lines as :records; NOERROR without
   records and NXDOMAIN are :absent with :items [], :records [] and the
   :status; every other status is :unread with the status as :reason, and
   so are a run that printed no response header and one that exited
   non-zero."
  [ref {:keys [exit out]}]
  (if-not (= 0 exit)
    (result/unread ref (dig-failure exit out))
    (let [status (response-status out)]
      (case status
        "NOERROR" (let [records (parse-records out)]
                    (if (seq records)
                      (result/present ref {:items (mapv :data records) :records records :status status})
                      (result/absent ref {:items [] :records [] :status status})))
        "NXDOMAIN" (result/absent ref {:items [] :records [] :status status})
        nil (result/unread ref "dig printed no response header")
        (result/unread ref status {:status status})))))

(defn ref-of
  "The ref of a lookup of name with type over connection."
  [{:keys [server transport reverse?]} type name]
  (cond-> {:source :dns :type type :name name}
    server (assoc :server server)
    transport (assoc :transport transport)
    reverse? (assoc :reverse? true)))

(defmethod result/describe :dns
  [{:keys [type name server transport reverse?]}]
  (let [shown (if (if reverse? (address? name) (query-name? name))
                name
                (pr-str (render/elide 80 (render/printable name))))]
    (str (if reverse? (str type " of " shown) (str type " " shown))
         (if server (str " at " server) " at the system resolver")
         (case transport :ipv4 " over IPv4" :ipv6 " over IPv6" ""))))

(defn lookup
  "Reads the records of type for name over connection {:server :transport
   :reverse?} as a three-state result (see the namespace docstring). A name
   dig may not be asked about (query-name?, or address? for :reverse?) is
   :unread and dig never runs. Never throws for a read that did not answer.

   Polarity: present? when a record must exist, absent? when none may, and
   items! for the record data or records! for the answer lines with their
   TTL and type, both of which throw for a read that did not answer."
  [connection type name]
  (let [ref (ref-of connection type name)]
    (if-let [args (dig-args connection type name)]
      (classify ref (apply shell/execute args))
      (result/unread ref (if (:reverse? connection)
                           "not an address dig can reverse"
                           "not a name dig may be asked about")))))

(defn records!
  "The parsed answer records of an answered lookup result, [] for :absent
   (see the namespace docstring). Throws an unread exception for an :unread
   result, as items! does, and IllegalArgumentException for an answered
   result that carries no :records, which no lookup or classify returns."
  [r]
  (result/items! r)
  (if (contains? r :records)
    (vec (:records r))
    (throw (IllegalArgumentException. "not a DNS result carrying :records"))))

(defn- legacy-answer
  "The older shape of a result: the records, [] when answered without any,
   nil when the read did not answer."
  [r]
  (case (result/state-of r)
    :present (vec (:items r))
    :absent []
    :unread nil))

(defn answer
  "What one dig run says: the records when the server answered NOERROR, []
   when it answered NXDOMAIN, and nil when dig failed, printed no response
   header, or the server answered with any other status - SERVFAIL, REFUSED
   and the like are a server failing to answer, not a name without records.
   classify returns the same as a result."
  [run]
  (legacy-answer (classify {:source :dns} run)))

(defn query
  "Answers as a vector of record data; [] when the server answered that there
   are none, nil when no answer was obtained: the server did not reply or
   failed the query, or the name is not one dig may be asked about. lookup
   returns the same as a result."
  [connection type name]
  (legacy-answer (lookup connection type name)))

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

(defn- with-stripped-names
  "An NS result whose items are the nameserver names without their trailing
   dot."
  [r]
  (if (contains? r :items) (update r :items #(mapv strip-dot %)) r))

(defn soa-serial-of
  "An SOA result with the serial of its first record as :value (nil when
   that record carries none), state kept."
  [r]
  (result/derived r #(some-> (first %) parse-soa-serial)))

(defn collect-evidence!
  "Probes the DNS setup in parallel: transports on the entry server, the
   zone's nameservers with their A/AAAA records, every nameserver's SOA
   serial, and the probe names on every nameserver. Every read is a
   three-state result of gumshoe.reads.result:

     \"transports\"  {:ipv4 r :ipv6 r}, the zone's SOA over each transport
     \"nameservers\" the NS result, :items the names without trailing dot
     \"addresses\"   {nameserver {:a r :aaaa r}}
     \"serials\"     {nameserver r}, r carrying the serial as :value
     \"probes\"      {probe {nameserver r}}

   When the entry server did not answer the NS query there is no nameserver
   to ask, so addresses, serials and probes are nil - unread rather than
   empty."
  [{:keys [server zone probes]}]
  (stdout/print-section "🔍 Evidence (dig)")
  (stdout/item :info (str "zone " zone " via " server))
  (let [ns-read (with-stripped-names (lookup {:server server} "NS" zone))
        nameservers (when (result/answered? ns-read) (result/items! ns-read))
        _ (doseq [ns nameservers]
            (stdout/item :info (str "nameserver " ns)))
        [transports addresses serials probed]
        (concurrent/all
         [(fn [] {:ipv4 (lookup {:server server :transport :ipv4} "SOA" zone)
                  :ipv6 (lookup {:server server :transport :ipv6} "SOA" zone)})
          #(into {} (concurrent/map-all (fn [ns] [ns {:a (lookup {:server server} "A" ns)
                                                      :aaaa (lookup {:server server} "AAAA" ns)}])
                                        nameservers))
          #(into {} (concurrent/map-all (fn [ns] [ns (soa-serial-of (lookup {:server ns} "SOA" zone))])
                                        nameservers))
          #(into {} (concurrent/map-all (fn [probe]
                                          [probe (into {} (map (fn [ns] [ns (lookup {:server ns} "A" probe)])
                                                               nameservers))])
                                        probes))])
        answered? (result/answered? ns-read)]
    {:now (java.time.Instant/now)
     "zone" zone
     "server" server
     "transports" transports
     "nameservers" ns-read
     "addresses" (when answered? addresses)
     "serials" (when answered? serials)
     "probes" (when answered? probed)}))
