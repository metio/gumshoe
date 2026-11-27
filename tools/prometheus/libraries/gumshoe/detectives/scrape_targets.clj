;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.scrape-targets
  "Why a Prometheus scrape target is down, from the lastError Prometheus
   publishes for it.

   The error text sends an operator to one of three places, and reading it
   comes before any theory about policy or workload:

     :dropped          nothing answered the connection attempt - the network
                       or routing between the Prometheus pod and the target
     :refused          the target's host answered that nothing listens on the
                       port - the workload and the port it binds
     :above-transport  the connection was made and the scrape failed above
                       it - TLS, authentication, the metrics path, a non-200
                       answer, a scrape limit; not a network-policy question

   :lookup names an error that never reached a connection because the host
   name did not resolve. Two more classes keep the judgement honest. :timed-out is a scrape that
   ran out of its timeout without saying how far it got: Go reports
   `context deadline exceeded` both when the connection was never made (the
   request's context ends while it waits for a connection) and when a
   connected target answered too slowly. :unknown is any text no rule
   matches; it is quoted as it came rather than guessed into a class.

   The rules match the error after its `Get \"<url>\": ` prefix is removed,
   so a host name such as tls-exporter or x509-exporter cannot decide the
   class. They are case-sensitive and quote the texts Go's net package and
   Prometheus' scraper write.

   The hint per class is a seam: (detective {:hints {class f}}) puts f, a
   string or (fn [target]) returning one or nil, over the default for that
   class, so a casebook names the next step its own estate needs; nil falls
   back to the default. :extra-rules adds rules tried after the built-in ones
   and before :unknown, so an estate-specific error text gets a class of its
   own (with a hint under :hints) without reclassifying a known one.

   Every text a finding quotes - the error, the URL, the pool - is cluster
   supplied, so it is made printable (control and format characters out) and
   cut before it is shown."
  (:require [clojure.string :as str]
            [gumshoe.prometheus.targets :as targets]
            [gumshoe.render :as render]))

(def default-rules
  "The built-in rules, tried in order; the first whose pattern finds a match
   in the error decides its class."
  [{:class :lookup :pattern #"\bdial (?:tcp|udp)[46]?: lookup "
    :why "the target's host name did not resolve"}
   {:class :dropped :pattern #"\bdial tcp[46]? \S+: i/o timeout"
    :why "the dial itself timed out: no answer to the connection attempt"}
   {:class :dropped :pattern #"\bconnect: no route to host"
    :why "no route to the target's address"}
   {:class :dropped :pattern #"\bconnect: network is unreachable"
    :why "the target's network is unreachable"}
   {:class :dropped :pattern #"\bconnect: host is down"
    :why "the target's host is down"}
   {:class :refused :pattern #"\bconnect: connection refused"
    :why "the host answered that nothing listens on the port"}
   {:class :above-transport :pattern #"\bserver returned HTTP status "
    :why "the target answered with a status other than 200"}
   {:class :above-transport :pattern #"\bx509: "
    :why "the target's certificate was not accepted"}
   {:class :above-transport :pattern #"\btls: "
    :why "the TLS handshake failed"}
   {:class :above-transport :pattern #"\bhttp: server gave HTTP response to HTTPS client"
    :why "the target speaks plain HTTP where HTTPS was configured"}
   {:class :above-transport :pattern #"\b(?:body size|sample|histogram bucket) limit exceeded"
    :why "the answer exceeded a scrape limit"}
   {:class :above-transport :pattern #"\blabel_(?:name_length_|value_length_)?limit exceeded"
    :why "the answer exceeded a label limit"}
   {:class :above-transport :pattern #"\bmissing metric name|\binvalid metric name or label names"
    :why "the answer did not parse as metrics"}
   {:class :above-transport :pattern #"(?:^|: )(?:unexpected )?EOF$"
    :why "the target closed the connection before it answered"}
   {:class :timed-out :pattern #"\bcontext deadline exceeded|\bi/o timeout|\bTLS handshake timeout|\btimeout awaiting response headers"
    :why "the scrape ran out of its timeout"}])

(def classes
  "What each class means, as a finding's summary names it."
  {:dropped "dropped before anything answered"
   :refused "connection refused"
   :above-transport "connected, failed above the transport"
   :timed-out "timed out, connected or not"
   :lookup "the host name did not resolve"
   :unknown "an error no rule classifies"})

(def default-hints
  "Where to look next, per class."
  {:dropped (str "nothing answered the connection attempt, so the packets are dropped on the way: "
                 "a network policy, a firewall or routing between the Prometheus pod and the target "
                 "decides this, not the workload")
   :refused (str "the target's host answered and nothing listens on that port: check that the "
                 "workload runs and binds this port on this address - the network delivered the packet")
   :above-transport (str "the connection was made and the scrape failed above it - TLS, "
                         "authentication, the metrics path, the answer or a scrape limit; "
                         "not a network-policy question")
   :timed-out (str "the error does not say whether the connection was made: either nothing "
                   "answered the connection attempt (look at policy and routing, as for a dropped "
                   "target) or the target answered slower than the scrape timeout (look at the "
                   "workload); a target on the same host that is up points to the second")
   :lookup (str "the scrape never reached a connection: the target's host name did not resolve "
                "from the Prometheus pod - check the name and the DNS the pod uses")
   :unknown "no rule classifies this error, so it is quoted as it came - read it before choosing where to look"})

(def ^:private error-limit 300)

(def ^:private text-limit 160)

(defn shown
  "Pure: cluster-supplied text as one printable line, at most limit columns."
  [limit s]
  (render/cut limit (render/printable s)))

(defn error-body
  "Pure: the error with the request prefix (`Get \"<url>\": `) removed, so
   the URL cannot match a rule."
  [last-error]
  (str/replace-first (str last-error) #"^[A-Z][a-z]* \"[^\"]*\": " ""))

(defn classify
  "Pure: the first of rules whose pattern finds a match in the error (see
   error-body), or {:class :unknown} when none does, a blank error included."
  ([last-error] (classify default-rules last-error))
  ([rules last-error]
   (let [body (error-body last-error)]
     (or (when-not (str/blank? body)
           (first (filter #(re-find (:pattern %) body) rules)))
         {:class :unknown}))))

(defn down-targets
  "The targets Prometheus reports as down; a target not yet scraped
   (health unknown) is not."
  [evidence]
  (filter #(= "down" (:health %)) (:targets evidence)))

(defn hint-for
  "Pure: the hint for a target of class: hints' entry for the class (a
   string, or a function of the target), else the default for the class."
  [hints class target]
  (let [h (get hints class)
        given (some-> (if (fn? h) (h target) h) str)]
    (if (str/blank? given)
      (get default-hints class (:unknown default-hints))
      given)))

(defn target-finding
  [rules hints severity target]
  (let [{:keys [class why]} (classify rules (:last-error target))
        target (assoc target :class class)
        error (if (str/blank? (:last-error target)) "no lastError reported" (shown error-limit (:last-error target)))]
    {:severity severity
     :component (format "%s shard %d %s" (:prometheus target) (:shard target) (shown text-limit (:scrape-pool target)))
     :summary (format "%s is down - %s: %s" (shown text-limit (:scrape-url target))
                      (get classes class (name class)) error)
     :hint (cond->> (hint-for hints class target)
             why (str why "; "))
     :class class}))

(defn detect-down-targets
  "Pure: one finding per down target, its class and hint."
  [rules hints severity evidence]
  (mapv #(target-finding rules hints severity %) (down-targets evidence)))

(defn detect-unread-shards
  "Pure: a critical finding per shard whose targets could not be read, and
   an info finding per Prometheus scaled down to no replica."
  [evidence]
  (vec (for [{:keys [prometheus shard statefulset state reason]} (:shards evidence)
             :when (#{:unread :scaled-down} state)]
         (if (= :unread state)
           {:severity :critical
            :component (format "%s shard %d" prometheus shard)
            :summary (format "the targets of shard %d (StatefulSet %s) could not be read" shard statefulset)
            :hint (str (shown error-limit reason)
                       " - the sweep is incomplete: the targets this shard scrapes were not judged, "
                       "so no finding about them is no evidence they are up")}
           {:severity :info
            :component (format "%s shard %d" prometheus shard)
            :summary (format "Prometheus %s runs no replica, so shard %d scrapes nothing" prometheus shard)}))))

(defn detective
  "The down-targets detective. Options:
     :hints        {class hint} over default-hints; a hint is a string or
                   (fn [target]) returning one, nil for the default
     :extra-rules  [{:class :pattern :why}] tried after default-rules
     :severity     the severity of a down target (default :warning)"
  ([] (detective {}))
  ([{:keys [hints extra-rules severity] :or {severity :warning}}]
   (let [rules (into default-rules extra-rules)]
     {:name "scrape-targets"
      :description "Prometheus scrape targets that are down, with the cause their lastError names"
      :requires [targets/evidence-key]
      :detect #(detect-down-targets rules hints severity (get % targets/evidence-key))})))

(def shards-detective
  {:name "scrape-target-shards"
   :description "Prometheus shards whose scrape targets could not be read"
   :requires [targets/evidence-key]
   :detect #(detect-unread-shards (get % targets/evidence-key))})

(defn detectives-with
  "Both detectives, the down-targets one built with opts (see detective)."
  [opts]
  [(detective opts) shards-detective])

(def detectives
  (detectives-with {}))
