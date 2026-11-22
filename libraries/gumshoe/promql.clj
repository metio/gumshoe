;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.promql
  "PromQL against the Prometheus HTTP query API, as served by Prometheus,
   Thanos Query and Mimir.

   An endpoint is a map built by `endpoint`: the base URL, the flavour, and
   the headers every request carries. The flavours differ only in where the
   API lives and what each request adds: Mimir serves it below /prometheus
   and selects its tenant with X-Scope-OrgID; Thanos deduplicates replicas
   and is asked to refuse a partial response, because an answer missing a
   store would silently shrink every count built on it.

   `query!` and `query-range!` fetch and parse; `parse` is pure and turns a
   response into {:ok true :type :vector :series [...]} or {:ok false
   :error \"...\"}, so callers judge a map and never handle an exception."
  (:require [clojure.string :as str]
            [gumshoe.http :as http]
            [gumshoe.secrets :as secrets])
  (:import (java.net URLEncoder)
           (java.nio.charset StandardCharsets)
           (java.time Instant)
           (java.util Base64)))

(def flavors #{:prometheus :thanos :mimir})

(defn- basic-auth [username password]
  (str "Basic " (.encodeToString (Base64/getEncoder)
                                 (.getBytes (str username ":" password) StandardCharsets/UTF_8))))

(defn credentials
  "{:username :password} for url: the given ones, else the login and password
   of the secret whose path matches the URL's host, else nil values."
  [url {:keys [username password]}]
  (let [secret (when-not (and username password) (secrets/find-path url))]
    {:username (or username (when secret (secrets/field secret "login")))
     :password (or password (when secret (secrets/password secret)))}))

(defn endpoint
  "An endpoint for url. :flavor is one of `flavors` (default :prometheus),
   :tenant the Mimir tenant, and :username with :password add basic auth;
   :headers are merged last."
  [{:keys [url flavor tenant username password headers] :or {flavor :prometheus}}]
  (when-not (contains? flavors flavor)
    (throw (ex-info (format "unknown PromQL flavour %s - one of %s" (pr-str flavor) (str/join ", " (map name (sort flavors))))
                    {:flavor flavor})))
  {:url (str/replace (str url) #"/+$" "")
   :flavor flavor
   :headers (cond-> {}
              (and username password) (assoc "Authorization" (basic-auth username password))
              (and (= :mimir flavor) tenant) (assoc "X-Scope-OrgID" tenant)
              headers (merge headers))})

(defn- encode [v] (URLEncoder/encode (str v) StandardCharsets/UTF_8))

(defn- api-base [{:keys [url flavor]}]
  (str url (when (= :mimir flavor) "/prometheus") "/api/v1"))

(defn- flavor-params [{:keys [flavor]}]
  (when (= :thanos flavor) [["dedup" "true"] ["partial_response" "false"]]))

(defn- url-of [ep path params]
  (str (api-base ep) path "?"
       (str/join "&" (for [[k v] (concat params (flavor-params ep)) :when (some? v)]
                       (str (encode k) "=" (encode v))))))

(defn query-url
  "The URL of an instant query; :time is an Instant or Unix seconds."
  [ep promql {at :time}]
  (url-of ep "/query" [["query" promql] ["time" (if (instance? Instant at) (.getEpochSecond ^Instant at) at)]]))

(defn query-range-url
  "The URL of a range query from :start to :end every :step (a PromQL
   duration or seconds)."
  [ep promql {:keys [start end step]}]
  (let [secs #(if (instance? Instant %) (.getEpochSecond ^Instant %) %)]
    (url-of ep "/query_range" [["query" promql] ["start" (secs start)] ["end" (secs end)] ["step" step]])))

(defn- number [s]
  (case (str s)
    ("+Inf" "Inf") Double/POSITIVE_INFINITY
    "-Inf" Double/NEGATIVE_INFINITY
    (try (Double/parseDouble (str s)) (catch Exception _ nil))))

(defn- instant [t] (when (number? t) (Instant/ofEpochMilli (long (* 1000 (double t))))))

(defn- sample [[t v]] {:at (instant t) :value (number v)})

(defn- labels [metric] (into (sorted-map) (map (fn [[k v]] [(name k) v])) metric))

(defn- series-of [result-type result]
  (case result-type
    "vector" (mapv (fn [{:keys [metric value]}] (merge {:labels (labels metric)} (sample value))) result)
    "matrix" (mapv (fn [{:keys [metric values]}] {:labels (labels metric) :samples (mapv sample values)}) result)
    ("scalar" "string") [(sample result)]
    nil))

(defn parse
  "A fetched response as {:ok true :type :series :warnings} or {:ok false
   :error}. Vector series are {:labels :value :at}, matrix series {:labels
   :samples [{:at :value}]}, a scalar one {:at :value}; label names are
   strings and values doubles, NaN and the infinities included."
  [response]
  (let [{:keys [status data error errorType warnings]} (:json response)]
    (cond
      (not (:reachable response))
      {:ok false :error (str "unreachable: " (:error response))}

      (= "error" status)
      {:ok false :error (str (or errorType "error") ": " error) :http-status (:status response)}

      (not (http/ok? response))
      {:ok false :error (str "HTTP " (:status response)) :http-status (:status response)}

      (not= "success" status)
      {:ok false :error "the response is not a Prometheus API answer"}

      :else
      (if-let [series (series-of (:resultType data) (:result data))]
        (cond-> {:ok true :type (keyword (:resultType data)) :series series}
          (seq warnings) (assoc :warnings (vec warnings)))
        {:ok false :error (str "unknown result type " (pr-str (:resultType data)))}))))

(defn query!
  "Runs an instant query and parses the answer."
  ([ep promql] (query! ep promql {}))
  ([ep promql opts] (parse (http/fetch (query-url ep promql opts) (:headers ep)))))

(defn query-range!
  "Runs a range query and parses the answer."
  [ep promql opts]
  (parse (http/fetch (query-range-url ep promql opts) (:headers ep))))

(defn by-label
  "The values of vector series keyed by one label, nil keys dropped; when two
   series share a key the larger value wins, so a duplicate scrape cannot
   change the answer."
  [series label]
  (reduce (fn [m {:keys [labels value]}]
            (if-let [k (get labels label)]
              (update m k #(if (and % (some? value)) (max % value) (or value %)))
              m))
          {} series))

(defn label-selector
  "A PromQL label selector body from {label value}, each value matched
   exactly and escaped, in label order: {\"job\" \"node\"} is job=\"node\"."
  [matchers]
  (str/join "," (for [[k v] (sort-by key matchers)]
                  (str (name k) "=\"" (str/escape (str v) {\\ "\\\\" \" "\\\"" \newline "\\n"}) "\""))))
