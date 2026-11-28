;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.http
  "Probing HTTP APIs. A fetch never throws and never hangs: a connection
   failure, a refused URL, or a body that does not arrive in time becomes an
   unreachable result, so detectives judge a map instead of handling
   exceptions. JSON bodies are parsed opportunistically: :json is nil unless
   the whole body is one complete JSON value.

   request, fetch and fetch-lines! go through briko.http, so every request
   follows its rules: https, or plain http to a loopback host only (a
   port-forward on 127.0.0.1); no user info in the URL; redirects checked hop
   by hop, never from https to http, and with Authorization dropped when one
   leaves the origin."
  (:require [briko.http :as briko-http]
            [clojure.string :as str]
            [gumshoe.json :as gumshoe-json]
            [gumshoe.offline :as offline]))

(def default-timeouts
  "How long a fetch waits. :timeout-ms bounds opening the connection,
   :deadline-ms the whole request including every redirect and the body, and
   :max-bytes the body a fetch keeps in memory."
  {:timeout-ms 5000
   :deadline-ms 15000
   :max-bytes (* 16 1024 1024)})

(defn- message-of [^Throwable e]
  (or (ex-message e) (.getName (class e))))

(defn- keep-interrupt!
  "Sets the interrupt flag again when e is an interrupt: these functions
   never throw, and a cancelled caller that got an answer instead must still
   refuse its next command."
  [e]
  (when (instance? InterruptedException e)
    (.interrupt (Thread/currentThread))))

(def ^:private max-redirects
  "How many redirects request follows: the five java.net.http's own
   redirect policy allows, each checked by briko.http before it is followed."
  5)

(defn request
  "Sends an HTTP request and returns {:reachable, :status, :body, :json} - or
   {:reachable false, :error} when the host does not answer at all, the URL is
   refused (plain http to a host that is not loopback, user info), or the
   whole answer does not arrive within the deadline or max-bytes. method is
   :get, :post, ...; opts takes :headers (merged over Accept:
   application/json), :query-params, and overrides of default-timeouts:
   :timeout-ms bounds opening the connection, :deadline-ms the whole
   request. It never throws, except inside gumshoe.offline/forbidding, where
   it refuses before it connects."
  [method url opts]
  (offline/check! [(str/upper-case (name method)) url])
  (let [{:keys [headers query-params timeout-ms deadline-ms max-bytes]} (merge default-timeouts opts)]
    (try
      (let [{:keys [status body]} (briko-http/request method url
                                                      (cond-> {:headers (merge {"Accept" "application/json"} headers)
                                                               :as :text
                                                               :connect-timeout-ms (min timeout-ms deadline-ms)
                                                               :timeout-ms deadline-ms
                                                               :max-bytes max-bytes
                                                               :max-redirects max-redirects}
                                                        query-params (assoc :query query-params)))]
        {:reachable true
         :status status
         :body body
         :json (gumshoe-json/parse-or-nil body)})
      (catch Exception e
        (keep-interrupt! e)
        {:reachable false :error (message-of e)}))))

(defn fetch
  "GETs url as request does. Extra headers (e.g. an Authorization bearer) are
   merged over the default Accept: application/json; opts overrides
   default-timeouts."
  ([url] (fetch url {}))
  ([url extra-headers] (fetch url extra-headers {}))
  ([url extra-headers opts]
   (request :get url (assoc opts :headers extra-headers))))

(defn ok?
  "True when the response arrived with a 2xx status."
  [response]
  (boolean (and (:reachable response)
                (:status response)
                (<= 200 (:status response) 299))))

(def ^:private max-error-body-bytes
  "How much of a non-2xx body fetch-lines! keeps: an error page, not a stream."
  (* 64 1024))

(def ^:private max-stream-bytes
  "The most a stream read by fetch-lines! may deliver: 256 MiB, far above
   max-lines of flow records, so the bound only stops a runaway stream. A
   stream past it, like an error page past it, is unreachable."
  (* 256 1024 1024))

(defn fetch-lines!
  "GETs url and reads the response body as lines until the stream ends, max-lines
   arrive, or deadline-ms passes - for an endpoint that streams indefinitely and
   would never satisfy a buffered read. Returns {:reachable, :status, :lines}
   with whatever arrived before the deadline, plus :truncated :deadline or
   :max-lines when the stream was still open; a line still incomplete at the
   deadline is dropped, and a line longer than 1 MiB is cut there. A non-2xx
   answers {:reachable, :status, :lines [], :body}, with a nil :body for an
   error page over 64 KiB. A URL briko.http refuses, a connection that fails
   (also in the middle of the stream), an error page that does not end
   within the deadline or a stream over 256 MiB answer {:reachable false,
   :error}. It never throws, except inside gumshoe.offline/forbidding, where
   it refuses before it connects."
  ([url] (fetch-lines! url {}))
  ([url {:keys [headers deadline-ms max-lines]
         :or {deadline-ms 15000 max-lines 20000}}]
   (offline/check! ["GET" url])
   (try
     (let [{:keys [status body truncated]}
           (briko-http/request :get url {:headers (merge {"Accept" "application/json"} headers)
                                         :as :lines
                                         :max-lines max-lines
                                         :on-long-line :truncate
                                         :connect-timeout-ms deadline-ms
                                         :timeout-ms deadline-ms
                                         :max-bytes max-stream-bytes
                                         :max-redirects max-redirects})]
       (if (<= 200 status 299)
         (cond-> {:reachable true :status status :lines body}
           truncated (assoc :truncated truncated))
         {:reachable true
          :status status
          :lines []
          :body (when (<= (count (.getBytes ^String body "UTF-8")) max-error-body-bytes) body)}))
     (catch Exception e
       (keep-interrupt! e)
       {:reachable false :error (message-of e)}))))
