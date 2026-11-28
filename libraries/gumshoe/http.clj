;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.http
  "Probing HTTP APIs. A fetch never throws and never hangs: a connection
   failure, a refused URL, or a body that does not arrive in time becomes an
   unreachable result, so detectives judge a map instead of handling
   exceptions. JSON bodies are parsed opportunistically: :json is nil unless
   the whole body is one complete JSON value.

   request and fetch go through briko.http, so every request follows its
   rules: https, or plain http to a loopback host only (a port-forward on
   127.0.0.1); no user info in the URL; redirects checked hop by hop, never
   from https to http, and with Authorization dropped when one leaves the
   origin. fetch-lines! streams a body line by line, which briko.http does
   not do, and stays on babashka.http-client."
  (:require [babashka.http-client :as http]
            [briko.http :as briko-http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [gumshoe.json :as gumshoe-json]
            [gumshoe.offline :as offline])
  (:import [java.io ByteArrayOutputStream InputStream]
           [java.nio.charset StandardCharsets]))

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

(defn- close-quietly [^java.io.Closeable c]
  (try (.close c) (catch Exception _ nil)))

(defn read-body
  "Reads stream to a UTF-8 string within deadline-ms and max-bytes, then closes
   it. Returns {:body s}, or {:error why} when the body did not end in time,
   grew past max-bytes, or failed. A reader blocked on a server that stopped
   sending is interrupted and the stream closed, so this always returns within
   about deadline-ms."
  [^InputStream stream {:keys [deadline-ms max-bytes]}]
  (let [reading (future
                  (try
                    (let [out (ByteArrayOutputStream.)
                          buffer (byte-array 8192)]
                      (loop []
                        (let [n (.read stream buffer)]
                          (cond
                            (neg? n) {:body (.toString out StandardCharsets/UTF_8)}
                            (> (+ (.size out) n) max-bytes)
                            {:error (format "the body exceeded %d bytes" max-bytes)}
                            :else (do (.write out buffer 0 n) (recur))))))
                    (catch Exception e {:error (message-of e)})))
        result (try (deref reading (max 0 deadline-ms) ::timed-out)
                    (finally
                      ;; also when the caller is interrupted while it waits
                      (when-not (realized? reading) (future-cancel reading))
                      (close-quietly stream)))]
    (if (= ::timed-out result)
      {:error (format "the body did not arrive within %d ms" deadline-ms)}
      result)))

(defn- remaining-ms [started deadline-ms]
  (- deadline-ms (quot (- (System/nanoTime) started) 1000000)))

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

(defn fetch-lines!
  "GETs url and reads the response body as lines until the stream ends, max-lines
   arrive, or deadline-ms passes - for an endpoint that streams indefinitely and
   would never satisfy a buffered read. Returns {:reachable, :status, :lines}
   with whatever arrived before the deadline, {:reachable, :status, :body} for a
   non-2xx (nil :body when the error page did not end within the deadline or
   64 KiB), or {:reachable false, :error}; it never throws, except inside
   gumshoe.offline/forbidding, where it refuses before it connects."
  ([url] (fetch-lines! url {}))
  ([url {:keys [headers deadline-ms max-lines]
         :or {deadline-ms 15000 max-lines 20000}}]
   (offline/check! ["GET" url])
   (let [started (System/nanoTime)]
     (try
       (let [{:keys [status body]} (http/get url {:throw false
                                                 :as :stream
                                                 :timeout deadline-ms
                                                 :headers (merge {"Accept" "application/json"}
                                                                 headers)})]
         (if-not (<= 200 (or status 0) 299)
           {:reachable true
            :status status
            :lines []
            :body (:body (read-body body {:deadline-ms (remaining-ms started deadline-ms)
                                          :max-bytes max-error-body-bytes}))}
           (let [collected (atom [])
                 reading (future
                           (try
                             (with-open [reader (io/reader body)]
                               (doseq [line (line-seq reader)
                                       :while (< (count @collected) max-lines)]
                                 (swap! collected conj line)))
                             (catch Exception _ nil)))]
             (try (deref reading (max 0 (remaining-ms started deadline-ms)) nil)
                  (finally
                    (future-cancel reading)
                    (close-quietly body)))
             {:reachable true :status status :lines @collected})))
       (catch Exception e
         (keep-interrupt! e)
         {:reachable false :error (message-of e)})))))
