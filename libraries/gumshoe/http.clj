;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.http
  "Probing HTTP APIs. A fetch never throws and never hangs: a connection
   failure, or a body that does not arrive in time, becomes an unreachable
   result, so detectives judge a map instead of handling exceptions. JSON
   bodies are parsed opportunistically."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.java.io :as io])
  (:import [java.io ByteArrayOutputStream InputStream]
           [java.nio.charset StandardCharsets]))

(def default-timeouts
  "How long a fetch waits. :timeout-ms bounds the connection and the response
   headers, :deadline-ms the whole request including the body, and :max-bytes
   the body a fetch keeps in memory. java.net.http stops applying its request
   timeout once the headers arrive, so the body needs a deadline of its own."
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

(defn request
  "Sends an HTTP request and returns {:reachable, :status, :body, :json} - or
   {:reachable false, :error} when the host does not answer at all, or answers
   the headers but not a whole body within the deadline (:status is then kept
   for the record). method is :get, :post, ...; opts takes :headers (merged
   over Accept: application/json), :query-params, and overrides of
   default-timeouts. It never throws."
  [method url opts]
  (let [{:keys [headers query-params timeout-ms deadline-ms max-bytes]} (merge default-timeouts opts)
        started (System/nanoTime)]
    (try
      (let [{:keys [status body]} (http/request (cond-> {:method method
                                                         :uri url
                                                         :throw false
                                                         :as :stream
                                                         :timeout (min timeout-ms deadline-ms)
                                                         :headers (merge {"Accept" "application/json"}
                                                                         headers)}
                                                  query-params (assoc :query-params query-params)))
            read (read-body body {:deadline-ms (remaining-ms started deadline-ms)
                                  :max-bytes max-bytes})]
        (if-let [error (:error read)]
          {:reachable false :status status :error error}
          {:reachable true
           :status status
           :body (:body read)
           :json (try (json/parse-string (:body read) true) (catch Exception _ nil))}))
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
   64 KiB), or {:reachable false, :error}; it never throws."
  ([url] (fetch-lines! url {}))
  ([url {:keys [headers deadline-ms max-lines]
         :or {deadline-ms 15000 max-lines 20000}}]
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
