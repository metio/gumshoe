;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.http
  "Probing HTTP APIs. A fetch never throws: a connection failure becomes an
   unreachable result, so detectives judge a map instead of handling
   exceptions. JSON bodies are parsed opportunistically."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.java.io :as io]))

(defn fetch
  "GETs url and returns {:reachable, :status, :body, :json} - or
   {:reachable false, :error} when the host does not answer at all. Extra
   headers (e.g. an Authorization bearer) may be supplied and are merged over
   the default Accept: application/json."
  ([url] (fetch url {}))
  ([url extra-headers]
   (try
     (let [{:keys [status body]} (http/get url {:throw false
                                                :timeout 5000
                                                :headers (merge {"Accept" "application/json"}
                                                                extra-headers)})]
       {:reachable true
        :status status
        :body body
        :json (try (json/parse-string body true) (catch Exception _ nil))})
     (catch Exception e
       {:reachable false :error (or (ex-message e) (.getName (class e)))}))))

(defn ok?
  "True when the response arrived with a 2xx status."
  [response]
  (boolean (and (:reachable response)
                (:status response)
                (<= 200 (:status response) 299))))

(defn fetch-lines!
  "GETs url and reads the response body as lines until the stream ends, max-lines
   arrive, or deadline-ms passes - for an endpoint that streams indefinitely and
   would never satisfy a buffered read. Returns {:reachable, :status, :lines}
   with whatever arrived before the deadline, {:reachable, :status, :body} for a
   non-2xx, or {:reachable false, :error}; it never throws."
  ([url] (fetch-lines! url {}))
  ([url {:keys [headers deadline-ms max-lines]
         :or {deadline-ms 15000 max-lines 20000}}]
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
          :body (try (slurp body) (catch Exception _ nil))}
         (let [collected (atom [])
               reading (future
                         (try
                           (with-open [reader (io/reader body)]
                             (doseq [line (line-seq reader)
                                     :while (< (count @collected) max-lines)]
                               (swap! collected conj line)))
                           (catch Exception _ nil)))]
           (deref reading deadline-ms nil)
           (future-cancel reading)
           (try (.close body) (catch Exception _ nil))
           {:reachable true :status status :lines @collected})))
     (catch Exception e
       {:reachable false :error (or (ex-message e) (.getName (class e)))}))))
