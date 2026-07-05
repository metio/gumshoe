;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.http
  "Probing HTTP APIs. A fetch never throws: a connection failure becomes an
   unreachable result, so detectives judge a map instead of handling
   exceptions. JSON bodies are parsed opportunistically."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]))

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
