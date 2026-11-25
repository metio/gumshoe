;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.loopback-server
  "A loopback HTTP server for tests of how a client bounds its waits: it can
   answer the headers and then drip the body, or never answer at all."
  (:require [clojure.string :as str])
  (:import [java.net InetAddress ServerSocket Socket]))

(defn serve!
  "Serves one loopback HTTP response per connection: head (status line and
   headers) at once, then body-chunks with pause-ms between them, holding the
   connection open until the client goes away. Returns {:url :close}."
  [head body-chunks pause-ms]
  (let [server (ServerSocket. 0 50 (InetAddress/getByName "127.0.0.1"))
        url (str "http://127.0.0.1:" (.getLocalPort server) "/")
        loop (future
               (try
                 (while true
                   (let [^Socket socket (.accept server)]
                     (future
                       (try
                         (let [in (.getInputStream socket)
                               out (.getOutputStream socket)]
                           ;; read the request head before answering
                           (loop [tail ""]
                             (let [b (.read in)]
                               (when (and (>= b 0) (not (str/ends-with? tail "\r\n\r")))
                                 (recur (str (subs tail (max 0 (- (count tail) 3))) (char b))))))
                           (.write out (.getBytes ^String head "UTF-8"))
                           (.flush out)
                           (doseq [chunk body-chunks]
                             (when (pos? pause-ms) (Thread/sleep (long pause-ms)))
                             (.write out (.getBytes ^String chunk "UTF-8"))
                             (.flush out))
                           (Thread/sleep 30000))
                         (catch Exception _ nil)
                         (finally (try (.close socket) (catch Exception _ nil)))))))
                 (catch Exception _ nil)))]
    {:url url :port (.getLocalPort server)
     :close (fn [] (.close server) (future-cancel loop))}))

(defn silent!
  "Accepts connections and never answers. Returns {:url :close}."
  []
  (let [server (ServerSocket. 0 50 (InetAddress/getByName "127.0.0.1"))
        held (atom [])
        loop (future
               (try
                 (while true (swap! held conj (.accept server)))
                 (catch Exception _ nil)))]
    {:url (str "http://127.0.0.1:" (.getLocalPort server) "/")
     :port (.getLocalPort server)
     :close (fn []
              (.close server)
              (future-cancel loop)
              (doseq [^Socket s @held] (try (.close s) (catch Exception _ nil))))}))
