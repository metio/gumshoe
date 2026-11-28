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

(defn- read-head
  "The request head of in, up to the blank line, as text."
  [^java.io.InputStream in]
  (loop [sb (StringBuilder.)]
    (if (str/ends-with? (str sb) "\r\n\r\n")
      (str sb)
      (let [b (.read in)]
        (if (neg? b)
          (str sb)
          (recur (.append sb (char b))))))))

(defn- parse-head
  [head]
  (let [[request-line & header-lines] (str/split-lines head)
        [method target] (str/split (str request-line) #" ")]
    {:method method
     :target target
     :headers (into {} (for [line header-lines
                             :let [[k v] (str/split line #":\s*" 2)]
                             :when (seq k)]
                         [(str/lower-case k) v]))}))

(defn recording!
  "Answers every loopback connection with (answer request), a whole HTTP
   response as text, and records each request as {:method :target :headers}
   (header names lower-cased) in the atom :requests. Every response is sent
   with Connection: close semantics: the socket closes after it. Returns
   {:url :port :requests :close}."
  [answer]
  (let [server (ServerSocket. 0 50 (InetAddress/getByName "127.0.0.1"))
        requests (atom [])
        loop (future
               (try
                 (while true
                   (let [^Socket socket (.accept server)]
                     (future
                       (try
                         (let [request (parse-head (read-head (.getInputStream socket)))
                               out (.getOutputStream socket)]
                           (swap! requests conj request)
                           (.write out (.getBytes ^String (answer request) "UTF-8"))
                           (.flush out))
                         (catch Exception _ nil)
                         (finally (try (.close socket) (catch Exception _ nil)))))))
                 (catch Exception _ nil)))]
    {:url (str "http://127.0.0.1:" (.getLocalPort server) "/")
     :port (.getLocalPort server)
     :requests requests
     :close (fn [] (.close server) (future-cancel loop))}))

(defn response
  "A whole HTTP/1.1 response with status line status, extra headers and
   body, closing the connection after it."
  ([status body] (response status {} body))
  ([status headers body]
   (let [bs (.getBytes ^String body "UTF-8")]
     (str "HTTP/1.1 " status "\r\n"
          (apply str (for [[k v] headers] (str k ": " v "\r\n")))
          "Content-Length: " (alength bs) "\r\n"
          "Connection: close\r\n\r\n"
          body))))
