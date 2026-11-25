;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.calico.flows
  "Reads Calico flow logs from Whisker's backend API. Whisker serves the React
   app and proxies /whisker-backend/ to the backend, which reads goldmane over
   gRPC - so a flow log is reachable over HTTPS with the credentials guarding
   Whisker itself, without cluster access.

   Two ways to ask. The bounded list returns one window in a single response,
   which is what an investigation wants; on a busy cluster the backend's own
   gRPC call to goldmane exceeds its 4 MiB receive limit and it answers 500, so
   the watch stream the browser UI uses is read instead, for a bounded time.
   Either way the envelope varies - a JSON array, {items: [...]}, or
   newline-delimited objects - and all are tolerated."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [gumshoe.http :as http])
  (:import [java.util Base64]))

(defn basic-auth-header
  "Pure: the Authorization header for user/password, or {} when either is absent."
  [user password]
  (if (and (not (str/blank? (str user))) (not (str/blank? (str password))))
    {"Authorization" (str "Basic " (.encodeToString (Base64/getEncoder)
                                                    (.getBytes (str user ":" password) "UTF-8")))}
    {}))

(defn flows-url
  "Pure: the bounded-window flow query for a Whisker base URL. The API takes the
   window as negative seconds from now."
  [base-url window-seconds]
  (str (str/replace (str base-url) #"/+$" "")
       "/whisker-backend/flows?startTimeGte=-" (max 1 (long window-seconds))))

(defn watch-url
  "Pure: the streaming form of the same query. The backend sends the window's
   backlog and then continues, so the window still bounds what arrives."
  [base-url window-seconds]
  (str (flows-url base-url window-seconds) "&watch=true"))

(defn parse-body
  "Pure: the flow objects in a response body, whatever envelope carries them -
   a JSON array, an {items|flows: [...]} envelope, or newline-delimited objects."
  [body json-body]
  (cond
    (sequential? json-body) (vec json-body)
    (sequential? (:items json-body)) (vec (:items json-body))
    (sequential? (:flows json-body)) (vec (:flows json-body))
    ;; Newline-delimited objects, with or without the server-sent-event `data:`
    ;; prefix the watch mode uses.
    :else (->> (str/split-lines (str body))
               (map #(str/replace % #"^\s*data:\s*" ""))
               (remove str/blank?)
               (keep #(try (json/parse-string % true) (catch Exception _ nil)))
               (filter map?)
               vec)))

(defn- verdict-of
  "Pure: the decisive entry of a policy chain - the last one, which carries the
   action the packet actually got."
  [entries]
  (last (filter map? entries)))

(defn- policy-ref
  "Pure: a readable reference to the policy that triggered a verdict."
  [entry]
  (let [t (:trigger entry)
        kind (or (:kind t) (:kind entry))
        ns-part (not-empty (str (or (:namespace t) (:namespace entry))))
        name-part (not-empty (str (or (:name t) (:name entry))))]
    (not-empty
     (str/join " " (remove nil? [kind
                                 (when (or ns-part name-part)
                                   (str (when ns-part (str ns-part "/")) name-part))])))))

(defn normalise
  "Pure: one backend flow as the flat row the detectives judge."
  [flow]
  (let [enforced (verdict-of (-> flow :policies :enforced))
        pending (verdict-of (-> flow :policies :pending))]
    {:source-namespace (:source_namespace flow)
     :source-name (:source_name flow)
     :dest-namespace (:dest_namespace flow)
     :dest-name (:dest_name flow)
     :protocol (:protocol flow)
     :port (:dest_port flow)
     :action (:action flow)
     :reporter (:reporter flow)
     :packets-in (:packets_in flow)
     :packets-out (:packets_out flow)
     :start-time (:start_time flow)
     :enforced-action (:action enforced)
     :enforced-kind (:kind enforced)
     :enforced-tier (:tier enforced)
     :enforced-policy (policy-ref enforced)
     :pending-action (:action pending)
     :pending-policy (policy-ref pending)}))

(defn- sse-framing?
  "Pure: a server-sent-event line that frames the stream rather than carrying
   data - a comment, an event name, an id or a retry hint."
  [line]
  (boolean (re-find #"^\s*(:|event:|id:|retry:)" line)))

(defn- unparsable-body?
  "Pure: a body that carries content, none of which is JSON - the HTML of
   Whisker's own web app or of an authenticating proxy answering 200 for the
   backend path. Such an answer holds no flow log, and reading it as an empty
   window would pass the flow checks on a window nobody read. A blank body,
   or a stream of nothing but event framing, is an empty window."
  [body json-body]
  (and (nil? json-body)
       (let [content (->> (str/split-lines (str body))
                          (map #(str/replace % #"^\s*data:\s*" ""))
                          (remove str/blank?)
                          (remove sse-framing?))]
         (and (seq content)
              (not-any? #(try (some? (json/parse-string % true)) (catch Exception _ false))
                        content)))))

(defn window-flows
  "Pure: the flows of one window as the rows the detectives judge, or nil when
   the window was not read - the API did not answer, answered other than 2xx,
   or answered 2xx with a body that holds no JSON. nil marks the flow checks
   unread, where an empty list would report a clean window."
  [response body json-body]
  (when (and (http/ok? response) (not (unparsable-body? body json-body)))
    (mapv normalise (parse-body body json-body))))

(defn- list-evidence!
  "One buffered window. Returns the response map plus :flows and :mode."
  [{:keys [url user password window]}]
  (let [target (flows-url url window)
        response (http/fetch target (basic-auth-header user password))]
    {:url target
     :mode :list
     :window window
     :reachable (:reachable response)
     :status (:status response)
     :error (:error response)
     :body (:body response)
     :flows (window-flows response (:body response) (:json response))}))

(defn- stream-evidence!
  "The same window read from the watch stream for at most deadline-ms. A stream
   that delivers nothing is reported as reachable with no flows, which the
   detectives read as a clean window rather than a fault; a stream that does
   not answer 2xx, or delivers only lines that are not JSON, has nil flows."
  [{:keys [url user password window deadline-ms]}]
  (let [target (watch-url url window)
        response (http/fetch-lines! target
                                    {:headers (basic-auth-header user password)
                                     :deadline-ms (or deadline-ms
                                                      (* 1000 (min 20 (max 5 window))))})
        lines (:lines response)]
    {:url target
     :mode :watch
     :window window
     :reachable (:reachable response)
     :status (:status response)
     :error (:error response)
     :body (:body response)
     :flows (window-flows response (str/join "\n" lines) nil)}))

(defn collect-evidence!
  "Fetches one window of flows, buffered where that works and streamed where the
   backend refuses to buffer it. Returns {:url :mode :window :reachable :status
   :flows}, never throws - an unreachable API is evidence a detective reports,
   not an exception the book has to handle."
  [opts]
  (let [opts (update opts :window #(or % 3600))
        listed (list-evidence! opts)]
    (if (or (http/ok? listed) (not= 500 (:status listed)))
      listed
      (let [streamed (stream-evidence! opts)]
        (if (http/ok? streamed)
          streamed
          (assoc listed :fallback-error (or (:error streamed) (:status streamed))))))))
