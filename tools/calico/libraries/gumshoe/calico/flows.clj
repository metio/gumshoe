;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.calico.flows
  "Reads Calico flow logs from Whisker's backend API. Whisker serves the React
   app and proxies /whisker-backend/ to the backend, which reads goldmane over
   gRPC - so a flow log is reachable over HTTPS with the credentials guarding
   Whisker itself, without cluster access.

   The browser UI streams (`?watch=true`); an investigation wants a bounded
   window, so this asks for one and tolerates every envelope the backend might
   answer with: a JSON array, {items: [...]}, or newline-delimited objects."
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

(defn collect-evidence!
  "Fetches one window of flows. Returns {:url :window :reachable :status :flows},
   never throws - an unreachable API is evidence a detective reports, not an
   exception the book has to handle."
  [{:keys [url user password window]}]
  (let [window (or window 3600)
        target (flows-url url window)
        response (http/fetch target (basic-auth-header user password))]
    {:url target
     :window window
     :reachable (:reachable response)
     :status (:status response)
     :error (:error response)
     :body (:body response)
     :flows (if (http/ok? response)
              (mapv normalise (parse-body (:body response) (:json response)))
              [])}))
