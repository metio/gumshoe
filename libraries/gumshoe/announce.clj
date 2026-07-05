;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.announce
  "Announcing operational changes - the first plugin seam. `announce-via` is a
   multimethod dispatched on an announcer's :type; the built-ins here are :matrix
   and :webhook (Slack/Discord/Mattermost-compatible). A plugin adds more by
   requiring this namespace and defmethod-ing its own type - an IRC or Mastodon
   announcer shipped as a separate git dep, loaded via env.edn :plugins - with no
   change to the core.

   env.edn configures which announcers are active, per environment, as a list -
   so one change can be announced to several places at once:

     :announce [{:type :matrix  :homeserver \"https://synapse.example.org\"
                                :room \"%21room:example.org\" :token-secret \"matrix/bot\"
                                :as \"@ops:example.org\"}
                {:type :webhook :url \"https://hooks.slack.com/services/...\" :format :slack}]

   An announcement never blocks the change it describes: a failing announcer is a
   warning, never an error."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.string :as str]
            [gumshoe.config :as config]
            [gumshoe.secrets :as secrets]
            [gumshoe.stdout :as stdout]))

(defmulti announce-via
  "Posts one announcement through one configured announcer. Dispatches on the
   announcer config's :type. system is what changed (a cluster, a ceph host),
   data is the change context (:actor, :cluster, git info), message is the
   change itself."
  (fn [announcer _system _data _message] (:type announcer)))

(defmethod announce-via :default
  [announcer _system _data _message]
  (stdout/warn (format "unknown announcer type %s - install its plugin or fix :announce in env.edn"
                       (pr-str (:type announcer)))))

(defn register-announcer!
  "Registers an announcer type from data - the same as `(defmethod announce-via
   :type ...)` but callable from a plugin manifest. f is
   (fn [announcer system data message] ...)."
  [type f]
  (.addMethod ^clojure.lang.MultiFn announce-via type f))

;; --- built-in: matrix ------------------------------------------------------

(defn changelog-message
  "Pure rendering of the plain and HTML changelog bodies."
  [system message actor]
  {:plain (format "%s: %s by %s" system message actor)
   :html (format "<code>%s</code>: %s by %s" system message actor)})

(defmethod announce-via :matrix
  [announcer system {:keys [actor]} message]
  (let [homeserver (:homeserver announcer)
        room (:room announcer)
        token (secrets/field (:token-secret announcer "matrix/announcement-bot") "access-token")
        {:keys [plain html]} (changelog-message system message (:as announcer actor))]
    (cond
      (str/blank? (str homeserver)) (stdout/warn "matrix: no :homeserver configured - skipping")
      (str/blank? (str token)) (stdout/warn "matrix: no changelog token - skipping")
      (str/blank? (str room)) (stdout/warn "matrix: no :room configured - skipping")
      :else
      (try
        (let [response (http/post (format "%s/_matrix/client/r0/rooms/%s/send/m.room.message" homeserver room)
                                  {:oauth-token token :throw false :timeout 10000
                                   :body (json/encode {:msgtype "m.text" :body plain
                                                       :format "org.matrix.custom.html" :formatted_body html})})]
          (when-not (= 200 (:status response))
            (stdout/warn "matrix: could not post, status" (:status response))))
        (catch Exception e
          (stdout/warn "matrix: could not reach the changelog room:" (ex-message e)))))))

;; --- built-in: webhook (Slack / Discord / Mattermost incoming webhooks) -----

(defn webhook-payload
  "Pure: the JSON body for an incoming webhook. Slack/Mattermost take {text};
   Discord takes {content}."
  [webhook-format system message actor]
  (let [text (str system ": " message " by " actor)]
    (case webhook-format
      :discord {:content text}
      {:text text})))

(defmethod announce-via :webhook
  [announcer system {:keys [actor]} message]
  (if-let [url (:url announcer)]
    (try
      (http/post url {:headers {"Content-Type" "application/json"}
                      :throw false :timeout 10000
                      :body (json/encode (webhook-payload (:format announcer) system message actor))})
      nil
      (catch Exception e
        (stdout/warn "webhook: could not post:" (ex-message e))))
    (stdout/warn "webhook: no :url configured - skipping")))

;; --- fan-out ---------------------------------------------------------------

(defn announce!
  "Announces a change through every announcer configured for the current
   environment. A failing announcer is warned about, never fatal - during an
   incident the changelog itself may be down."
  [system {:keys [cluster] :as data} message]
  (let [announcers (config/env-value {:kubernetes-cluster cluster} [:announce] [])]
    (if (empty? announcers)
      (stdout/warn "no announcers configured (:announce in env.edn) - skipping the announcement")
      (doseq [announcer announcers]
        (try
          (announce-via announcer system data message)
          (catch Exception e
            (stdout/warn "announcer failed:" (ex-message e))))))))
