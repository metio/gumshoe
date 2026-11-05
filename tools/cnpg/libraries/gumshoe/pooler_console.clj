;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pooler-console
  "Reading a Pooler's PgBouncer log: separating the health-check chatter from
   the events, and telling the two look-alike faults apart.

   PgBouncer relays a rejected credential and an unreachable backend in nearly
   the same words, and the fixes are opposite - the first is the credential
   path, the second is the cluster. A rejected credential splits again by leg:
   the pooler can reject a client itself, or the backend can reject the login
   the pooler makes on a client's behalf, and only the second is named in the
   log as a server login."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(def auth-signature "password authentication failed")
(def connect-signature "connect failed")

(def server-login-signature
  "Marks a failure on the pooler-to-backend leg. Every such line also carries
   the credential signature, so this count is a subset of :authentication."
  "server login failed")

(defn noise?
  "The pooler manager reconnects to the admin console over the unix socket
   every 30s and reports two absent metrics each time. Neither says anything
   about clients, and a filter that misses them reads an idle pooler as a busy
   one - or, excluded by the wrong string, hides the events entirely."
  [line]
  (or (str/includes? line "pgbouncer@unix")
      (str/includes? line "Missing metric")))

(defn interesting-lines
  [log]
  (vec (remove noise? (str/split-lines (str log)))))

(def ^:private timestamp-pattern #"\"ts\":\"([^\"]+)\"")

(defn line-instant
  "The timestamp the pooler manager stamps on each record, or nil."
  [line]
  (when-let [[_ ts] (re-find timestamp-pattern (str line))]
    (try (java.time.Instant/parse ts) (catch Exception _ nil))))

(defn- newest
  [instants]
  (when (seq instants) (reduce #(if (.isAfter ^java.time.Instant %2 %1) %2 %1) instants)))

(def stale-after
  "How far behind the end of the window a failure may sit and still describe the
   present. A log tail spans whatever it spans, so a window that reaches back
   into a resolved outage counts those failures exactly as it counts current
   ones."
  (java.time.Duration/ofMinutes 5))

(defn describe-duration
  [^java.time.Duration d]
  (let [days (.toDays d)]
    (cond (pos? days) (format "%dd %dh" days (.toHoursPart d))
          (pos? (.toHours d)) (format "%dh %dm" (.toHours d) (.toMinutesPart d))
          :else (format "%dm" (.toMinutes d)))))

(defn classify
  "What the window contains, and whether it still describes the present. The
   counts answer `what failed in this log`; :stale? answers `is any of it still
   happening`, which is the question an operator reading a verdict is asking."
  [log]
  (let [lines (interesting-lines log)
        auth-lines (filter #(str/includes? % auth-signature) lines)
        connect-lines (filter #(str/includes? % connect-signature) lines)
        server-login-lines (filter #(str/includes? % server-login-signature) lines)
        ;; the end of the window is the newest line of any kind: measured over
        ;; the interesting lines alone, an old failure is its own window end
        window-end (newest (keep line-instant (str/split-lines (str log))))
        latest-failure (newest (keep line-instant (concat auth-lines connect-lines)))
        behind (when (and window-end latest-failure)
                 (java.time.Duration/between latest-failure window-end))]
    {:verdict (cond (and (seq auth-lines) (seq connect-lines)) :both
                    (seq auth-lines) :authentication
                    (seq connect-lines) :unreachable
                    :else :quiet)
     :authentication (count auth-lines)
     :server-login (count server-login-lines)
     :unreachable (count connect-lines)
     :latest-failure latest-failure
     :window-end window-end
     :behind behind
     :stale? (boolean (and behind (pos? (.compareTo behind stale-after))))
     :lines (count lines)}))

(defn- stale-note
  [{:keys [behind authentication unreachable]}]
  (let [total (+ authentication unreachable)]
    (format (str "The %d login failure%s in this window %s historical: the newest sits %s "
                 "before the end of the window, so the log reaches back into something "
                 "already over. Read SHOW SERVERS above for a live backing connection "
                 "before acting on them.")
            total (if (= 1 total) "" "s") (if (= 1 total) "is" "are")
            (describe-duration behind))))

(defn- authentication-note
  "Which leg rejected the login, and what each leg licenses. A roll clears the
   cached login error on either leg, so it returns service without saying
   anything about the cause - on the server leg the credential the pooler
   stores is sound and reusing it after a reconnect is what fails."
  [{:keys [authentication server-login]} pooler]
  (let [roll (format "kubectl -n %s rollout restart deployment/%s"
                     (kubectl/namespace-of pooler) (kubectl/name-of pooler))]
    (if (pos? (or server-login 0))
      (format (str "The backend rejects the login this pooler makes on a client's behalf: "
                   "%d of %d failure%s in this window %s a server login. Compare SHOW USERS - "
                   "a role holding a server connection keeps working while a role needing a "
                   "fresh one fails - and read SHOW VERSION: PgBouncer 1.25.1 and 1.25.2 "
                   "cannot reuse a stored SCRAM secret after a server reconnect, which 1.26 "
                   "fixes. A roll restores logins until the next reconnect, so treat it as "
                   "relief rather than a fix: %s")
              server-login authentication (if (= 1 authentication) "" "s")
              (if (= 1 server-login) "is" "are") roll)
      (format (str "The pooler rejects the client before reaching the backend, so the "
                   "credential the backend holds is not in question. Read SHOW CONFIG for "
                   "auth_type, auth_query and auth_dbname, and confirm the auth_query "
                   "function exists and is executable in the auth_dbname database. A roll "
                   "clears a cached login error and leaves the credential path unchanged: %s")
              roll))))

(defn guidance
  "What the verdict means, and what it does not license. The quiet case
   deliberately refuses to claim health: an idle pooler logs nothing. A window
   whose newest failure is old leads with that, because a count alone reads as
   the present."
  [{:keys [verdict stale?] :as classification} pooler]
  (if stale?
    [(stale-note classification)]
    (case verdict
      :authentication
      [(authentication-note classification pooler)]
      :unreachable
      [(str "The backend cannot be reached, which is not a pooler fault. Check the "
            "cluster's phase and ready instances - rolling the pooler will not fix it.")]
      :both
      [(str "Both signatures are present. Resolve the unreachable backend first: while "
            "the cluster is unwell, a rejected credential cannot be told from a cluster "
            "that is simply absent.")]
      :quiet
      [(str "No login failure in the window read. An idle pooler logs nothing, so this is "
            "not evidence that pooled logins work - check SHOW SERVERS for a backing "
            "connection, or generate one from an application pod.")])))
