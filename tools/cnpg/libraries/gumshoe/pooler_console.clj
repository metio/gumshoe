;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.pooler-console
  "Reading a Pooler's PgBouncer log: separating the health-check chatter from
   the events, and telling the two look-alike faults apart.

   PgBouncer relays a rejected credential and an unreachable backend in nearly
   the same words, and the fixes are opposite - the first is a pooler left
   behind by an instance rebuild and clears on a restart, the second is the
   cluster and does not."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]))

(def auth-signature "password authentication failed")
(def connect-signature "connect failed")

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
      [(format (str "The backend rejects the credential this pooler presents. A pooler left "
                    "behind by an instance rebuild does this; roll it: "
                    "kubectl -n %s rollout restart deployment/%s")
               (kubectl/namespace-of pooler) (kubectl/name-of pooler))]
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
