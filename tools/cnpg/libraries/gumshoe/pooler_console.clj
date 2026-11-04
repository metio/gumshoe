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

(defn classify
  [log]
  (let [lines (interesting-lines log)
        auth (count (filter #(str/includes? % auth-signature) lines))
        connect (count (filter #(str/includes? % connect-signature) lines))]
    {:verdict (cond (and (pos? auth) (pos? connect)) :both
                    (pos? auth) :authentication
                    (pos? connect) :unreachable
                    :else :quiet)
     :authentication auth
     :unreachable connect
     :lines (count lines)}))

(defn guidance
  "What the verdict means, and what it does not license. The quiet case
   deliberately refuses to claim health: an idle pooler logs nothing."
  [{:keys [verdict]} pooler]
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
          "connection, or generate one from an application pod.")]))
