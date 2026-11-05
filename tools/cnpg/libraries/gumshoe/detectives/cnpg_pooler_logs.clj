;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.cnpg-pooler-logs
  "A detective for pooled logins that are being rejected.

   Every other pooler detective reads objects, and this fault leaves no trace
   in them: the pods are ready and the Pooler is active while the backend
   refuses the login the pooler makes for each client. Only the log shows it,
   so the log is the evidence this detective asks for.

   The window is a tail, so it can reach back into something already over. A
   failure that is not the newest thing in its window is reported as historical
   rather than current, and the two never carry the same severity.

   An unreachable backend is left to the cluster detectives. A pooler whose
   cluster is unwell reports the cluster's fault, and naming it twice buries the
   one finding that is the pooler's own."
  (:require [gumshoe.cnpg.pooler-logs :as pooler-logs]
            [gumshoe.pooler-console :as console]))

(defn- component [{:keys [namespace pod]}] (format "%s/%s" namespace pod))

(defn- credential-verdict? [verdict] (contains? #{:authentication :both} verdict))

(defn- leg-of
  [{:keys [server-login]}]
  (if (pos? (or server-login 0)) "server" "client"))

(defn- leg-hint
  [leg]
  (if (= "server" leg)
    (str "the pooler holds the role's SCRAM verifier from the moment each client "
         "logged in, and a verifier re-derived on the server - the same password "
         "applied again is enough, since every apply picks a fresh salt - leaves "
         "the pooler authenticating with the old one. Transaction-mode poolers "
         "reconnect inside a live client session and so meet it; session-mode "
         "poolers hold one backend for the session and do not. Compare "
         "`md5(rolpassword)` from `pg_authid` across a few minutes to see whether "
         "something is re-applying the password, and check what manages the role "
         "before restarting the pooler, which only clears the cache until the next "
         "re-derivation")
    (str "clients that reach this pooler are not getting a backend; the "
         "pooler console book reads the admin console and says which leg "
         "rejected the login and what that leg licenses")))

(defn detect-rejected-logins
  "Pooler pods whose log shows logins being rejected, and pods whose log could
   not be read."
  [evidence]
  (let [rows (get evidence pooler-logs/evidence-key)]
    (concat
     (for [row rows
           :when (:read? row)
           :let [verdict (console/classify (:log row))]
           :when (credential-verdict? (:verdict verdict))]
       (if (:stale? verdict)
         {:severity :warning
          :component (component row)
          :summary (format "%d rejected login%s in the log, none of them current"
                           (:authentication verdict)
                           (if (= 1 (:authentication verdict)) "" "s"))
          :hint (str "the newest failure sits behind the end of the window, so the log "
                     "reaches back into something already over - confirm against a live "
                     "server connection before acting on it")}
         {:severity :critical
          :component (component row)
          :summary (format "%d rejected login%s in the log, on the %s leg"
                           (:authentication verdict)
                           (if (= 1 (:authentication verdict)) "" "s")
                           (leg-of verdict))
          :hint (leg-hint (leg-of verdict))}))
     (for [row rows
           :when (not (:read? row))]
       {:severity :warning
        :component (component row)
        :summary "pooler log unread - kubectl logs failed for this pod"
        :hint (str "a log that could not be read is not a log without failures, so this "
                   "pod is unchecked rather than healthy")}))))

(def detectives
  [{:name "cnpg-pooler-logins"
    :description "Poolers whose backend is rejecting pooled logins"
    :requires [pooler-logs/evidence-key]
    :detect detect-rejected-logins}])
