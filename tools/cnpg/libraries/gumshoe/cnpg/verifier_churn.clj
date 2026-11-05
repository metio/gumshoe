;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.cnpg.verifier-churn
  "Whether something is re-deriving the stored password verifiers of a cluster's
   login roles.

   A pooler caches a role's SCRAM verifier when a client logs in, and a verifier
   re-derived on the server afterwards breaks the pooler's next backend login.
   Re-applying the same password is enough: PostgreSQL picks a fresh salt every
   time, so the stored verifier differs while the password does not.

   One reading cannot show this. The verifier carries no timestamp and the
   server keeps no record of who last set it, so the only evidence is the same
   roles read twice with a wait in between - long enough to cover whatever
   re-applies them, which for a controller means its reconcile interval.

   The comparison is by role, not by count: roles appearing and disappearing
   between the readings is a different fault from a role whose verifier moved,
   and a detective that only counted rows would miss both."
  (:require [clojure.string :as str]))

(def default-window-seconds
  "Longer than a 300s reconcile interval, the common default, so one full cycle
   falls inside the window."
  420)

(defn parse-snapshot
  "Rows of `rolname|md5` into a map of role name to verifier digest. Blank and
   malformed lines are dropped, so a psql notice in the output cannot become a
   role."
  [text]
  (into {}
        (for [line (str/split-lines (or text ""))
              :let [[role digest] (str/split (str/trim line) #"\|")]
              :when (and (not (str/blank? role))
                         (not (str/blank? digest)))]
          [role digest])))

(defn compare-snapshots
  "What changed between two readings of the same cluster: roles whose verifier
   moved, and roles that only exist in one of them."
  [before after]
  (let [shared (filter #(contains? after %) (keys before))]
    {:total (count before)
     :moved (vec (sort (filter #(not= (get before %) (get after %)) shared)))
     :appeared (vec (sort (remove #(contains? before %) (keys after))))
     :vanished (vec (sort (remove #(contains? after %) (keys before))))}))

(defn verdict
  "Reads a comparison. A single moved verifier is enough to report churn: the
   fault needs one re-derivation per pooled session, not a majority of roles."
  [{:keys [total moved appeared vanished]}]
  (cond
    (zero? total) {:verdict :no-roles
                   :summary "no login roles with a password were read"}
    (seq moved) {:verdict :churning
                 :moved (count moved)
                 :total total
                 :summary (format "%d of %d verifier%s re-derived"
                                  (count moved) total
                                  (if (= 1 total) "" "s"))}
    :else {:verdict :stable
           :total total
           :summary (format "all %d verifier%s unchanged" total
                            (if (= 1 total) "" "s"))
           :note (when (or (seq appeared) (seq vanished))
                   (format "%d role(s) appeared and %d vanished between the readings"
                           (count appeared) (count vanished)))}))

(defn guidance
  "What to do about the verdict."
  [{:keys [verdict moved]}]
  (case verdict
    :churning
    [(format "%d verifier(s) moved while nothing changed the passwords" moved)
     (str "transaction-mode poolers reconnect inside a live client session, so they "
          "meet this as `server login failed`; session-mode poolers hold one backend "
          "per session and do not")
     (str "find what re-applies the roles - a controller that writes the password on "
          "every reconcile re-derives the verifier each time, and restarting the "
          "pooler only clears its cache until the next one")]

    :stable
    [(str "no verifier moved in this window, so nothing re-derived them while it was "
          "open - a slower trigger than the window would not show here")]

    :no-roles
    [(str "nothing was read, so this says nothing about churn - check the connection "
          "and that the cluster has login roles with passwords")]

    []))
