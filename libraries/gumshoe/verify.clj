;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.verify
  "Post-checks: after a change, prove that the intended state actually holds.
   Kubernetes is eventually consistent, so checks are retried until they pass
   or a timeout elapses.

   Two refinements make the wait useful and honest:
   - :soft? checks treat a timeout as a best-effort caveat, not a failure -
     for state that only converges on a pod restart or a background compaction,
     where the operation itself succeeded and only the auto-confirmation could
     not complete in time.
   - :watch turns the dead polling time into live diagnosis: a sampler run
     each interval, whose new signals (a Warning event, a resizer error) are
     surfaced the moment they appear, so the operator learns something is wrong
     while waiting instead of after.

   A check runs under kubectl/*strict-reads*: a read that did not answer
   counts as not-yet-verified, so a check asserting an absence or a negation
   waits for an answer instead of holding on the nil a failed read returns.
   A read that answered NotFound still returns nil, so a deletion check holds.
   The :watch and :abort samplers read lax.

   A post-check fails closed. One that did not get an answer never holds,
   and one about the state of an object never holds on an object that does
   not exist: build it on object-holds?, which is false for nil. Only a
   check whose success is an absence - something deleted, nothing left -
   holds on NotFound, and it says so with :verifies-absence? true.
   fail-closed-findings tests a set of checks against both rules."
  (:require [clojure.string :as str]
            [gumshoe.kubectl :as kubectl]
            [gumshoe.outcome :as outcome]
            [gumshoe.shell :as shell]
            [gumshoe.stdout :as stdout]))

(defn object-holds?
  "Whether object exists and (holds? object) is true. A post-check about the
   state of an object judges it through here, so that an object no read
   found - NotFound, or the nil of a lax read that did not answer - never
   verifies a state."
  [object holds?]
  (boolean (and (some? object) (holds? object))))

(defn attempt
  "One run of check as eventually runs it, with strict reads: true when it
   held, the message of a read that did not answer, or false."
  [check]
  (try (binding [kubectl/*strict-reads* true] (boolean (check)))
       (catch clojure.lang.ExceptionInfo e
         (if (::kubectl/unread (ex-data e)) (ex-message e) false))
       (catch Exception _ false)))

(defn bounded-attempt
  "One attempt of check, as attempt runs it, given at most limit-ms to return:
   its result, or a message saying it did not return in time. A check that
   blocks without a timeout of its own is abandoned there instead of holding
   the post-check, and the book, forever."
  [check limit-ms]
  (let [work (future (attempt check))
        result (try (deref work limit-ms ::timed-out)
                    (finally
                      ;; also when this thread is interrupted while it waits,
                      ;; which reaches only this thread, not the attempt's
                      (when-not (realized? work) (future-cancel work))))]
    (if (= ::timed-out result)
      (format "the check did not return within %ds" (quot limit-ms 1000))
      result)))

(defn- abort-reason
  "The reason abort names, or nil when it names none or throws. Called once
   per attempt, so the reason judged is the reason printed."
  [abort]
  (when abort
    (try (abort) (catch Exception _ nil))))

(defn- print-detail!
  "The text detail names, printed under a timed-out check; nothing when it
   names none or throws."
  [detail]
  (when-let [text (when detail (try (detail) (catch Exception _ nil)))]
    (stdout/err-println (str "  last seen: " text))))

(defn eventually
  "Retries check until it returns truthy or timeout (seconds) elapses on the
   wall clock, counting the time each attempt takes. Exceptions inside check
   count as not-yet-verified, among them a strict read that did not answer.
   Each attempt gets the time left before the deadline, and at least one
   interval, so a check that hangs ends the wait at most one interval late. A
   malformed post-check is refused. Returns the verdict - true when verified,
   true-with-a-warning when a :soft? check times out, false when a hard check
   times out.

   :abort (fn [] -> reason or nil) names a state the check can no longer reach
   from, such as an operation that failed: the wait ends at once with the
   reason, a :soft? check included, instead of running out its timeout.

   :detail (fn [] -> text or nil) says how the state the check judged last
   stood; it is printed when the check times out, so a check that never
   held names what it last saw instead of only what it waited for."
  [{:keys [description check timeout interval soft? watch abort detail] :or {timeout 60 interval 5}}]
  (if-not (and (string? description) (fn? check))
    (do (stdout/error "refusing a malformed post-check: it needs a :description string and a :check function")
        false)
    (let [start (System/currentTimeMillis)
          deadline (+ start (* 1000 timeout))
          interval-ms (* 1000 (max interval 1))]
      (loop [;; Prime the dedup set with a baseline sample, so the pre-existing
             ;; signal backlog (Kubernetes Warning events persist ~1h, a ceph log
             ;; tail) is not reported as if it appeared during this wait. Only
             ;; signals that show up after the change are surfaced.
             seen (if watch (try (set (watch)) (catch Exception _ #{})) #{})]
        (let [result (bounded-attempt check (max interval-ms (- deadline (System/currentTimeMillis))))
              unread (when (string? result) (str " - the last read did not answer: " result))
              reason (when-not (true? result) (abort-reason abort))
              now (System/currentTimeMillis)]
          (cond
            (true? result)
            (do (stdout/ok (str "verified: " description))
                true)

            reason
            (do (stdout/error (format "NOT verified: %s - %s" description reason))
                false)

            (>= now deadline)
            (if soft?
              (do (stdout/warn (format "could not confirm within %ds (best effort): %s%s" timeout description (str unread)))
                  (print-detail! detail)
                  (stdout/warn "the operation itself reported no error - confirm by hand if in doubt")
                  true)
              (do (stdout/error (format "NOT verified within %ds: %s%s" timeout description (str unread)))
                  (print-detail! detail)
                  false))

            :else
            (do (stdout/err-println (format "  ⏳ waiting for: %s (%d/%ds)" description (quot (- now start) 1000) timeout))
                (Thread/sleep (long (min interval-ms (max 0 (- deadline now)))))
                (let [fresh (when watch
                              (try (seq (remove seen (watch))) (catch Exception _ nil)))]
                  (doseq [signal fresh]
                    (stdout/warn (str "while waiting — " signal)))
                  (recur (into seen fresh))))))))))

(defn all
  "Runs every post-check in order; all of them must hold (a :soft? check that
   times out still counts as held)."
  [checks]
  (let [held? (every? true? (mapv eventually checks))]
    (outcome/verified! held?)
    held?))

;; --- failing closed ------------------------------------------------------------------

(def ^:private unanswered
  {:exit 1 :out "" :err "Unable to connect to the server: dial tcp 192.0.2.1:6443: i/o timeout"})

(defn- answered-absent
  "What a cluster holding nothing answers for argv: NotFound for a kubectl get
   of one named object, an empty list for a listing, and no output for a get
   that ignores a missing object or prints names or a jsonpath. Every other
   command fails as an unreachable host does, since only the API server can
   answer that something does not exist."
  [argv]
  (let [positional (remove #(str/starts-with? % "-") (rest (drop-while #(not= "get" %) argv)))]
    (cond
      (not (and (= "kubectl" (first argv)) (some #{"get"} argv))) unanswered
      (some #(or (= "--ignore-not-found" %)
                 (str/starts-with? % "--output=name")
                 (str/starts-with? % "--output=jsonpath"))
            argv)
      {:exit 0 :out "" :err ""}
      (second positional)
      {:exit 1 :out "" :err (format "Error from server (NotFound): %s \"%s\" not found" (first positional) (second positional))}
      :else {:exit 0 :out "{\"apiVersion\":\"v1\",\"items\":[]}" :err ""})))

(defn fail-closed-findings
  "The checks among post-checks that hold where they must not, as
   {:description .. :answer :unanswered|:absent}. Each check runs once, as
   eventually runs it, with gumshoe.shell/execute answering every command:
   once as a cluster that does not answer, where no check may hold, and
   once as a cluster that answers that nothing exists, where only a check
   declaring :verifies-absence? may hold. Every read a post-check makes runs
   a command through gumshoe.shell/execute, so nothing is reached; a check
   that talks to a host by other means is outside what this can judge."
  [post-checks]
  (vec (for [{:keys [description check verifies-absence?]} post-checks
             [answer respond] [[:unanswered (constantly unanswered)] [:absent answered-absent]]
             :when (not (and (= :absent answer) verifies-absence?))
             :when (true? (with-redefs [shell/execute (fn [& argv] (respond (filterv string? argv)))]
                            (attempt check)))]
         {:description description :answer answer})))
