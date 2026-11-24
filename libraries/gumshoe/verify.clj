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
   The :watch and :abort samplers read lax."
  (:require [gumshoe.kubectl :as kubectl]
            [gumshoe.outcome :as outcome]
            [gumshoe.stdout :as stdout]))

(defn- attempt
  "One run of check with strict reads: true when it held, the message of a
   read that did not answer, or false."
  [check]
  (try (binding [kubectl/*strict-reads* true] (boolean (check)))
       (catch clojure.lang.ExceptionInfo e
         (if (::kubectl/unread (ex-data e)) (ex-message e) false))
       (catch Exception _ false)))

(defn eventually
  "Retries check until it returns truthy or timeout (seconds) elapses.
   Exceptions inside check count as not-yet-verified, among them a strict
   read that did not answer. A malformed post-check is
   refused. Returns the verdict - true when verified, true-with-a-warning when
   a :soft? check times out, false when a hard check times out.

   :abort (fn [] -> reason or nil) names a state the check can no longer reach
   from, such as an operation that failed: the wait ends at once with the
   reason, a :soft? check included, instead of running out its timeout."
  [{:keys [description check timeout interval soft? watch abort] :or {timeout 60 interval 5}}]
  (if-not (and (string? description) (fn? check))
    (do (stdout/error "refusing a malformed post-check: it needs a :description string and a :check function")
        false)
    (loop [elapsed 0
           ;; Prime the dedup set with a baseline sample, so the pre-existing
           ;; signal backlog (Kubernetes Warning events persist ~1h, a ceph log
           ;; tail) is not reported as if it appeared during this wait. Only
           ;; signals that show up after the change are surfaced.
           seen (if watch (try (set (watch)) (catch Exception _ #{})) #{})]
      (let [result (attempt check)
            unread (when (string? result) (str " - the last read did not answer: " result))]
        (cond
          (true? result)
          (do (stdout/ok (str "verified: " description))
              true)

          (when abort (try (abort) (catch Exception _ nil)))
          (do (stdout/error (format "NOT verified: %s - %s" description (abort)))
              false)

          (>= elapsed timeout)
          (if soft?
            (do (stdout/warn (format "could not confirm within %ds (best effort): %s%s" timeout description (str unread)))
                (stdout/warn "the operation itself reported no error - confirm by hand if in doubt")
                true)
            (do (stdout/error (format "NOT verified within %ds: %s%s" timeout description (str unread)))
                false))

          :else
          (do (stdout/err-println (format "  ⏳ waiting for: %s (%d/%ds)" description elapsed timeout))
              (Thread/sleep (* interval 1000))
              (let [fresh (when watch
                            (try (seq (remove seen (watch))) (catch Exception _ nil)))]
                (doseq [signal fresh]
                  (stdout/warn (str "while waiting — " signal)))
                (recur (+ elapsed (max interval 1))
                       (into seen fresh)))))))))

(defn all
  "Runs every post-check in order; all of them must hold (a :soft? check that
   times out still counts as held)."
  [checks]
  (let [held? (every? true? (mapv eventually checks))]
    (outcome/verified! held?)
    held?))
