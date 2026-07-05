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
     while waiting instead of after."
  (:require [gumshoe.stdout :as stdout]))

(defn eventually
  "Retries check until it returns truthy or timeout (seconds) elapses.
   Exceptions inside check count as not-yet-verified. A malformed post-check is
   refused. Returns the verdict - true when verified, true-with-a-warning when
   a :soft? check times out, false when a hard check times out."
  [{:keys [description check timeout interval soft? watch] :or {timeout 60 interval 5}}]
  (if-not (and (string? description) (fn? check))
    (do (stdout/error "refusing a malformed post-check: it needs a :description string and a :check function")
        false)
    (loop [elapsed 0
           seen #{}]
      (cond
        (try (boolean (check)) (catch Exception _ false))
        (do (stdout/ok (str "verified: " description))
            true)

        (>= elapsed timeout)
        (if soft?
          (do (stdout/warn (format "could not confirm within %ds (best effort): %s" timeout description))
              (stdout/warn "the operation itself reported no error - confirm by hand if in doubt")
              true)
          (do (stdout/error (format "NOT verified within %ds: %s" timeout description))
              false))

        :else
        (do (stdout/err-println (format "  ⏳ waiting for: %s (%d/%ds)" description elapsed timeout))
            (Thread/sleep (* interval 1000))
            (let [fresh (when watch
                          (try (seq (remove seen (watch))) (catch Exception _ nil)))]
              (doseq [signal fresh]
                (stdout/warn (str "while waiting — " signal)))
              (recur (+ elapsed (max interval 1))
                     (into seen fresh))))))))

(defn all
  "Runs every post-check in order; all of them must hold (a :soft? check that
   times out still counts as held)."
  [checks]
  (every? true? (mapv eventually checks)))
