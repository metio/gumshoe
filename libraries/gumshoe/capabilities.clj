;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.capabilities
  "What a cluster can do, as opt-in labels - the k8s label/selector idea applied
   to whole clusters. A book declares the capabilities it needs
   (:cluster-capabilities [:ceph]); env.edn labels each cluster with what it
   provides; the prerequisite check verifies the match. Nobody hardcodes cluster
   names.

   Capabilities are also DETECTED: each capability has a detector that probes the
   current cluster for it, and the setup wizard runs them all to fill in
   :capabilities for you. Detection is itself a plugin seam - a plugin ships a
   detector for its own tool with `register-detector!`, so a gumshoe-ceph package
   teaches the wizard to recognise ceph with no change to the core (using
   kubectl/serves-crd?, kubectl/any-matching?, or any other predicate).")

(defonce ^:private detectors (atom {}))

(defn register-detector!
  "Registers how to detect a capability. detect-fn takes no args, probes the
   current cluster, and returns truthy when the capability is present, false
   when the cluster answered that it is absent, and nil (or throws) when the
   cluster did not answer - the three answers kubectl/serves-crd?,
   kubectl/resource-exists? and kubectl/any-matching? give."
  [capability detect-fn]
  (swap! detectors assoc capability detect-fn))

(defn registered
  "Every capability that has a detector, sorted for stable display."
  []
  (sort (keys @detectors)))

(defn answer
  "Pure: :present, :absent or :unanswered for what a detector returned. Only
   false is an absence; nil is a probe that did not answer, so an unreachable
   API server never reads as a missing capability."
  [value]
  (cond
    (false? value) :absent
    (nil? value) :unanswered
    :else :present))

(defn detect-present
  "Runs every registered detector against the current cluster and returns the
   sorted vector of capabilities found. Best-effort: a detector that throws,
   times out or does not answer leaves the capability out, never breaks
   detection."
  []
  (->> @detectors
       ;; Throwable, not Exception: a plugin detector that trips an assert or a
       ;; :pre throws an AssertionError (an Error), and one bad detector must not
       ;; abort the whole sweep - the same reason plugins/load! catches Throwable.
       (keep (fn [[capability detect]]
               (when (try (detect) (catch Throwable _ false)) capability)))
       sort
       vec))

(defn detect-answers
  "Runs every registered detector against the current cluster and sorts the
   capabilities into those found (:present), those not found (:absent) and
   those whose detector threw or returned nil (:unanswered), each a sorted
   vector. Unlike detect-present, a detector that did not answer is no
   evidence of absence, so a health check judging absences reads only
   :absent."
  []
  (let [answers (for [[capability detect] @detectors]
                  [capability (try (answer (detect))
                                   (catch Throwable _ :unanswered))])
        by-answer (group-by second answers)]
    (into {} (for [answer [:present :absent :unanswered]]
               [answer (vec (sort (map first (get by-answer answer))))]))))
