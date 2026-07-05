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
   teaches the wizard to recognise ceph with no change to the core."
  (:require [gumshoe.kubectl :as kubectl]))

(defonce ^:private detectors (atom {}))

(defn register-detector!
  "Registers how to detect a capability. detect-fn takes no args, probes the
   current cluster, and returns truthy when the capability is present."
  [capability detect-fn]
  (swap! detectors assoc capability detect-fn))

(defn registered
  "Every capability that has a detector, sorted for stable display."
  []
  (sort (keys @detectors)))

(defn detect-present
  "Runs every registered detector against the current cluster and returns the
   sorted vector of capabilities found. Best-effort: a detector that throws or
   times out counts the capability as absent, never breaks detection."
  []
  (->> @detectors
       (keep (fn [[capability detect]]
               (when (try (detect) (catch Exception _ false)) capability)))
       sort
       vec))

;; --- built-in detectors: presence of a well-known CRD marks the capability ---
;; A cluster running flux serves the flux CRDs, cert-manager serves Certificate,
;; and so on. This is stable across installation methods (helm, operator, raw),
;; because it keys on the API the tool serves, not on how it was deployed.

(defn- serves-crd?
  [crd]
  (kubectl/resource-exists? "customresourcedefinition" crd))

(register-detector! :calico #(serves-crd? "installations.operator.tigera.io"))
