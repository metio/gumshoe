;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns gumshoe.detectives.expectations
  "Absence detection for cluster-wide services. env.edn already labels each
   cluster with the capabilities it should provide (:capabilities [:calico :flux
   :ceph ...]), and every capability has a detector that probes the live cluster
   for it. This turns that pair into a health check: a capability the cluster
   *declares* but whose detector cannot *find* is a cluster service that should be
   running and is not - calico down, an operator uninstalled, a migration
   half-done.

   Only capabilities that have a detector are checked: a declared capability with
   no detector is a wiring gap (a plugin missing from :plugins), which the
   casebook's own verify task reports - not an outage worth paging on. Opt-in by
   nature: a cluster that declares no capabilities is not checked.

   The detectors probe the current kubectl context, so the \"capabilities\"
   evidence is collected only when the scan inspects that context, and it
   counts only next to an answered read of the CRDs: an unreachable API
   server, a timed-out read or a user who may not read CRDs would make every
   detector answer \"absent\", and the check is unread then. A detector that
   throws is reported once as unanswered, never as an outage.
   `missing-capabilities` keeps the decision pure and testable."
  (:require [gumshoe.evidence :as evidence]))

(defn missing-capabilities
  "The declared capabilities that have a detector but are not present - the
   cluster services env.edn expects that nothing can find. Pure: `declared` from
   env.edn :capabilities, `registered` the capabilities that have a detector,
   `present` the ones detected live."
  [declared registered present]
  (let [registered (set registered)
        present (set present)]
    (sort (for [capability declared
                :when (contains? registered capability)
                :when (not (contains? present capability))]
            capability))))

(def capability-joins
  "The detectors' answers, and the CRD read that shows the API server
   answered the scan's context and lets it read what most detectors probe."
  ["capabilities" "customresourcedefinitions"])

(defn detect-missing-capabilities
  "One critical per declared capability its detector answered is absent, and
   one warning per declared capability whose detector did not answer.
   Nothing is judged when a joined read did not answer."
  [evidence]
  (when (evidence/joined? evidence capability-joins)
    (let [declared (set (get-in evidence ["config" :capabilities]))
          {:keys [absent unanswered]} (get evidence "capabilities")]
      (concat
       (for [capability (sort (filter declared absent))]
         {:severity :critical
          :component (name capability)
          :summary "declared cluster capability is not present"
          :hint "env.edn :capabilities lists this cluster service, but its detector does not find it - it may be down, uninstalled, or mid-migration"})
       (for [capability (sort (filter declared unanswered))]
         {:severity :warning
          :component (name capability)
          :summary "could not tell whether the declared cluster capability is present"
          :hint "its detector failed instead of answering - check that the API server answers and that you may read what the detector probes"})))))

(def detectives
  [{:name "expected-capabilities"
    :description "Cluster services env.edn declares that no detector can find running"
    :requires capability-joins
    :joins capability-joins
    :detect detect-missing-capabilities}])
