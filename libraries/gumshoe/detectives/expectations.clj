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

   Unlike the workload detectives, this one runs the capability detectors live
   rather than reading the shared evidence, because capability presence is a probe
   by definition; `missing-capabilities` keeps the decision pure and testable."
  (:require [gumshoe.capabilities :as capabilities]))

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

(defn detect-missing-capabilities
  [evidence]
  (let [declared (get-in evidence ["config" :capabilities])]
    (for [capability (missing-capabilities declared
                                           (capabilities/registered)
                                           (capabilities/detect-present))]
      {:severity :critical
       :component (name capability)
       :summary "declared cluster capability is not present"
       :hint "env.edn :capabilities lists this cluster service, but its detector does not find it - it may be down, uninstalled, or mid-migration"})))

(def detectives
  [{:name "expected-capabilities"
    :description "Cluster services env.edn declares that no detector can find running"
    :requires []
    :detect detect-missing-capabilities}])
