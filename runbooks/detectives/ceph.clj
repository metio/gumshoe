;; SPDX-FileCopyrightText: The gumshoe Authors
;; SPDX-License-Identifier: 0BSD

(ns runbooks.detectives.ceph
  "Investigates a cephadm-managed ceph cluster over SSH: health checks, OSDs,
   placement groups, monitor quorum, capacity, services, and crashes."
  (:require [gumshoe.ceph :as ceph]
            [gumshoe.detective :as detective]
            [gumshoe.detectives.ceph :as ceph-detectives]
            [gumshoe.runbook :as runbook]
            [gumshoe.ssh :as ssh]
            [gumshoe.stdout :as stdout]))

(def options
  (merge ceph/ssh-options detective/output-option))

(def prerequisites
  {:installed-tools ["ssh"]})

(defn- investigate
  [opts _ctx]
  (detective/when-to-run! "Reach for this when storage is slow or degraded - the ceph cluster's own health, its OSDs, and placement-group state, straight from a mgr host.")
  (let [connection (ceph/connection opts)]
    (stdout/print-section "🔌 Connection")
    (if-not (ssh/check-connection? connection)
      false
      (detective/report!
       ceph-detectives/detectives
       (detective/run-detectives ceph-detectives/detectives
                                 (ceph/collect-evidence! connection))
       (:output opts "text")))))

(runbook/execute!
 {:description "Investigates a cephadm-managed ceph cluster over SSH"
  :options options
  :prerequisites prerequisites
  :announce? false
  :action investigate})
